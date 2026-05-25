;;; init-gptel.el --- GPTel configuration (extracted from emacs_asahi)  -*- lexical-binding: t; -*-
; Created on: Sat 13 Sep 2025
; Last Updated: Mon 25 May 2026

;;; GPTel specific configurations ;;;
;;
(require 'gptel)
(require 'json)

(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function gptel--display-tool-calls "gptel" (tool-calls info &optional use-minibuffer))
(declare-function magit-git-insert "magit-git" (&rest args))
(declare-function projectile-project-files "projectile" (&optional project-root))
(declare-function projectile-project-root "projectile" (&optional dir))

(setq gptel-default-mode 'org-mode)

;; Configure Anthropic Claude
;; API key will be read from '~/.authinfo.gpg'
(gptel-make-anthropic "Claude"
  :stream t
  :key gptel-api-key
  :models '(claude-opus-4-7
            claude-sonnet-4-6))

;; Configure Google Gemini
;; API key will be read from '~/.authinfo.gpg'
(gptel-make-gemini "Gemini"
  :stream t
  :key gptel-api-key
  :models '(gemini-3.1-pro-preview
            gemini-3.5-flash))

;; Configure OpenAI subscription (OAuth)
(gptel-make-openai-oauth "OpenAI-pro"
  :models '(gpt-5.3-codex
            gpt-5.5))

;; Configure Deepseek
;; API key will be read from '~/.authinfo.gpg'
(gptel-make-deepseek "DeepSeek"
  :stream t
  :key gptel-api-key
  :models '(deepseek-v4-pro
            deepseek-v4-flash))

;; Configure Github Copilot Chat
;; no API key; browser auth login required
(gptel-make-gh-copilot "Copilot")

;; Configure xAI Grok
;; API key will be read from '~/.authinfo.gpg'
(gptel-make-xai "xAI"
  :stream t
  :key gptel-api-key
  :models '(grok-4.3
            grok-4-1-fast))

;; Configure Alibaba Qwen3
;; API key will be read from '~/.authinfo.gpg'
(gptel-make-openai "Alibaba"
  :stream t
  :key gptel-api-key
  :protocol "https"
  :host "dashscope-intl.aliyuncs.com"
  :endpoint "/compatible-mode/v1/chat/completions"
  :models '(qwen3.6-plus))

;; OpenRouter offers an OpenAI compatible API
(gptel-make-openai "OpenRouter"
  :host "openrouter.ai"
  :endpoint "/api/v1/chat/completions"
  :stream t
  :key gptel-api-key
  :models '(openrouter/auto
            openrouter/free
            anthropic/claude-opus-4.7
            google/gemini-3.1-pro-preview
            minimax/minimax-m2.7
            moonshotai/kimi-k2.6
            openai/gpt-5.3-codex
            openai/gpt-5.5-pro
            x-ai/grok-4.20-beta
            xiaomi/mimo-v2.5-pro
            z-ai/glm-5.1))


;; Helper functions for gptel custom tools

(defconst my/gptel--high-risk-buffer-max-chars 200000
  "Maximum buffer size that read/context tools may inspect by default.
Larger buffers are refused to avoid accidentally replaying huge tool,
diagnostic, process, or chat transcripts into later gptel requests.")

(defconst my/gptel--high-risk-buffer-name-regexp
  (rx string-start "*"
      (or "gptel" "gptel-diagnostic" "Compile-Log" "Async Shell Command"
          "Shell Command Output" "Messages")
      (* not-newline) "*" string-end)
  "Internal or diagnostic buffer names that read/context tools refuse.")

(defun my/gptel--high-risk-buffer-reason ()
  "Return a reason current buffer is unsafe for read/context tools, or nil."
  (cond
   ((string-match-p my/gptel--high-risk-buffer-name-regexp (buffer-name))
    "it looks like an internal diagnostic, transcript, or log buffer")
   ((bound-and-true-p gptel-mode)
    "it is a gptel chat/transcript buffer")
   ((get-buffer-process (current-buffer))
    "it is attached to a process that may contain unbounded output")
   ((> (buffer-size) my/gptel--high-risk-buffer-max-chars)
    (format "it is very large (%d chars; limit %d)"
            (buffer-size) my/gptel--high-risk-buffer-max-chars))))

(defun my/gptel--ensure-readable-buffer (operation)
  "Signal an error if current buffer is unsafe for OPERATION.
The refusal protects gptel sessions from token runaway caused by reading
recursive diagnostics, chat transcripts, process buffers, or huge buffers."
  (when-let ((reason (my/gptel--high-risk-buffer-reason)))
    (error "%s refused for buffer '%s': %s. Reading this buffer could paste recursive tool schemas, chat history, diagnostics, or unbounded process output into the transcript and cause token runaway. Narrow the request to a normal file buffer, use a smaller file range, or inspect the buffer manually."
           operation (buffer-name) reason)))

(defmacro my/gptel--with-buffer-safety (buffer-expr error-label &rest body)
  "Execute BODY with current-buffer set to BUFFER-EXPR.
On error, return a formatted error message using ERROR-LABEL."
  (declare (indent 2))
  `(condition-case err
       (with-current-buffer ,buffer-expr
         ,@body)
     (error (format "Error %s: %s" ,error-label (error-message-string err)))))

(defun my/gptel--resolve-buffer (name-or-path)
  "Return a live buffer matching NAME-OR-PATH (buffer name or file path).
Signals an error if no matching buffer is found."
  (or (get-buffer name-or-path)
      (find-buffer-visiting (expand-file-name name-or-path))
      (error "No buffer for '%s'.  Use open_file to open it first." name-or-path)))

(defun my/gptel--render-hashed-lines (label start-line end-line)
  "Render the current buffer with line numbers + 3-char content hash.
  Each line is formatted as \"N:HHH|text\" (HHH = first 3 hex chars of
  MD5 of trimmed line content).  This enables hash-anchored editing and
  prevents phantom edits from line shifts or whitespace differences.
  LABEL, START-LINE, END-LINE behave as before. Default window: 2000 lines."
  (save-excursion
    (let* ((total (count-lines (point-min) (point-max)))
           (start-line (and start-line (round start-line)))
           (end-line (and end-line (round end-line)))
           (start (max 1 (or start-line 1)))
           (default-end (+ start 1999))
           (requested-end (or end-line default-end))
           (end (min total requested-end)))
      (cond
       ((zerop total)
        (format "%s: file is empty (0 lines)" label))
       ((> start total)
        (format "%s: start-line %d is past end of file (%d line%s)"
                label start total (if (= total 1) "" "s")))
       ((< requested-end start)
        (format "Error: end-line (%d) is less than start-line (%d)"
                requested-end start))
       (t
        (goto-char (point-min))
        (forward-line (1- start))
        (let ((lines '())
              (n start))
          (while (and (<= n end) (not (eobp)))
            (let* ((line-text (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))
                   (h (my/gptel--hash-line line-text)))
              (push (format "%d:%s|%s" n h line-text) lines))
            (forward-line 1)
            (setq n (1+ n)))
          (format "%s (lines %d-%d of %d):\n%s"
                  label start end total
                  (mapconcat 'identity (nreverse lines) "\n"))))))))

(defconst my/gptel--hashline-displacement-window 20
  "Number of nearby lines to scan when a hashline tag has shifted.")

(defun my/gptel--hash-line (line)
  "Return the 3-character hash for LINE used in hashline tool output."
  (substring (md5 (string-trim line)) 0 3))

(defun my/gptel--current-line-hash ()
  "Return the hashline hash for the current line."
  (my/gptel--hash-line
   (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun my/gptel--parse-hashline-tag (tag label)
  "Parse TAG of the form N:HHH for LABEL and return (LINE . HASH)."
  (unless (and (stringp tag)
               (string-match-p (rx string-start (+ digit) ":" (+ hex-digit) string-end) tag))
    (error "%s must be a line tag like '42:abc', copied from read_file output" label))
  (let ((parts (split-string tag ":")))
    (cons (string-to-number (car parts)) (downcase (cadr parts)))))

(defun my/gptel--goto-line-checked (line total)
  "Move point to LINE in current buffer, validating against TOTAL lines."
  (when (or (< line 1) (> line total))
    (error "Line %d is out of range (1-%d)" line total))
  (goto-char (point-min))
  (forward-line (1- line)))

(defun my/gptel--resolve-hashline-tag (tag label)
  "Resolve TAG for LABEL to (LINE DISPLACED-P), or signal with context."
  (let* ((parsed (my/gptel--parse-hashline-tag tag label))
         (target-line (car parsed))
         (target-hash (cdr parsed))
         (total (count-lines (point-min) (point-max)))
         (scan-start (max 1 (- target-line my/gptel--hashline-displacement-window)))
         (scan-end (min total (+ target-line my/gptel--hashline-displacement-window)))
         (matches nil))
    (my/gptel--goto-line-checked target-line total)
    (if (string= (my/gptel--current-line-hash) target-hash)
        (list target-line nil)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- scan-start))
        (let ((line scan-start))
          (while (and (<= line scan-end) (not (eobp)))
            (when (string= (my/gptel--current-line-hash) target-hash)
              (push line matches))
            (forward-line 1)
            (setq line (1+ line)))))
      (setq matches (nreverse matches))
      (cond
       ((= (length matches) 1)
        (list (car matches) t))
       ((> (length matches) 1)
        (error "%s tag %s no longer matches line %d, and hash %s appears multiple times nearby at lines %s. Re-read a narrower range before editing."
               label tag target-line target-hash (mapconcat #'number-to-string matches ", ")))
       (t
        (error "%s tag %s no longer matches line %d, and hash %s was not found within +/- %d lines. Re-read and retry with fresh tags.\n\n%s"
               label tag target-line target-hash my/gptel--hashline-displacement-window
               (my/gptel--render-hashed-lines
                (format "Current context around stale tag %s" tag)
                scan-start scan-end)))))))

(defun my/gptel--buffer-edit-string (buffer old-str new-str replace-all)
  "Replace OLD-STR with NEW-STR in current buffer (named BUFFER).
If OLD-STR matches more than once and REPLACE-ALL is nil, return an
error string instead of replacing.  Returns a result string."
  (let ((case-fold-search nil)
        (count 0))
    (cond
     ((string-empty-p old-str)
      "Error: old_str must not be empty")
     (t
      (save-excursion
        (goto-char (point-min))
        (while (search-forward old-str nil t)
          (setq count (1+ count))))
      (cond
       ((zerop count)
        (format "Error: old_str not found in buffer '%s'" buffer))
       ((and (> count 1) (not replace-all))
        (format "Error: old_str matches %d times in buffer '%s'; pass replace_all=true to replace all, or extend old_str with surrounding context to make it unique."
                count buffer))
       (t
        (save-excursion
          (goto-char (point-min))
          (while (search-forward old-str nil t)
            (replace-match new-str t t)))
        (format "Replaced %d occurrence%s of old_str in buffer '%s'"
                count (if (= count 1) "" "s") buffer)))))))

(defun my/gptel--buffer-edit-hashline (buffer start-line end-line new-str)
  "Replace hash-anchored START-LINE..END-LINE in BUFFER with NEW-STR.
START-LINE and END-LINE are read_file tags like \"42:abc\".  If a tag has
shifted, scan a small nearby window for the same hash before editing."
  (let* ((start-result (my/gptel--resolve-hashline-tag start-line "start_line"))
         (end-tag (if (and end-line (not (string-empty-p end-line)))
                      end-line
                    start-line))
         (end-result (my/gptel--resolve-hashline-tag end-tag "end_line"))
         (resolved-start (car start-result))
         (resolved-end (car end-result))
         (displaced (or (cadr start-result) (cadr end-result))))
    (cond
     ((> resolved-start resolved-end)
      (format "Error: Resolved start_line %s to line %d, after end_line %s at line %d. Re-read and retry with fresh tags."
              start-line resolved-start end-tag resolved-end))
     (t
      (goto-char (point-min))
      (forward-line (1- resolved-start))
      (let ((start-pos (point)))
        (forward-line (- resolved-end resolved-start))
        (end-of-line)
        (delete-region start-pos (point))
        (insert (or new-str "")))
      (format "Hashline edit replaced lines %d-%d in buffer '%s'%s"
              resolved-start resolved-end buffer
              (if displaced " after nearby hash recovery" ""))))))

(defun my/gptel--buffer-insert (buffer text start-line position)
  "Insert TEXT verbatim in BUFFER at START-LINE.
POSITION should be \"before\" or \"after\"; defaults to \"before\".
TEXT is inserted exactly as provided -- no indentation adjustment.
Use the indent_region tool afterwards if reformatting is needed.
Assumes current buffer is the target buffer.  Returns a result string."
  (let* ((start-line (and start-line (round start-line)))
         (total-lines (count-lines (point-min) (point-max))))
    (cond
     ((not start-line)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert text)
      (format "Inserted text at end of buffer %s" buffer))
     ((or (< start-line 1) (> start-line (1+ total-lines)))
      (format "Error: Line %d is out of range (1-%d) in buffer %s"
              start-line (1+ total-lines) buffer))
     (t
      (goto-char (point-min))
      (forward-line (1- start-line))
      (if (string= position "after")
          (progn (end-of-line) (insert "\n" text))
        (progn (beginning-of-line) (insert text "\n")))
      (format "Inserted text %s line %d in buffer %s"
              (or position "before")
              start-line
              buffer)))))

(defun my/gptel--buffer-replace (buffer text start-line end-line)
  "Replace lines START-LINE to END-LINE in BUFFER with TEXT.
Assumes current buffer is the target buffer.  Returns a result string."
  (let* ((start-line (round start-line))
         (end-line (round end-line))
         (total-lines (count-lines (point-min) (point-max))))
    (cond
     ((or (< start-line 1) (> end-line total-lines) (> start-line end-line))
      (format "Error: Invalid line range %d-%d (total lines: %d)"
              start-line end-line total-lines))
     (t
      (goto-char (point-min))
      (forward-line (1- start-line))
      (let ((start-pos (point)))
        (forward-line (- end-line start-line))
        (end-of-line)
        (delete-region start-pos (point))
        (insert text))
      (format "Replaced lines %d-%d in buffer %s" start-line end-line buffer)))))

(defun my/gptel--buffer-delete (buffer start-line end-line)
  "Delete lines START-LINE to END-LINE in BUFFER.
Assumes current buffer is the target buffer.  Returns a result string."
  (let* ((start-line (round start-line))
         (end-line (round end-line))
         (total-lines (count-lines (point-min) (point-max))))
    (cond
     ((or (< start-line 1) (> end-line total-lines) (> start-line end-line))
      (format "Error: Invalid line range %d-%d (total lines: %d)"
              start-line end-line total-lines))
     (t
      (goto-char (point-min))
      (forward-line (1- start-line))
      (let ((start-pos (point)))
        (forward-line (- end-line start-line))
        (end-of-line)
        (when (< (point) (point-max)) (forward-char 1))
        (delete-region start-pos (point)))
      (format "Deleted lines %d-%d in buffer %s" start-line end-line buffer)))))

(defcustom my/gptel--tool-result-max-chars 20000
  "Fallback maximum characters returned by a single gptel custom tool result."
  :type 'integer
  :group 'gptel)

(defcustom my/gptel--tool-result-max-chars-by-tool
  '(("read_file" . 40000)
    ("show_buffer_context" . 12000)
    ("search_buffer_text" . 12000)
    ("search_project" . 16000)
    ("list_project_files" . 12000)
    ("list_buffers" . 10000)
    ("run_shell_command" . 12000)
    ("git_status" . 10000)
    ("git_diff" . 12000)
    ("buffer_diagnostics" . 12000)
    ("delegate_agent" . 20000))
  "Tool-specific body caps for high-volume gptel custom tool results.
Tools not listed here use `my/gptel--tool-result-max-chars'.  Deliberate
file reads get a larger cap because they are paginated and hash-anchored;
shell output, diffs, diagnostics, and arbitrary buffer views use stricter
caps because they are more likely to contain runaway or recursive output."
  :type '(alist :key-type string :value-type integer)
  :group 'gptel)

(defun my/gptel--tool-result-cap (tool &optional max-chars)
  "Return the result cap for TOOL, honoring explicit MAX-CHARS first."
  (or max-chars
      (cdr (assoc-string tool my/gptel--tool-result-max-chars-by-tool t))
      my/gptel--tool-result-max-chars))

(defcustom my/gptel--tool-output-store-directory
  (expand-file-name "gptel/tool-outputs/"
                    (or (getenv "XDG_STATE_HOME")
                        (expand-file-name "~/.local/state")))
  "Local side-car directory for oversized gptel custom tool outputs.
Objects are addressed by sha256 and may contain sensitive source code, logs,
diagnostics, command output, or local paths.  Keep this outside project repos
and org chat files."
  :type 'directory
  :group 'gptel)

(defun my/gptel--tool-output-store-path (hash suffix)
  "Return side-car store path for object HASH with file SUFFIX."
  (expand-file-name (concat hash suffix)
                    (expand-file-name (substring hash 0 2)
                                      my/gptel--tool-output-store-directory)))

(defun my/gptel--tool-output-metadata-json (tool metadata hash body)
  "Return JSON manifest text for stored TOOL output BODY with HASH."
  (let ((record `((id . ,(concat "sha256:" hash))
                  (tool . ,tool)
                  (stored_at . ,(format-time-string "%FT%TZ" (current-time) t))
                  (bytes . ,(string-bytes body))
                  (chars . ,(length body))
                  (lines . ,(with-temp-buffer
                              (insert body)
                              (count-lines (point-min) (point-max))))
                  (content_type . "text/plain; charset=utf-8")
                  (scope . ,(mapcar
                             (lambda (entry)
                               (cons (format "%s" (car entry))
                                     (my/gptel--format-metadata-value (cdr entry))))
                             metadata)))))
    (concat (json-encode record) "\n")))

(defun my/gptel--store-tool-output (tool metadata body)
  "Store oversized TOOL output BODY and return reference metadata."
  (let* ((hash (secure-hash 'sha256 body))
         (object-path (my/gptel--tool-output-store-path hash ".txt"))
         (manifest-path (my/gptel--tool-output-store-path hash ".json"))
         (dir (file-name-directory object-path))
         (lines (with-temp-buffer
                  (insert body)
                  (count-lines (point-min) (point-max)))))
    (make-directory dir t)
    (unless (file-exists-p object-path)
      (let ((coding-system-for-write 'utf-8-unix))
        (with-temp-file object-path
          (insert body))))
    (unless (file-exists-p manifest-path)
      (let ((coding-system-for-write 'utf-8-unix))
        (with-temp-file manifest-path
          (insert (my/gptel--tool-output-metadata-json tool metadata hash body)))))
    `((id . ,(concat "sha256:" hash))
      (tool . ,tool)
      (bytes . ,(string-bytes body))
      (chars . ,(length body))
      (lines . ,lines)
      (object_path . ,(abbreviate-file-name object-path))
      (manifest_path . ,(abbreviate-file-name manifest-path)))))

(defun my/gptel--tool-output-reference-block (reference cap-chars)
  "Return an org-friendly reference block for stored output REFERENCE."
  (format ":TOOL_OUTPUT_REF:\n:id %s\n:tool %s\n:bytes %s\n:chars %s\n:lines %s\n:object_path %s\n:manifest_path %s\n:summary Output exceeded cap_chars=%d, so the raw body was stored out-of-band and omitted from this transcript. Treat the stored object as sensitive local data.\n:END:"
          (alist-get 'id reference)
          (alist-get 'tool reference)
          (alist-get 'bytes reference)
          (alist-get 'chars reference)
          (alist-get 'lines reference)
          (alist-get 'object_path reference)
          (alist-get 'manifest_path reference)
          cap-chars))

(defun my/gptel--truncate-tool-result (text &optional max-chars)
  "Return TEXT capped to MAX-CHARS for compact tool transcripts."
  (let ((max-chars (or max-chars my/gptel--tool-result-max-chars)))
    (if (<= (length text) max-chars)
        text
      (concat (substring text 0 max-chars)
              (format "\n\n[Tool result truncated after %d of %d characters. Narrow the request or use pagination to inspect the rest.]"
                      max-chars (length text))))))

(defun my/gptel--format-metadata-value (value)
  "Return VALUE formatted for a compact tool metadata header."
  (cond
   ((null value) nil)
   ((eq value t) "true")
   ((eq value :false) "false")
   ((stringp value) value)
   (t (format "%s" value))))

(defun my/gptel--format-tool-result (tool metadata body &optional next max-chars)
  "Return BODY with a compact metadata header for TOOL.
METADATA is an alist of (KEY . VALUE). NEXT is a recommended follow-up.
The body is capped to MAX-CHARS, TOOL's specific cap, or
`my/gptel--tool-result-max-chars'."
  (let* ((body (or body ""))
         (max-chars (my/gptel--tool-result-cap tool max-chars))
         (body-chars (length body))
         (truncated (> body-chars max-chars))
         (reference (when truncated
                      (my/gptel--store-tool-output tool metadata body)))
         (shown-body (if truncated
                         (my/gptel--tool-output-reference-block reference max-chars)
                       body))
         (metadata-lines
          (delq nil
                (mapcar (lambda (entry)
                          (when-let ((value (my/gptel--format-metadata-value (cdr entry))))
                            (format "%s: %s" (car entry) value)))
                        (append `((tool . ,tool)
                                  (body_chars . ,body-chars)
                                  (cap_chars . ,max-chars)
                                  (stored_ref . ,(alist-get 'id reference))
                                  (truncated . ,(if truncated "yes" "no")))
                                metadata
                                (when next `((next . ,next))))))))
    (concat "Metadata:\n"
            (mapconcat #'identity metadata-lines "\n")
            "\n\n"
            shown-body
            (when truncated
              (format "\n\n[Tool result stored after exceeding the `%s` cap: %d of %d body characters were omitted from this transcript. Use the reference above plus the `next` hint in Metadata to narrow or paginate.]"
                      tool max-chars body-chars)))))

;; custom tools for use in 'gptel' mode

(gptel-make-tool
 :name "edit_buffer"
 :function (lambda (buffer old-str new-str &optional replace-all no-save start-line end-line)
             (condition-case err
                 (with-current-buffer (my/gptel--resolve-buffer buffer)
                   (let ((result (if (and start-line (not (string-empty-p start-line)))
                                     (my/gptel--buffer-edit-hashline buffer start-line end-line new-str)
                                   (my/gptel--buffer-edit-string buffer old-str new-str replace-all))))
                     (when (and (not no-save)
                                (not (string-prefix-p "Error:" result))
                                (buffer-file-name)
                                (buffer-modified-p))
                       (save-buffer)
                       (setq result (concat result " (saved)")))
                     result))
               (error (format "Error in edit_buffer: %s" (error-message-string err)))))
 :description "Edit an Emacs buffer, then auto-save if the buffer visits a file. Preferred mode: pass START_LINE and optional END_LINE as hashline tags from read_file output, e.g. `42:abc`, and NEW_STR as the replacement text. The tool verifies the current line hashes before editing, scans nearby lines if tags shifted, and refuses stale/ambiguous tags with fresh context. Fallback mode: omit START_LINE and replace exact OLD_STR with NEW_STR; OLD_STR must be unique unless REPLACE_ALL is true."
 :args (list '(:name "buffer"
               :type "string"
               :description "Buffer name or file path.  Use list_buffers to find valid names.")
             '(:name "old_str"
               :type "string"
               :description "Fallback exact text to replace when start_line is omitted. Use the empty string in hashline mode. Whitespace and newlines must match exactly. Must be unique unless replace_all is true.")
             '(:name "new_str"
               :type "string"
               :description "Replacement text. In hashline mode, this replaces the entire tagged line/range. Use the empty string to delete.")
             '(:name "replace_all"
               :type "boolean"
               :description "Fallback exact-text mode only: when true, replace every occurrence of old_str. Default: false (error if old_str matches more than once).")
             '(:name "no_save"
               :type "boolean"
               :description "When true, skip the automatic save.  Default: false (changes are saved immediately if the buffer visits a file).")
             '(:name "start_line"
               :type "string"
               :description "Preferred hashline edit anchor copied from read_file output, e.g. `42:abc`. When set, old_str is ignored.")
             '(:name "end_line"
               :type "string"
               :description "Optional ending hashline tag for a multi-line replacement. Defaults to start_line."))
 :confirm t
 :category "emacs")


(gptel-make-tool
 :name "show_buffer_context"
 :function (lambda (buffer line-number &optional context-lines)
             (let ((line-number (round line-number))
                   (context-lines (and context-lines (round context-lines))))
               (my/gptel--with-buffer-safety (my/gptel--resolve-buffer buffer) "showing context"
                 (my/gptel--ensure-readable-buffer "show_buffer_context")
                 (save-excursion
                   (let* ((context-size (or context-lines 5))
                          (total-lines (count-lines (point-min) (point-max)))
                          (start-line (max 1 (- line-number context-size)))
                          (end-line (min total-lines (+ line-number context-size)))
                          (lines '()))
                     (goto-char (point-min))
                     (forward-line (1- start-line))
                     (dotimes (i (1+ (- end-line start-line)))
                       (let ((current-line-num (+ start-line i))
                             (line-content (buffer-substring-no-properties
                                            (line-beginning-position)
                                            (line-end-position))))
                         (push (format "%s%d: %s"
                                       (if (= current-line-num line-number) ">>> " "    ")
                                       current-line-num
                                       line-content) lines))
                       (forward-line 1))
                     (my/gptel--format-tool-result
                      "show_buffer_context"
                      `((target . ,buffer)
                        (line . ,line-number)
                        (range . ,(format "%d-%d" start-line end-line))
                        (total_lines . ,total-lines))
                      (format "Context around line %d in buffer '%s' (lines %d-%d of %d):\n%s"
                              line-number buffer start-line end-line total-lines
                              (mapconcat 'identity (nreverse lines) "\n"))
                      "Call show_buffer_context with a smaller context-lines value, or read_file with start-line/end-line for a precise range."))))))
 :description "Show the lines around a specific line number in a buffer, with the target line marked.  Useful for understanding code structure and indentation before editing."
 :args (list '(:name "buffer"
               :type "string"
               :description "The name of the buffer to examine.")
             '(:name "line-number"
               :type "number"
               :description "The line number to show context around (1-based).")
             '(:name "context-lines"
               :type "number"
               :description "Number of lines before and after to show (default: 5)."))
 :include nil
 :category "emacs")

(gptel-make-tool
 :name "search_buffer_text"
 :function (lambda (buffer search-text)
             (my/gptel--with-buffer-safety (my/gptel--resolve-buffer buffer) "searching buffer"
               (my/gptel--ensure-readable-buffer "search_buffer_text")
               (save-excursion
                 (if (string-empty-p search-text)
                     "Error: search-text must not be empty."
                   (goto-char (point-min))
                   (let ((matches '())
                         (count 0)
                         (max-show 50))
                     (while (search-forward search-text nil t)
                       (setq count (1+ count))
                       (when (<= count max-show)
                         (let ((line-num (line-number-at-pos))
                               (line-content (string-trim (thing-at-point 'line t))))
                           (push (format "Line %d: %s" line-num line-content) matches))))
                     (my/gptel--format-tool-result
                      "search_buffer_text"
                      `((target . ,buffer)
                        (query . ,search-text)
                        (match_count . ,count)
                        (shown_count . ,(length matches)))
                      (if matches
                          (format "Found %d occurrence%s of '%s' in buffer '%s'%s:\n%s"
                                  count
                                  (if (= count 1) "" "s")
                                  search-text buffer
                                  (if (> count max-show)
                                      (format " (showing first %d)" max-show)
                                    "")
                                  (mapconcat 'identity (nreverse matches) "\n"))
                        (format "Text '%s' not found in buffer '%s'" search-text buffer))
                      "If shown_count is lower than match_count, refine search-text or use show_buffer_context around a reported line."))))))
 :description "Search for text in a buffer and return all matching line numbers and content.  Useful before editing to confirm the target text exists and is unique."
 :args (list '(:name "buffer"
               :type "string"
               :description "The name of the buffer to search in.")
             '(:name "search-text"
               :type "string"
               :description "The text to search for (e.g., a function name or key phrase)."))
 :include nil
 :category "emacs")

(gptel-make-tool
 :name "save_buffer"
 :function (lambda (buffer)
             (my/gptel--with-buffer-safety (my/gptel--resolve-buffer buffer) "saving buffer"
               (if (buffer-file-name)
                   (progn
                     (save-buffer)
                     (format "Saved buffer '%s' to file: %s" buffer (buffer-file-name)))
                 (format "Buffer '%s' is not associated with a file. Use overwrite_file to save it to a path." buffer))))
 :description "Save buffer changes to disk.  edit_buffer auto-saves after each edit, so this is mainly needed when batching multiple edits with no_save=true before a single save."
 :args (list '(:name "buffer"
               :type "string"
               :description "The name of the buffer to save."))
 :confirm t
 :category "emacs")

(gptel-make-tool
 :name "read_file"
  :description "[READ-ONLY] Native Emacs file reader. Use this instead of shell commands like cat/head/tail/sed for inspecting file contents. Opens a file in Emacs and returns its contents with each line prefixed by `N:HHH|' (line number plus short content hash). By default returns the first 2000 lines; pass START-LINE and/or END-LINE to paginate through larger files."
  :args (list '(:name "path"
                :type "string"
                :description "Path to the file (e.g. ~/notes/todo.txt).")
              '(:name "start-line"
                :type "number"
                :description "First line to return (1-based, inclusive).  Defaults to 1.")
              '(:name "end-line"
                :type "number"
                :description "Last line to return (1-based, inclusive).  Defaults to start-line + 1999 (a 2000-line window)."))
 :function (lambda (path &optional start-line end-line)
             (condition-case err
                 (let ((existing (get-file-buffer path))
                       (buf (find-file-noselect path)))
                   (unwind-protect
                       (with-current-buffer buf
                         (my/gptel--ensure-readable-buffer "read_file")
                         (let* ((total (count-lines (point-min) (point-max)))
                                (start-line (and start-line (round start-line)))
                                (end-line (and end-line (round end-line)))
                                (start (max 1 (or start-line 1)))
                                (requested-end (or end-line (+ start 1999)))
                                (end (min total requested-end)))
                           (my/gptel--format-tool-result
                            "read_file"
                            `((target . ,(abbreviate-file-name (expand-file-name path)))
                              (range . ,(if (zerop total) "0-0" (format "%d-%d" start end)))
                              (total_lines . ,total)
                              (requested_end . ,requested-end))
                            (my/gptel--render-hashed-lines path start-line end-line)
                            "Call read_file again with start-line/end-line for the next page or a narrower range.")))
                     (unless existing
                       (kill-buffer buf))))
               (error (format "Error reading file '%s': %s" path (error-message-string err)))))
 :include nil
 :category "filesystem")

(gptel-make-tool
 :name "open_file"
 :function (lambda (path)
             (condition-case err
                 (let* ((expanded (expand-file-name path))
                        (buf (find-file-noselect expanded)))
                   (format "Opened '%s' in buffer: %s" expanded (buffer-name buf)))
               (error (format "Error opening file '%s': %s" path (error-message-string err)))))
 :description "Open a file in Emacs and return its buffer name.  Unlike read_file, the buffer is kept alive so you can edit it with edit_buffer.  Pass the returned buffer name to edit_buffer, save_buffer, etc."
 :args (list '(:name "path"
               :type "string"
               :description "Path to the file to open (relative or absolute)."))
 :include nil
 :category "filesystem")

(gptel-make-tool
 :name "list_buffers"
 :function (lambda ()
             (let* ((bufs (seq-filter
                           (lambda (b)
                             (not (string-prefix-p " " (buffer-name b))))
                           (buffer-list)))
                    (max-show 100)
                    (entries
                     (mapcar (lambda (buf)
                               (with-current-buffer buf
                                 (format "  %-35s  %-20s  %7d bytes  %s"
                                         (buffer-name)
                                         (symbol-name major-mode)
                                         (buffer-size)
                                         (or (buffer-file-name) "(no file)"))))
                             bufs)))
               (my/gptel--format-tool-result
                "list_buffers"
                `((buffer_count . ,(length entries))
                  (shown_count . ,(min (length entries) max-show)))
                (format "%d buffer%s%s:\n%s"
                        (length entries)
                        (if (= (length entries) 1) "" "s")
                        (if (> (length entries) max-show)
                            (format " (showing first %d)" max-show)
                          "")
                        (mapconcat #'identity
                                   (cl-subseq entries 0 (min (length entries) max-show))
                                   "\n"))
                "Use a specific buffer name with show_buffer_context, search_buffer_text, or buffer_diagnostics.")))
 :description "List all visible Emacs buffers with their name, major mode, size, and associated file path.  Use this to find the exact buffer name to pass to edit_buffer and similar tools.  Buffer names and file paths are both accepted by any tool with a 'buffer' argument."
 :args '()
 :include nil
 :category "emacs")

(gptel-make-tool
 :name "run_shell_command"
 :function (lambda (command)
             (condition-case err
                 (let ((temp-buffer (generate-new-buffer " *shell-output*")))
                   (unwind-protect
                       (let* ((exit-code (call-process-shell-command command nil temp-buffer t))
                              (output (with-current-buffer temp-buffer (buffer-string)))
                              (trimmed-output (string-trim output)))
                         (my/gptel--format-tool-result
                          "run_shell_command"
                          `((command . ,command)
                            (exit_code . ,exit-code)
                            (output_chars . ,(length output)))
                          (format "```bash\n%s\n```\n\n**Exit code:** %d\n\n**Output:**\n```\n%s\n```"
                                  command exit-code
                                  (if (string-empty-p trimmed-output)
                                      "(no output)"
                                    trimmed-output))
                          "If output was truncated, rerun a narrower command or redirect output to a file and inspect targeted ranges."))
                     (when (buffer-live-p temp-buffer)
                       (kill-buffer temp-buffer))))
               (error (format "Command: %s\nError: %s" command (error-message-string err)))))
 :description "Run an arbitrary shell command and return its output and exit code.

LAST RESORT ONLY. Do NOT use this for routine project navigation, file inspection, or text search.

Use native Emacs tools instead:
  - ls/find/tree/pwd-style file discovery -> list_project_files
  - cat/head/tail/sed-style file reading -> read_file or show_buffer_context
  - grep/rg/find+xargs-style text search -> search_project or search_buffer_text
  - git status/diff -> git_status / git_diff

Reserve this tool for commands that genuinely need an external process: tests, builds, linters/formatters, package managers, one-off system inspection, or git operations not covered by git_status/git_diff."
 :args (list '(:name "command"
               :type "string"
               :description "External command to execute. Before using this, prefer list_project_files/read_file/search_project/show_buffer_context/search_buffer_text/git_status/git_diff when they can answer the question."))
 :confirm t
 :include nil
 :category "shell")


(gptel-make-tool
 :name "indent_region"
 :function (lambda (buffer start-line end-line)
             (let ((start-line (round start-line))
                   (end-line (round end-line)))
               (my/gptel--with-buffer-safety (my/gptel--resolve-buffer buffer) "indenting region"
                 (let ((total-lines (count-lines (point-min) (point-max))))
                   (cond
                    ((or (< start-line 1)
                         (> end-line total-lines)
                         (> start-line end-line))
                     (format "Error: Invalid line range %d-%d (total lines: %d)"
                             start-line end-line total-lines))
                    (t
                     (save-excursion
                       (goto-char (point-min))
                       (forward-line (1- start-line))
                       (let ((start (point)))
                         (forward-line (- end-line start-line))
                         (end-of-line)
                         (indent-region start (point))))
                     (format "Indented lines %d-%d in buffer '%s' using %s rules"
                             start-line end-line buffer major-mode)))))))
 :description "Re-indent lines START-LINE through END-LINE in BUFFER using the buffer's major-mode indentation logic (calls `indent-region').  Use AFTER edit_buffer if inserted code needs reformatting.  Note: indentation is the major mode's interpretation, which may differ from what you wrote -- re-read the buffer afterwards to confirm."
 :args (list '(:name "buffer"
               :type "string"
               :description "Name of the buffer to re-indent.")
             '(:name "start-line"
               :type "number"
               :description "First line of the region to indent (1-based, inclusive).")
             '(:name "end-line"
               :type "number"
               :description "Last line of the region to indent (1-based, inclusive)."))
 :confirm t
 :category "emacs")

(gptel-make-tool
 :name "check_parens"
 :function (lambda (buffer)
             (condition-case err
                 (with-current-buffer (my/gptel--resolve-buffer buffer)
                   (save-excursion
                     (check-parens))
                   (format "Parentheses and expressions are balanced in buffer '%s'." buffer))
               (error (format "Unbalanced parentheses detected in buffer '%s': %s"
                              buffer (error-message-string err)))))
 :description "Checks the specified buffer for unbalanced parentheses. This function verifies that all parentheses, brackets, and quotes are correctly matched according to the buffer's syntax rules. It returns a success message if balanced, or an error if an imbalance is found."
 :args (list '(:name "buffer"
               :type "string"
               :description "The name of the buffer to check."))
 :category "emacs")


(gptel-make-tool
 :name "byte_compile_file"
 :description "Byte-compile an Emacs Lisp file to check for syntax errors. Returns a success message or the compilation log on failure."
 :args (list '(:name "filename" :type "string" :description "The path to the Emacs Lisp file to compile."))
 :function (lambda (filename)
             (condition-case err
                 (progn
                   (unless (file-exists-p filename)
                     (error "File not found: %s" filename))
                   (with-current-buffer (get-buffer-create "*Compile-Log*")
                     (erase-buffer))
                   (if (byte-compile-file filename)
                       (format "Successfully compiled '%s' with no errors." filename)
                     (let ((log-output (with-current-buffer (get-buffer "*Compile-Log*")
                                         (buffer-string))))
                       (format "Compilation of '%s' failed. Errors:\n%s" filename log-output))))
               (error (format "An error occurred while trying to compile '%s': %s"
                              filename (error-message-string err)))))
 :confirm t
 :category "emacs")

(gptel-make-tool
 :name "search_project"
 :function (lambda (pattern &optional dir file-glob case-sensitive)
             (condition-case err
                 (let* ((rg (executable-find "rg"))
                        (search-dir (or dir
                                        (condition-case nil
                                            (projectile-project-root)
                                          (error default-directory))))
                        (expanded-dir (expand-file-name search-dir)))
                   (cond
                    ((not rg)
                     "Error: ripgrep ('rg') is not installed or not on PATH.")
                    ((not (file-directory-p expanded-dir))
                     (format "Error: directory does not exist: %s" expanded-dir))
                    (t
                     (let* ((args (append
                                   (list "--line-number" "--no-heading" "--color=never" "--follow" "--no-messages"
                                         (if case-sensitive "--case-sensitive" "--ignore-case"))
                                   (when file-glob (list "--glob" file-glob))
                                   (list "--" pattern expanded-dir)))
                            (exit-status nil)
                            (output (with-temp-buffer
                                      (setq exit-status
                                            (apply #'call-process rg nil t nil args))
                                      (buffer-string)))
                            (trimmed (string-trim output)))
                       (cond
                        ((and (= exit-status 1) (string-empty-p trimmed))
                         (my/gptel--format-tool-result
                          "search_project"
                          `((target . ,(abbreviate-file-name search-dir))
                            (query . ,pattern)
                            (file_glob . ,file-glob)
                            (exit_code . ,exit-status)
                            (match_count . 0)
                            (shown_count . 0))
                          (format "No matches for '%s' in %s"
                                  pattern (abbreviate-file-name search-dir))
                          "Broaden the pattern or adjust file-glob/dir if needed."))
                        ((not (string-empty-p trimmed))
                         (let ((lines (split-string trimmed "\n" t))
                               (max-show 50))
                           (my/gptel--format-tool-result
                            "search_project"
                            `((target . ,(abbreviate-file-name search-dir))
                              (query . ,pattern)
                              (file_glob . ,file-glob)
                              (exit_code . ,exit-status)
                              (match_count . ,(length lines))
                              (shown_count . ,(min (length lines) max-show)))
                            (format "Found %d match%s for '%s' in %s%s:\n%s"
                                    (length lines)
                                    (if (= (length lines) 1) "" "es")
                                    pattern
                                    (abbreviate-file-name search-dir)
                                    (if (> (length lines) max-show)
                                        (format " (showing first %d)" max-show)
                                      "")
                                    (mapconcat 'identity
                                               (cl-subseq lines 0 (min (length lines) max-show))
                                               "\n"))
                            "If shown_count is lower than match_count, narrow dir/file-glob or use a more specific pattern.")))
                        ((not (zerop exit-status))
                         (my/gptel--format-tool-result
                          "search_project"
                          `((target . ,(abbreviate-file-name search-dir))
                            (query . ,pattern)
                            (file_glob . ,file-glob)
                            (exit_code . ,exit-status))
                          (format "Error: rg exited with status %d."
                                  exit-status)
                          "Check the regex pattern, directory, and file-glob."))
                        (t
                         (my/gptel--format-tool-result
                          "search_project"
                          `((target . ,(abbreviate-file-name search-dir))
                            (query . ,pattern)
                            (file_glob . ,file-glob)
                            (exit_code . ,exit-status)
                            (match_count . 0)
                            (shown_count . 0))
                          (format "No matches for '%s' in %s"
                                  pattern (abbreviate-file-name search-dir))
                          "Broaden the pattern or adjust file-glob/dir if needed.")))))))
               (error (format "Error searching project: %s" (error-message-string err)))))
 :description "Search for a regex pattern across project files without invoking run_shell_command. Use this instead of grep/rg/find+xargs shell commands. Respects .gitignore via ripgrep. Returns matching lines with file path and line number."
 :args (list '(:name "pattern"
               :type "string"
               :description "Regex pattern to search for (ripgrep syntax).")
             '(:name "dir"
               :type "string"
               :description "Directory to search in (default: projectile project root or current directory).")
             '(:name "file-glob"
               :type "string"
               :description "Optional file type filter (e.g. '*.el', '*.{ts,tsx}').")
             '(:name "case-sensitive"
               :type "boolean"
               :description "Set to true for case-sensitive search (default: case-insensitive)."))
 :include nil
 :category "search")

(gptel-make-tool
 :name "list_project_files"
 :function (lambda (&optional pattern dir)
             (condition-case err
                 (let* ((root (or dir
                                  (condition-case nil
                                      (projectile-project-root)
                                    (error default-directory))))
                        (all-files
                         (condition-case nil
                             (projectile-project-files root)
                           (error
                            (directory-files-recursively root ".*" t))))
                        (filtered
                         (if pattern
                             (seq-filter (lambda (f) (string-match-p pattern f)) all-files)
                           all-files))
                        (max-show 100))
                   (my/gptel--format-tool-result
                    "list_project_files"
                    `((target . ,(abbreviate-file-name root))
                      (pattern . ,pattern)
                      (file_count . ,(length filtered))
                      (shown_count . ,(min (length filtered) max-show)))
                    (if (null filtered)
                        (format "No files found in %s%s"
                                (abbreviate-file-name root)
                                (if pattern (format " matching '%s'" pattern) ""))
                      (format "%d file%s in %s%s:\n%s"
                              (length filtered)
                              (if (= (length filtered) 1) "" "s")
                              (abbreviate-file-name root)
                              (if (> (length filtered) max-show)
                                  (format " (showing first %d)" max-show)
                                "")
                              (mapconcat 'identity
                                         (cl-subseq filtered 0 (min (length filtered) max-show))
                                         "\n")))
                    "If shown_count is lower than file_count, pass a narrower pattern or dir."))
               (error (format "Error listing files: %s" (error-message-string err)))))
 :description "List files in the project using native Emacs/projectile APIs, optionally filtered by a regex pattern. Use this instead of ls/find/tree shell commands. Respects .gitignore when projectile is available."
 :args (list '(:name "pattern"
               :type "string"
               :description "Optional regex to filter filenames (e.g. '\\.el$', 'src/').")
             '(:name "dir"
               :type "string"
               :description "Directory to list (default: projectile project root or current directory)."))
 :include nil
 :category "search")

(gptel-make-tool
 :name "create_file"
 :function (lambda (path content)
             (condition-case err
                 (let ((expanded (expand-file-name path)))
                   (when (file-exists-p expanded)
                     (error "File already exists: %s.  Use overwrite_file to replace it." expanded))
                   (let ((dir (file-name-directory expanded)))
                     (when (and dir (not (file-directory-p dir)))
                       (mkdir dir t)))
                   (with-temp-file expanded
                     (insert content))
                   (format "Created %s (%d bytes)" expanded (length content)))
               (error (format "Error creating file '%s': %s" path (error-message-string err)))))
 :description "Create a new file.  Errors if the file already exists (use overwrite_file to replace it).  Creates parent directories as needed."
 :args (list '(:name "path"
               :type "string"
               :description "File path to create (relative or absolute).")
             '(:name "content"
               :type "string"
               :description "Content to write to the new file."))
 :confirm t
 :category "filesystem")

(gptel-make-tool
 :name "overwrite_file"
 :function (lambda (path content)
             (condition-case err
                 (let ((expanded (expand-file-name path)))
                   (let ((dir (file-name-directory expanded)))
                     (when (and dir (not (file-directory-p dir)))
                       (mkdir dir t)))
                   (with-temp-file expanded
                     (insert content))
                   (format "Wrote %s (%d bytes)" expanded (length content)))
               (error (format "Error writing file '%s': %s" path (error-message-string err)))))
 :description "Overwrite a file with new content, replacing it entirely.  Creates the file and parent directories if they do not exist.  Use create_file when you want to be sure you are not clobbering an existing file."
 :args (list '(:name "path"
               :type "string"
               :description "File path to write (relative or absolute).")
             '(:name "content"
               :type "string"
               :description "Full content to write to the file."))
 :confirm t
 :category "filesystem")

(gptel-make-tool
 :name "git_status"
 :function (lambda ()
             (require 'magit-git)
             (condition-case err
                 (let ((output (with-temp-buffer
                                 (magit-git-insert "status" "--porcelain=v1")
                                 (buffer-string))))
                   (if (string-empty-p (string-trim output))
                       (my/gptel--format-tool-result
                        "git_status"
                        '((target . "current git repository")
                          (entry_count . 0))
                        "Working tree clean. No staged, unstaged, or untracked changes."
                        "Use git_diff only if you need to verify a specific path.")
                     (let* ((trimmed (string-trim output))
                            (lines (split-string trimmed "\n" t)))
                       (my/gptel--format-tool-result
                        "git_status"
                        `((target . "current git repository")
                          (entry_count . ,(length lines)))
                        (format "```\n%s\n```" trimmed)
                        "Use git_diff with staged/path arguments to inspect the relevant changes."))))
               (error (format "Error getting git status: %s" (error-message-string err)))))
 :description "Show git status in porcelain format. See https://git-scm.com/docs/git-status#_porcelain_format_format for key."
 :args nil
 :include nil
 :category "git")

(gptel-make-tool
 :name "git_diff"
 :function (lambda (&optional staged path)
             (require 'magit-git)
             (condition-case err
                 (let* ((args `("diff"
                                ,@(when staged '("--staged"))
                                ,@(when path (list "--" path))))
                        (output (with-temp-buffer
                                  (apply #'magit-git-insert args)
                                  (buffer-string))))
                   (if (string-empty-p (string-trim output))
                       (my/gptel--format-tool-result
                        "git_diff"
                        `((target . ,(or path "current git repository"))
                          (staged . ,(if staged "yes" "no"))
                          (diff_chars . 0)
                          (file_count . 0))
                        (format "No %schanges%s."
                                (if staged "staged " "")
                                (if path (format " for %s" path) ""))
                        "No follow-up needed unless you want a different staged/path scope.")
                     (let* ((trimmed (string-trim output))
                            (file-count (with-temp-buffer
                                          (insert trimmed)
                                          (goto-char (point-min))
                                          (cl-loop while (re-search-forward "^diff --git " nil t)
                                                   count 1))))
                       (my/gptel--format-tool-result
                        "git_diff"
                        `((target . ,(or path "current git repository"))
                          (staged . ,(if staged "yes" "no"))
                          (diff_chars . ,(length trimmed))
                          (file_count . ,file-count))
                        (format "```diff\n%s\n```" trimmed)
                        "If truncated, call git_diff with a narrower path or staged scope."))))
               (error (format "Error getting git diff: %s" (error-message-string err)))))
 :description "Show git diff of unstaged changes (or staged if STAGED is non-nil). Optionally limit to PATH."
 :args (list '(:name "staged"
               :type "boolean"
               :description "Show staged changes instead of unstaged.")
             '(:name "path"
               :type "string"
               :description "Limit diff to a specific file or directory."))
 :include nil
 :category "git")

(gptel-make-tool
 :name "buffer_diagnostics"
 :function (lambda (buffer)
             (my/gptel--with-buffer-safety (my/gptel--resolve-buffer buffer) "checking diagnostics"
               (my/gptel--ensure-readable-buffer "buffer_diagnostics")
               (cond
                ((not (bound-and-true-p flymake-mode))
                 (my/gptel--format-tool-result
                  "buffer_diagnostics"
                  `((target . ,buffer)
                    (flymake_active . "no")
                    (diagnostic_count . 0))
                  (format "flymake-mode is not active in buffer '%s'." buffer)
                  "Enable flymake-mode or run a file-specific validator if diagnostics are needed."))
                ((null (flymake-diagnostics))
                 (my/gptel--format-tool-result
                  "buffer_diagnostics"
                  `((target . ,buffer)
                    (flymake_active . "yes")
                    (diagnostic_count . 0))
                  (format "No diagnostics in buffer '%s'." buffer)
                  "No follow-up needed unless the buffer changed; rerun after edits."))
                (t
                 (let* ((diags (flymake-diagnostics))
                        (grouped (seq-group-by #'flymake-diagnostic-type diags))
                        (severity-counts
                         (mapconcat (lambda (group)
                                      (format "%s=%d" (car group) (length (cdr group))))
                                    grouped ", ")))
                   (my/gptel--format-tool-result
                    "buffer_diagnostics"
                    `((target . ,buffer)
                      (flymake_active . "yes")
                      (diagnostic_count . ,(length diags))
                      (severity_counts . ,severity-counts))
                    (format "%d diagnostic%s in '%s':%s"
                            (length diags)
                            (if (= (length diags) 1) "" "s")
                            buffer
                            (mapconcat
                             (lambda (group)
                               (format "\n\n[%s]\n%s"
                                       (car group)
                                       (mapconcat
                                        (lambda (d)
                                          (format "  Line %d: %s"
                                                  (line-number-at-pos
                                                   (flymake-diagnostic-beg d))
                                                  (flymake-diagnostic-text d)))
                                        (cdr group)
                                        "\n")))
                             grouped
                             ""))
                    "Use show_buffer_context around a diagnostic line, then edit and rerun buffer_diagnostics."))))))
 :description "Show flymake diagnostics (errors, warnings, notes) for a buffer, grouped by severity. Requires flymake-mode to be active in the buffer."
 :args (list '(:name "buffer"
               :type "string"
               :description "The name of the buffer to check diagnostics for."))
 :include nil
 :category "emacs")

(defconst my/gptel--subagent-preset-prefix "agent-"
  "Preset-name prefix used to expose gptel presets as sub-agents.")

(defun my/gptel--subagent-preset-names ()
  "Return names of gptel presets available to `delegate_agent'."
  (cl-loop for (name . _spec) in gptel--known-presets
           for name-string = (symbol-name name)
           when (string-prefix-p my/gptel--subagent-preset-prefix name-string)
           collect name-string))

(defun my/gptel--update-subagent-tool-enum ()
  "Refresh `delegate_agent' subagent_type enum from live agent presets."
  (when-let* ((names (my/gptel--subagent-preset-names))
              (tool (ignore-errors (gptel-get-tool "delegate_agent")))
              (subagent-arg (car (gptel-tool-args tool))))
    (setf (plist-get subagent-arg :enum) (vconcat names))))

(defun my/gptel--run-subagent (callback subagent-type description prompt)
  "Run SUBAGENT-TYPE on PROMPT and return the final result through CALLBACK.
DESCRIPTION is a short user-visible task label.  The sub-agent runs as a
fresh gptel request with no inherited gptel context."
  (condition-case err
      (let* ((subagent-name (string-trim subagent-type))
             (available (my/gptel--subagent-preset-names))
             (preset (intern subagent-name)))
        (if (not (member subagent-name available))
            (funcall callback
                     (format "Error: Unknown subagent_type '%s'. Available sub-agents: %s"
                             subagent-type
                             (if available
                                 (mapconcat #'identity available ", ")
                               "(none)")))
          (let ((partial (format "%s result for task: %s\n\n"
                                 subagent-name description))
                (done nil))
            (message "Launching gptel sub-agent %s: %s" subagent-name description)
            (gptel-with-preset
                `(:parents ,preset
                  :context nil
                  :use-tools t
                  :include-reasoning nil)
              (gptel-request prompt
                :stream nil
                :callback
                (lambda (response info)
                  (cond
                   ((stringp response)
                    (setq partial (concat partial response))
                    (unless (or done (plist-get info :tool-use))
                      (setq done t)
                      (funcall callback
                               (my/gptel--format-tool-result
                                "delegate_agent"
                                `((subagent . ,subagent-name)
                                  (task . ,description))
                                partial
                                "If stored or truncated, ask delegate_agent for a narrower task or result."))))
                   ((and (consp response) (eq (car response) 'tool-call))
                    (gptel--display-tool-calls (cdr response) info))
                   ((and (consp response) (eq (car response) 'tool-result))
                    nil)
                   ((eq response 'abort)
                    (unless done
                      (setq done t)
                      (funcall callback
                               (format "Error: Sub-agent %s aborted task '%s'."
                                       subagent-name description))))
                   ((null response)
                    (unless done
                      (setq done t)
                      (funcall callback
                               (format "Error: Sub-agent %s failed task '%s': %s"
                                       subagent-name description
                                       (or (plist-get info :status) "no response")))))
                   ((eq response t)
                    nil)
                   (t
                    (unless done
                      (setq done t)
                      (funcall callback
                               (format "Error: Sub-agent %s returned unexpected response for task '%s': %S"
                                       subagent-name description response)))))))))))
    (error (funcall callback
                    (format "Error launching sub-agent '%s': %s"
                            subagent-type (error-message-string err))))))

(gptel-make-tool
 :name "delegate_agent"
 :function #'my/gptel--run-subagent
 :description "Launch a sibling gptel sub-agent for an independent multi-step task.  The sub-agent uses one of the existing agent-* presets, runs with fresh request context (`:context nil`, so it does not inherit the parent conversation), may use that preset's tools autonomously, and returns one consolidated result string to the parent.  Use agent-read for context-cheap research or inspection; choose mutating sub-agents only when file edits or shell access are intentionally needed."
 :args (list '(:name "subagent_type"
               :type "string"
               :enum ["agent-read" "agent-edit" "agent-shell"]
               :description "Which agent-* preset to launch.  This enum is refreshed from the live preset roster when agent presets are applied.")
             '(:name "description"
               :type "string"
               :description "Short 3-5 word task label used for status/result headers.")
             '(:name "prompt"
               :type "string"
               :description "Detailed instructions for the sub-agent.  Include exactly what to investigate or change, constraints to follow, and what information to return."))
 :async t
 :confirm t
 :include t
 :category "agent")

;; --- Agentic preset helpers ---

(defvar-local my/gptel-prompt-size-guard-enabled nil
  "Non-nil means `gptel-request' checks prompt size before dispatch.
Agentic presets enable this buffer-locally; ordinary chat notebooks can leave
it nil or set it nil to opt out.")

(defcustom my/gptel--prompt-size-soft-chars 120000
  "Approximate prompt size where agentic gptel requests warn before sending."
  :type 'integer
  :group 'gptel)

(defcustom my/gptel--prompt-size-hard-chars 240000
  "Approximate prompt size where agentic gptel requests abort before sending."
  :type 'integer
  :group 'gptel)

(defun my/gptel--agent-preset-preflight-setup ()
  "Enable agentic preflight guardrails for the current gptel buffer."
  (setq-local my/gptel-prompt-size-guard-enabled t))

(defun my/gptel--agent-edit-preflight-setup ()
  "Enable edit-agent setup hooks for the current gptel buffer."
  (my/gptel--agent-preset-preflight-setup)
  (my/gptel--update-subagent-tool-enum))

(defun my/gptel--string-size (value)
  "Return an approximate character size for VALUE."
  (cond
   ((null value) 0)
   ((stringp value) (length value))
   ((functionp value)
    (my/gptel--string-size
     (condition-case nil
         (funcall value)
       (error nil))))
   (t (length (format "%S" value)))))

(defun my/gptel--request-prompt-size (prompt args)
  "Estimate the character size of a pending gptel request.
PROMPT and ARGS are the arguments received by `gptel-request'."
  (let* ((buffer (or (plist-get args :buffer) (current-buffer)))
         (system (if (plist-member args :system)
                     (plist-get args :system)
                   gptel--system-message))
         (context (plist-get args :context)))
    (+ (my/gptel--string-size system)
       (my/gptel--string-size context)
       (if prompt
           (my/gptel--string-size prompt)
         (with-current-buffer buffer
           (if (use-region-p)
               (- (region-end) (region-beginning))
             (- (or (plist-get args :position) (point)) (point-min))))))))

(defun my/gptel--prompt-size-warning (size threshold kind)
  "Return actionable prompt-size warning text for SIZE, THRESHOLD, and KIND."
  (format "%s gptel prompt-size guard: estimated request is ~%d chars (threshold %d). To avoid provider-side token runaway, narrow the context, start a fresh chat, use Org topic/branching, reduce messages-to-send, or remove diagnostic/tool transcript dumps before retrying."
          kind size threshold))

(defun my/gptel--prompt-size-preflight (prompt &rest args)
  "Warn or abort oversized agentic gptel requests before provider dispatch."
  (let ((buffer (or (plist-get args :buffer) (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when my/gptel-prompt-size-guard-enabled
          (let ((size (my/gptel--request-prompt-size prompt args)))
            (cond
             ((>= size my/gptel--prompt-size-hard-chars)
              (user-error "%s"
                          (my/gptel--prompt-size-warning
                           size my/gptel--prompt-size-hard-chars "Hard abort")))
             ((>= size my/gptel--prompt-size-soft-chars)
              (let ((warning (my/gptel--prompt-size-warning
                              size my/gptel--prompt-size-soft-chars "Warning")))
                (unless (yes-or-no-p (concat warning "\n\nSend anyway? "))
                  (user-error "%s" warning)))))))))))

(advice-remove 'gptel-request #'my/gptel--prompt-size-preflight)
(advice-add 'gptel-request :before #'my/gptel--prompt-size-preflight)

(defun my/gptel--project-context-string ()
  "Return a formatted project-context block for agentic system prompts.
Injected into system messages at request-send time via the preset lambda."
  (let* ((root (condition-case nil
                   (abbreviate-file-name (projectile-project-root))
                 (error (abbreviate-file-name default-directory))))
         (buf-name (buffer-name))
         (buf-file (buffer-file-name)))
    (concat "<project-context>\n"
            "Project root: " root "\n"
            "Active buffer: " buf-name
            (when buf-file (concat " (" (abbreviate-file-name buf-file) ")"))
            "\n</project-context>")))

(defconst my/gptel--agent-read-system
  "You are an Emacs coding assistant in read-only research mode.

TOOLS: search, read, and analyse only -- no edits, no shell.

TOOL SELECTION: prefer the native Emacs/project tools. They are faster, keep
context compact, and avoid wasting tokens on shell transcript boilerplate.

WORKFLOW:
1. Use list_project_files or search_project to discover relevant files.
2. Use read_file to read (full file first; paginate with start-line/end-line only for re-reads).
3. Use show_buffer_context to zoom in on a specific line range.
4. Use search_buffer_text and search_project for cross-referencing symbols.
5. Use git_status / git_diff to understand recent changes.
6. Use check_parens or buffer_diagnostics for static analysis.

PARALLELIZE: when reads are independent (e.g. reading several files), issue them in a single message.

[project_context]"
  "System prompt for the `agent-read' preset.")

(defconst my/gptel--agent-edit-system
  "You are an Emacs coding assistant operating directly on the user's open buffers.

TOOLS: native Emacs file/search/edit tools only -- no shell.

TOOL SELECTION: prefer the native Emacs/project tools. They are faster, keep
context compact, and avoid wasting tokens on shell transcript boilerplate.

CRITICAL: edit_buffer auto-saves the buffer after each edit (unless
no_save=true).  Use save_buffer explicitly only when deliberately batching
several no_save=true edits before a single save.

WORKFLOW:
1. Use list_project_files or search_project to discover relevant files.
2. Use read_file (full file first!) or show_buffer_context before editing.
3. Use delegate_agent with subagent_type=agent-read for independent, context-cheap research.
4. Use open_file to load a file into a live buffer; pass the returned buffer name to edit_buffer.
5. Prefer edit_buffer hashline edits: copy start_line/end_line tags like `42:abc` from read_file output and put the replacement in new_str.
   - Hashline edits verify the current line hash, recover nearby shifted lines, and fail safely with refreshed context if stale.
   - For single-line edits, pass start_line only; for ranges, pass both start_line and end_line.
   - In hashline mode, old_str can be the empty string.
   - Use exact old_str replacement only as a fallback when hashline tags are unavailable.
   - In fallback mode, old_str must uniquely match the target text; pass replace_all=true only when intentionally replacing every occurrence.
6. Use check_parens / byte_compile_file / buffer_diagnostics to verify after edits.
7. Use git_status / git_diff to review your changes before finishing.

PARALLELIZE: when reads are independent, issue them in a single message.

[project_context]"
  "System prompt for the `agent-edit' preset.")

(defconst my/gptel--agent-shell-system
  "You are an Emacs coding assistant with full shell access.

TOOL SELECTION: native Emacs tools are the default. They are faster, keep
context compact, and avoid wasting tokens on shell transcript boilerplate.
Do not shell out for routine file discovery, file reading, or text search.

CRITICAL: edit_buffer auto-saves the buffer after each edit (unless
no_save=true).  Use save_buffer explicitly only when deliberately batching
several no_save=true edits before a single save.

WORKFLOW:
1. Use list_project_files or search_project to discover relevant files.
2. Use read_file (full file first!) or show_buffer_context before editing.
3. Use delegate_agent with subagent_type=agent-read for independent, context-cheap research.
4. Use open_file to load a file into a live buffer; pass the returned buffer name to edit_buffer.
5. Prefer edit_buffer hashline edits: copy start_line/end_line tags like `42:abc` from read_file output and put the replacement in new_str.
   - Hashline edits verify the current line hash, recover nearby shifted lines, and fail safely with refreshed context if stale.
   - In hashline mode, old_str can be the empty string.
   - Use exact old_str replacement only as a fallback when hashline tags are unavailable.
6. Use check_parens / byte_compile_file / buffer_diagnostics to verify after edits.
7. Reserve run_shell_command for commands that genuinely need an external process:
   tests, builds, linters/formatters, package managers, one-off system inspection,
   or git operations not covered by git_status/git_diff.
   NEVER use run_shell_command for ls/find/tree/pwd/cat/head/tail/sed/grep/rg-style work:
   use list_project_files / read_file / show_buffer_context / search_project / search_buffer_text.
8. Use git_status / git_diff to review your changes before finishing.

PARALLELIZE: when operations are independent, issue them in a single message.

[project_context]"
  "System prompt for the `agent-shell' preset.")

;; --- Agentic presets ---

(gptel-make-preset 'agent-read
  :description "Read-only agent: search, read, plan, review. No edits, no shell."
  :pre #'my/gptel--agent-preset-preflight-setup
  :backend "Claude"
  :model 'claude-sonnet-4-6
  :system (lambda ()
            (string-replace "[project_context]"
                            (my/gptel--project-context-string)
                            my/gptel--agent-read-system))
  :tools '("read_file" "show_buffer_context" "search_buffer_text"
           "search_project" "list_project_files" "list_buffers"
           "git_status" "git_diff" "buffer_diagnostics" "check_parens"))

(gptel-make-preset 'agent-edit
  :description "Full-edit agent: read, modify buffers, create files, verify. No shell."
  :pre #'my/gptel--agent-edit-preflight-setup
  :backend "Claude"
  :model 'claude-sonnet-4-6
  :system (lambda ()
            (string-replace "[project_context]"
                            (my/gptel--project-context-string)
                            my/gptel--agent-edit-system))
  :tools '("read_file" "show_buffer_context" "search_buffer_text"
           "search_project" "list_project_files" "list_buffers"
           "delegate_agent"
            "open_file" "edit_buffer" "save_buffer"
           "create_file" "overwrite_file" "indent_region"
           "check_parens" "byte_compile_file" "buffer_diagnostics"
           "git_status" "git_diff"))

(gptel-make-preset 'agent-shell
  :description "Shell-capable agent: full edit plus arbitrary shell commands."
  :parents 'agent-edit
  :system (lambda ()
            (string-replace "[project_context]"
                            (my/gptel--project-context-string)
                            my/gptel--agent-shell-system))
  :tools '(:append ("run_shell_command")))

(gptel-make-preset 'git-review
  :description "Git review agent: status and diff only -- no edits, no file reads."
  :backend "Claude"
  :model 'claude-sonnet-4-6
  :system "You are a code reviewer.

WORKFLOW:
1. Start by calling `git_status` to see the working-tree state.
2. Call `git_diff` (and `git_diff` with staged=true if anything is staged) to fetch the actual changes -- do NOT wait for the user to paste output.
3. Issue independent git_status / git_diff calls in parallel in a single message when possible.
4. Then provide concise, actionable feedback: what changed, what looks risky, what's missing (tests, docs, edge cases)."
  :tools '("git_status" "git_diff"))

(my/gptel--update-subagent-tool-enum)

;; GPTel config block end

;; Integrations
(require 'gptel-integrations)

(provide 'init-gptel)
