;;; init-gptel.el --- GPTel configuration (extracted from emacs_asahi)  -*- lexical-binding: t; -*-
; Created on: Sat 13 Sep 2025
; Last Updated: Wed 27 May 2026

;;; GPTel specific configurations ;;;
;;
(require 'gptel)
(require 'json)
(require 'bytecomp)
(require 'cl-lib)
(require 'seq)


(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function gptel--display-tool-calls "gptel" (tool-calls info &optional use-minibuffer))
(declare-function magit-git-insert "magit-git" (&rest args))
(declare-function projectile-project-files "projectile" (&optional project-root))
(declare-function projectile-project-root "projectile" (&optional dir))
(declare-function beads-client-list "beads-client" (&optional filters))
(declare-function beads-client-show "beads-client" (id))
(declare-function beads-client-ready "beads-client" (&optional filters))
(declare-function byte-compile-dest-file "bytecomp" (filename))

(defvar beads-dolt-sql-list-lite)

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
  :request-params '(:thinking (:type "disabled"))
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
      (error "No buffer for '%s'. Use buffer_edit operation=open to open it first." name-or-path)))

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

(defconst my/gptel--hashline-stale-context-radius 3
  "Number of lines around a stale hashline target to include in errors.
The resolver may scan a wider window for safe automatic recovery, but stale
failure output stays intentionally small to avoid replaying large context
blocks into gptel transcripts.")

(defun my/gptel--stale-hashline-context-range (target-line total)
  "Return (START . END) for compact context around TARGET-LINE of TOTAL."
  (let* ((line (min total (max 1 target-line)))
         (start (max 1 (- line my/gptel--hashline-stale-context-radius)))
         (end (min total (+ line my/gptel--hashline-stale-context-radius))))
    (cons start end)))

(defun my/gptel--stale-hashline-reread-hint (target-line total)
  "Return concise retry guidance for a stale hashline near TARGET-LINE."
  (if (zerop total)
      "No edit was made. The buffer is now empty; re-read it and retry with fresh hashline tags."
    (let* ((range (my/gptel--stale-hashline-context-range target-line total))
           (start (car range))
           (end (cdr range)))
      (format "No edit was made. Re-read a narrow range around the stale line (workspace operation=read_file start-line=%d end-line=%d), then retry with fresh hashline tags."
              start end))))

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
    (error "%s must be a line tag like '42:abc', copied from workspace operation=read_file output" label))
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
    (unless (and (>= target-line 1) (<= target-line total))
      (error "%s tag %s points to line %d, but the buffer now has %d line%s. %s"
             label tag target-line total (if (= total 1) "" "s")
             (my/gptel--stale-hashline-reread-hint target-line total)))
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
        (error "%s tag %s no longer matches line %d, and hash %s appears multiple times nearby at lines %s. %s"
               label tag target-line target-hash
               (mapconcat #'number-to-string matches ", ")
               (my/gptel--stale-hashline-reread-hint target-line total)))
       (t
        (let* ((context-range (my/gptel--stale-hashline-context-range target-line total))
               (context-start (car context-range))
               (context-end (cdr context-range)))
          (error "%s tag %s no longer matches line %d, and hash %s was not found within +/- %d lines. %s\n\n%s"
                 label tag target-line target-hash my/gptel--hashline-displacement-window
                 (my/gptel--stale-hashline-reread-hint target-line total)
                 (my/gptel--render-hashed-lines
                  (format "Current bounded context around stale tag %s" tag)
                  context-start context-end))))))))

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
START-LINE and END-LINE are workspace operation=read_file tags like
\"42:abc\".  If a tag has shifted, scan a small nearby window for the same
hash before editing."
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
Use buffer_edit operation=indent afterwards if reformatting is needed.
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
    ("check_parens" . 4000)
    ("byte_compile_file" . 12000)
    ("verify_task" . 12000)
    ("fetch_tool_output" . 12000)
    ("search_tool_output" . 12000)
    ("beads_list_issues" . 12000)
    ("beads_ready_issues" . 12000)
    ("beads_search_issues" . 12000)
    ("beads_show_issue" . 16000)
    ("remember" . 4000)
    ("list_skills" . 8000)
    ("search_skills" . 12000)
    ("load_skill" . 12000)
    ("save_skill" . 4000)
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
diagnostics, command output, secrets, or local paths.  This directory is a
local private cache, not a project artifact: keep it outside project repos,
do not sync it into `pj-gtd-org', and clean it periodically with
`my/gptel-clean-tool-output-store'."
  :type 'directory
  :group 'gptel)

(defcustom my/gptel--tool-output-retention-days 14
  "Default maximum age in days for gptel side-car tool output objects.
`my/gptel-clean-tool-output-store' deletes `.txt' objects and `.json'
manifests older than this threshold.  Set to nil to disable age-based cleanup
unless an explicit DAYS argument is provided."
  :type '(choice (const :tag "Disable age cleanup" nil) integer)
  :group 'gptel)

(defun my/gptel--tool-output-store-path (hash suffix)
  "Return side-car store path for object HASH with file SUFFIX."
  (expand-file-name (concat hash suffix)
                    (expand-file-name (substring hash 0 2)
                                      my/gptel--tool-output-store-directory)))

(defun my/gptel--tool-output-files ()
  "Return side-car `.txt' object and `.json' manifest files."
  (when (file-directory-p my/gptel--tool-output-store-directory)
    (directory-files-recursively
     my/gptel--tool-output-store-directory
     "\\.\\(txt\\|json\\)\\'")))

(defun my/gptel--tool-output-old-p (path cutoff)
  "Return non-nil when PATH was last modified before CUTOFF time."
  (time-less-p (file-attribute-modification-time (file-attributes path)) cutoff))

(defun my/gptel--delete-empty-tool-output-dirs ()
  "Delete empty sharded directories under the gptel tool-output store."
  (when (file-directory-p my/gptel--tool-output-store-directory)
    (dolist (dir (directory-files my/gptel--tool-output-store-directory t "\`[^.]"))
      (when (and (file-directory-p dir)
                 (null (directory-files dir nil "\`[^.]")))
        (delete-directory dir)))))

(defun my/gptel-clean-tool-output-store (&optional days dry-run)
  "Delete side-car tool-output files older than DAYS.
DAYS defaults to `my/gptel--tool-output-retention-days'.  With prefix arg,
prompt for DAYS.  When DRY-RUN is non-nil, report candidates without deleting.

Privacy policy: this store is local-only and may contain secrets, source code,
logs, diagnostics, and command output.  Raw objects are intentionally kept out
of gptel chat transcripts and out of `pj-gtd-org' org files; use this command
to remove stale local copies."
  (interactive
   (list (when current-prefix-arg
           (read-number "Delete gptel tool outputs older than days: "
                        (or my/gptel--tool-output-retention-days 14)))
         nil))
  (let* ((days (or days my/gptel--tool-output-retention-days))
         (files (my/gptel--tool-output-files)))
    (unless days
      (error "No retention period configured; pass DAYS or set my/gptel--tool-output-retention-days"))
    (let* ((cutoff (time-subtract (current-time) (days-to-time days)))
           (old-files (seq-filter (lambda (path)
                                    (my/gptel--tool-output-old-p path cutoff))
                                  files))
           (deleted 0))
      (dolist (path old-files)
        (unless dry-run
          (delete-file path)
          (setq deleted (1+ deleted))))
      (unless dry-run
        (my/gptel--delete-empty-tool-output-dirs))
      (let ((message (format "%s %d gptel side-car tool-output file%s older than %d day%s in %s. These files may contain sensitive local data."
                             (if dry-run "Would delete" "Deleted")
                             (if dry-run (length old-files) deleted)
                             (if (= (length old-files) 1) "" "s")
                             days
                             (if (= days 1) "" "s")
                             (abbreviate-file-name my/gptel--tool-output-store-directory))))
        (when (called-interactively-p 'interactive)
          (message "%s" message))
        message))))

(defun my/gptel--tool-output-id-hash (id)
  "Return the sha256 hash portion of stored tool output ID."
  (let ((hash (string-remove-prefix "sha256:" (string-trim id))))
    (unless (and (= (length hash) 64)
                 (not (string-match-p "[^[:xdigit:]]" hash)))
      (error "Invalid tool output id '%s'; expected sha256:<64 hex chars>" id))
    hash))

(defun my/gptel--tool-output-object-path (id)
  "Return existing object path for stored tool output ID."
  (let* ((hash (my/gptel--tool-output-id-hash id))
         (path (my/gptel--tool-output-store-path hash ".txt")))
    (unless (file-readable-p path)
      (error "Stored tool output not found for sha256:%s under %s"
             hash (abbreviate-file-name my/gptel--tool-output-store-directory)))
    path))

(defun my/gptel--read-stored-output-lines (id start-line end-line)
  "Return bounded lines START-LINE through END-LINE from stored output ID."
  (let* ((path (my/gptel--tool-output-object-path id))
         (start-line (round start-line))
         (end-line (round end-line)))
    (when (or (< start-line 1) (< end-line start-line))
      (error "Invalid line range %d-%d; use positive 1-based inclusive lines"
             start-line end-line))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((total-lines (count-lines (point-min) (point-max))))
        (when (> end-line total-lines)
          (setq end-line total-lines))
        (if (> start-line total-lines)
            `((path . ,path)
              (total_lines . ,total-lines)
              (range . ,(format "%d-%d" start-line end-line))
              (text . ""))
          (goto-char (point-min))
          (forward-line (1- start-line))
          (let ((start (point)))
            (forward-line (1+ (- end-line start-line)))
            `((path . ,path)
              (total_lines . ,total-lines)
              (range . ,(format "%d-%d" start-line end-line))
              (text . ,(buffer-substring-no-properties start (point))))))))))

(defun my/gptel--tool-output-metadata-json (tool metadata hash body summary)
  "Return JSON manifest text for stored TOOL output BODY with HASH and SUMMARY."
  (let ((record `((id . ,(concat "sha256:" hash))
                  (tool . ,tool)
                  (stored_at . ,(format-time-string "%FT%TZ" (current-time) t))
                  (bytes . ,(string-bytes body))
                  (chars . ,(length body))
                  (lines . ,(with-temp-buffer
                              (insert body)
                              (count-lines (point-min) (point-max))))
                  (content_type . "text/plain; charset=utf-8")
                  (summary . ,summary)
                  (scope . ,(mapcar
                             (lambda (entry)
                               (cons (format "%s" (car entry))
                                     (my/gptel--format-metadata-value (cdr entry))))
                             metadata)))))
    (concat (json-encode record) "\n")))

(defun my/gptel--store-tool-output (tool metadata body summary)
  "Store oversized TOOL output BODY with SUMMARY and return reference metadata."
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
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file manifest-path
        (insert (my/gptel--tool-output-metadata-json tool metadata hash body summary))))
    `((id . ,(concat "sha256:" hash))
      (tool . ,tool)
      (bytes . ,(string-bytes body))
      (chars . ,(length body))
      (lines . ,lines)
      (object_path . ,(abbreviate-file-name object-path))
      (manifest_path . ,(abbreviate-file-name manifest-path)))))

(defun my/gptel--tool-output-reference-block (reference cap-chars summary)
  "Return an org-friendly reference block for stored output REFERENCE and SUMMARY."
  (format ":TOOL_OUTPUT_REF:\n:id %s\n:tool %s\n:bytes %s\n:chars %s\n:lines %s\n:object_path %s\n:manifest_path %s\n:summary %s\n:END:"
          (alist-get 'id reference)
          (alist-get 'tool reference)
          (alist-get 'bytes reference)
          (alist-get 'chars reference)
          (alist-get 'lines reference)
          (alist-get 'object_path reference)
          (alist-get 'manifest_path reference)
          (format "Output exceeded cap_chars=%d and was stored out-of-band. %s Treat the stored object as sensitive local data."
                  cap-chars summary)))

(defun my/gptel--summary-lines (body count &optional from-end)
  "Return up to COUNT non-empty BODY lines as a compact summary string.
When FROM-END is non-nil, return lines from the end of BODY."
  (let* ((lines (split-string body "\n" t "[[:space:]]+"))
         (selected (if from-end
                       (last lines (min count (length lines)))
                     (cl-subseq lines 0 (min count (length lines))))))
    (string-join
     (mapcar (lambda (line)
               (truncate-string-to-width (string-trim line) 180 nil nil t))
             selected)
     " | ")))

(defun my/gptel--metadata-summary (metadata keys)
  "Return compact KEY=VALUE summary entries from METADATA for KEYS."
  (mapconcat
   #'identity
   (delq nil
         (mapcar (lambda (key)
                   (let ((value (my/gptel--format-metadata-value (alist-get key metadata))))
                     (when value (format "%s=%s" key value))))
                 keys))
   ", "))

(defun my/gptel--tool-output-summary (tool metadata body reference max-chars)
  "Return deterministic compact summary for oversized TOOL result.
The summary is rule-based; it does not call an LLM or invent facts."
  (let* ((id (alist-get 'id reference))
         (base (format "stored_ref=%s, cap_chars=%d, body_chars=%d"
                       id max-chars (length body)))
         (meta (pcase tool
                 ("read_file"
                  (my/gptel--metadata-summary metadata '(target range total_lines requested_end)))
                 ((or "search_project" "search_buffer_text" "search_tool_output")
                  (my/gptel--metadata-summary metadata '(target query match_count shown_count max_matches total_lines)))
                 ("run_shell_command"
                  (my/gptel--metadata-summary metadata '(command exit_code output_chars)))
                 ("git_diff"
                  (my/gptel--metadata-summary metadata '(target staged diff_chars file_count)))
                 ("buffer_diagnostics"
                  (my/gptel--metadata-summary metadata '(target flymake_active diagnostic_count severity_counts)))
                 (_
                  (my/gptel--metadata-summary metadata '(target scope range total_lines match_count shown_count exit_code)))))
         (sample (pcase tool
                   ("run_shell_command"
                    (format "first_lines=[%s], last_lines=[%s]"
                            (my/gptel--summary-lines body 3)
                            (my/gptel--summary-lines body 3 t)))
                   ((or "search_project" "search_buffer_text" "search_tool_output" "buffer_diagnostics")
                    (format "first_hits=[%s]" (my/gptel--summary-lines body 5)))
                   ("git_diff"
                    (format "diff_headers=[%s]"
                            (let ((headers (seq-filter
                                            (lambda (line)
                                              (string-match-p "\`\(diff --git\|+++ \|--- \|@@ \)" line))
                                            (split-string body "\n" t))))
                              (mapconcat #'identity
                                         (cl-subseq headers 0 (min 20 (length headers)))
                                         " | "))))
                   (_ nil))))
    (string-join (delq nil (list base meta sample)) "; ")))

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
         (summary nil)
         (reference (when truncated
                      (let ((reference (my/gptel--store-tool-output tool metadata body "pending summary")))
                        (setq summary (my/gptel--tool-output-summary tool metadata body reference max-chars))
                        (my/gptel--store-tool-output tool metadata body summary))))
         (shown-body (if truncated
                         (concat "Deterministic summary:\n"
                                 summary
                                 "\n\n"
                                 (my/gptel--tool-output-reference-block reference max-chars summary))
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
                                  (summary . ,summary)
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

(defconst my/gptel--beads-source-roots
  (mapcar #'expand-file-name
          '("~/Documents/repos/personal/beads.el"
            "~/Documents/repos/personal/beads.el-gojun077"
            "~/Documents/repos/personal/beads-turbo.el"))
  "Candidate checkouts containing beads.el, used by the Beads gptel tool.")

(defconst my/gptel--beads-source-root
  (or (seq-find #'file-directory-p my/gptel--beads-source-roots)
      (car my/gptel--beads-source-roots))
  "Selected checkout containing beads.el, used by the Beads gptel tool.")

(defconst my/gptel--beads-default-page-size 50
  "Default number of Beads issue summaries returned by list-like tools.")

(defconst my/gptel--beads-max-page-size 100
  "Maximum Beads issue summaries returned by one list-like tool call.")

(defun my/gptel--ensure-beads-client ()
  "Ensure the beads.el client API is available."
  (let ((lisp-dir (expand-file-name "lisp" my/gptel--beads-source-root)))
    (when (file-directory-p lisp-dir)
      (add-to-list 'load-path lisp-dir))
    (require 'beads-client nil t)
    (require 'beads-backend-dolt-sql nil t)
    (unless (fboundp 'beads-client-list)
      (error "beads-client is unavailable; expected beads.el under %s"
             my/gptel--beads-source-root))))

(defun my/gptel--beads-project-root (project-root)
  "Return a Beads project root for PROJECT-ROOT or `default-directory'."
  (file-name-as-directory
   (expand-file-name
    (or (and (stringp project-root) (not (string-empty-p project-root)) project-root)
        (or (ignore-errors (projectile-project-root)) default-directory)))))

(defun my/gptel--beads-with-project (project-root thunk)
  "Call THUNK with `default-directory' bound to PROJECT-ROOT."
  (my/gptel--ensure-beads-client)
  (let ((default-directory (my/gptel--beads-project-root project-root)))
    (funcall thunk)))

(defun my/gptel--beads-normalize-sequence (value)
  "Return VALUE as a plain list when it is a sequence-like JSON value."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun my/gptel--beads-field (issue field)
  "Return ISSUE alist FIELD, accepting symbol or string keys."
  (or (alist-get field issue)
      (alist-get (symbol-name field) issue nil nil #'string=)))

(defun my/gptel--beads-format-scalar (value)
  "Format VALUE as a compact scalar string."
  (cond
   ((null value) "")
   ((eq value t) "true")
   ((memq value '(:json-false :false)) "false")
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   (t (format "%s" value))))

(defun my/gptel--beads-truthy-p (value)
  "Return non-nil when VALUE is a non-false JSON/gptel boolean value."
  (and value (not (memq value '(:json-false :false)))))

(defun my/gptel--beads-format-list (value)
  "Format VALUE as a comma-separated list."
  (mapconcat #'my/gptel--beads-format-scalar
             (my/gptel--beads-normalize-sequence value)
             ", "))

(defun my/gptel--beads-split-filter (value)
  "Split comma-separated Beads filter VALUE into non-empty strings."
  (when (and (stringp value) (not (string-empty-p (string-trim value))))
    (seq-filter
     (lambda (part) (not (string-empty-p part)))
     (mapcar #'string-trim (split-string value "," t)))))

(defun my/gptel--beads-labels-match-p (issue labels)
  "Return non-nil when ISSUE has every comma-separated label in LABELS."
  (let ((wanted (my/gptel--beads-split-filter labels)))
    (or (null wanted)
        (let ((present (mapcar #'my/gptel--beads-format-scalar
                               (my/gptel--beads-normalize-sequence
                                (my/gptel--beads-field issue 'labels)))))
          (seq-every-p (lambda (label)
                         (member label present))
                       wanted)))))

(defun my/gptel--beads-priority-match-p (issue priority)
  "Return non-nil when ISSUE matches optional PRIORITY."
  (or (null priority)
      (let ((issue-priority (my/gptel--beads-field issue 'priority)))
        (if (and (numberp priority) (numberp issue-priority))
            (= (round priority) (round issue-priority))
          (string= (my/gptel--beads-format-scalar priority)
                   (my/gptel--beads-format-scalar issue-priority))))))

(defun my/gptel--beads-issue-matches-p (issue status priority type assignee labels all)
  "Return non-nil when ISSUE matches the Beads list/search filters."
  (let* ((issue-status (downcase (my/gptel--beads-format-scalar
                                  (my/gptel--beads-field issue 'status))))
         (statuses (mapcar #'downcase (my/gptel--beads-split-filter status)))
         (issue-type (downcase (my/gptel--beads-format-scalar
                                (or (my/gptel--beads-field issue 'type)
                                    (my/gptel--beads-field issue 'issue_type)))))
         (wanted-type (and (stringp type) (downcase (string-trim type))))
         (issue-assignee (my/gptel--beads-format-scalar
                          (my/gptel--beads-field issue 'assignee))))
    (and (or statuses (my/gptel--beads-truthy-p all)
             (not (string= issue-status "closed")))
         (or (null statuses) (member issue-status statuses))
         (my/gptel--beads-priority-match-p issue priority)
         (or (null wanted-type) (string-empty-p wanted-type)
             (string= issue-type wanted-type))
         (or (null assignee) (string-empty-p assignee)
             (string= issue-assignee assignee))
         (my/gptel--beads-labels-match-p issue labels))))

(defun my/gptel--beads-filter-issues (issues status priority type assignee labels all)
  "Return ISSUES matching list/search filters."
  (seq-filter
   (lambda (issue)
     (my/gptel--beads-issue-matches-p
      issue status priority type assignee labels all))
   issues))

(defun my/gptel--beads-issue-summary (issue)
  "Return one compact line summarizing ISSUE."
  (format "%s [%s P%s %s] %s"
          (my/gptel--beads-format-scalar (my/gptel--beads-field issue 'id))
          (my/gptel--beads-format-scalar (my/gptel--beads-field issue 'status))
          (my/gptel--beads-format-scalar (my/gptel--beads-field issue 'priority))
          (my/gptel--beads-format-scalar (or (my/gptel--beads-field issue 'type)
                                             (my/gptel--beads-field issue 'issue_type)))
          (my/gptel--beads-format-scalar (my/gptel--beads-field issue 'title))))

(defun my/gptel--beads-issue-detail (issue)
  "Return a readable bounded detail view for ISSUE."
  (let ((sections
         `(("ID" . ,(my/gptel--beads-field issue 'id))
           ("Title" . ,(my/gptel--beads-field issue 'title))
           ("Status" . ,(my/gptel--beads-field issue 'status))
           ("Type" . ,(or (my/gptel--beads-field issue 'type)
                          (my/gptel--beads-field issue 'issue_type)))
           ("Priority" . ,(my/gptel--beads-field issue 'priority))
           ("Assignee" . ,(my/gptel--beads-field issue 'assignee))
           ("Labels" . ,(my/gptel--beads-format-list (my/gptel--beads-field issue 'labels)))
           ("Description" . ,(my/gptel--beads-field issue 'description))
           ("Design" . ,(my/gptel--beads-field issue 'design))
           ("Acceptance" . ,(or (my/gptel--beads-field issue 'acceptance_criteria)
                                (my/gptel--beads-field issue 'acceptance)))
           ("Notes" . ,(my/gptel--beads-field issue 'notes))
           ("Created" . ,(my/gptel--beads-field issue 'created_at))
           ("Updated" . ,(my/gptel--beads-field issue 'updated_at))
           ("Closed" . ,(my/gptel--beads-field issue 'closed_at)))))
    (string-join
     (delq nil
           (mapcar (lambda (section)
                     (let ((value (my/gptel--beads-format-scalar (cdr section))))
                       (unless (string-empty-p value)
                         (format "%s: %s" (car section) value))))
                   sections))
     "\n")))

(defun my/gptel--beads-filter-plist (status priority type assignee labels all)
  "Build a beads-client filter plist from scalar tool arguments."
  (let (filters)
    (when (my/gptel--beads-truthy-p all)
      (setq filters (plist-put filters :all t)))
    (when (and status (not (string-empty-p status)))
      (setq filters (plist-put filters :status status)))
    (when priority
      (setq filters (plist-put filters :priority priority)))
    (when (and type (not (string-empty-p type)))
      (setq filters (plist-put filters :issue-type type)))
    (when (and assignee (not (string-empty-p assignee)))
      (setq filters (plist-put filters :assignee assignee)))
    (when (and labels (not (string-empty-p labels)))
      (setq filters (plist-put filters :labels labels)))
    filters))

(defun my/gptel--beads-ready-filter-plist (assignee priority)
  "Build a beads-client ready filter plist from scalar tool arguments."
  (let ((filters (list :limit 0)))
    (when (and assignee (not (string-empty-p assignee)))
      (setq filters (plist-put filters :assignee assignee)))
    (when priority
      (setq filters (plist-put filters :priority priority)))
    filters))

(defun my/gptel--beads-list-raw (filters &optional include-details)
  "Return Beads issues for FILTERS, optionally asking SQL list for DETAILS."
  (my/gptel--beads-normalize-sequence
   (if (and include-details (boundp 'beads-dolt-sql-list-lite))
       (let ((beads-dolt-sql-list-lite nil))
         (beads-client-list filters))
     (beads-client-list filters))))

(defun my/gptel--beads-page-size (limit)
  "Return a safe Beads page size from LIMIT."
  (min my/gptel--beads-max-page-size
       (max 1 (round (or limit my/gptel--beads-default-page-size)))))

(defun my/gptel--beads-page-offset (offset)
  "Return a safe zero-based Beads page OFFSET."
  (max 0 (round (or offset 0))))

(defun my/gptel--beads-page (items limit offset)
  "Return pagination metadata for ITEMS using LIMIT and OFFSET."
  (let* ((page-size (my/gptel--beads-page-size limit))
         (page-offset (my/gptel--beads-page-offset offset))
         (total (length items))
         (end (min total (+ page-offset page-size)))
         (shown (when (< page-offset total)
                  (cl-subseq items page-offset end))))
    (list :items shown
          :total total
          :offset page-offset
          :limit page-size
          :end end
          :next-offset (when (< end total) end))))

(defun my/gptel--beads-format-page (heading page formatter empty-message)
  "Format PAGE of Beads issues using HEADING, FORMATTER and EMPTY-MESSAGE."
  (let ((total (plist-get page :total))
        (offset (plist-get page :offset))
        (end (plist-get page :end))
        (next-offset (plist-get page :next-offset))
        (shown (plist-get page :items)))
    (cond
     ((zerop total) empty-message)
     ((null shown)
      (format "Offset %d is past the last result (%d total). Retry with offset=0 or a smaller offset."
              offset total))
     (t
      (format "%s (showing %d-%d of %d%s):\n%s"
              heading
              (1+ offset)
              end
              total
              (if next-offset
                  (format "; next offset=%d" next-offset)
                "")
              (mapconcat formatter shown "\n"))))))

(defun my/gptel--beads-next-hint (operation page)
  "Return next-step guidance for Beads OPERATION and pagination PAGE."
  (if-let ((next-offset (plist-get page :next-offset)))
      (format "More results available; repeat beads operation=%s with offset=%d and limit=%d, or narrow filters/search. Use operation=show with an issue id for full details."
              operation next-offset (plist-get page :limit))
    "Use beads operation=show with an issue id for full details."))

(defun my/gptel--beads-page-metadata (page)
  "Return compact metadata entries for pagination PAGE."
  `((issue_count . ,(plist-get page :total))
    (shown_count . ,(length (plist-get page :items)))
    (offset . ,(plist-get page :offset))
    (limit . ,(plist-get page :limit))
    (has_more . ,(if (plist-get page :next-offset) "yes" "no"))
    (next_offset . ,(plist-get page :next-offset))))

(defun my/gptel--beads-list-issues (&optional project-root status priority type assignee labels limit all offset)
  "Return compact Beads issue summaries for PROJECT-ROOT and filters."
  (condition-case err
      (my/gptel--beads-with-project
       project-root
       (lambda ()
         (let* ((filters (my/gptel--beads-filter-plist
                          status priority type assignee labels all))
                (issues (my/gptel--beads-filter-issues
                         (my/gptel--beads-list-raw filters)
                         status priority type assignee labels all))
                (page (my/gptel--beads-page issues limit offset)))
           (my/gptel--format-tool-result
            "beads_list_issues"
            `((project_root . ,default-directory)
              ,@(my/gptel--beads-page-metadata page))
            (my/gptel--beads-format-page
             "Beads issues"
             page
             #'my/gptel--beads-issue-summary
             "No issues matched.")
            (my/gptel--beads-next-hint "list" page)))))
    (error (format "Error listing Beads issues: %s" (error-message-string err)))))

(defun my/gptel--beads-show-issue (id &optional project-root)
  "Return one Beads issue ID from PROJECT-ROOT."
  (condition-case err
      (my/gptel--beads-with-project
       project-root
       (lambda ()
         (let ((issue (beads-client-show id)))
           (my/gptel--format-tool-result
            "beads_show_issue"
            `((project_root . ,default-directory)
              (issue_id . ,id))
            (my/gptel--beads-issue-detail issue)
            "Use beads operation=list/search/ready to find related or next issues."))))
    (error (format "Error showing Beads issue '%s': %s" id (error-message-string err)))))

(defun my/gptel--beads-ready-issues (&optional project-root assignee priority limit offset)
  "Return unblocked/ready Beads issues for PROJECT-ROOT and filters."
  (condition-case err
      (my/gptel--beads-with-project
       project-root
       (lambda ()
         (let* ((filters (my/gptel--beads-ready-filter-plist assignee priority))
                (issues (my/gptel--beads-normalize-sequence
                         (beads-client-ready filters)))
                (shown-issues (my/gptel--beads-filter-issues
                               issues nil priority nil assignee nil t))
                (page (my/gptel--beads-page shown-issues limit offset)))
           (my/gptel--format-tool-result
            "beads_ready_issues"
            `((project_root . ,default-directory)
              ,@(my/gptel--beads-page-metadata page))
            (my/gptel--beads-format-page
             "Ready Beads issues"
             page
             #'my/gptel--beads-issue-summary
             "No ready issues matched.")
            (my/gptel--beads-next-hint "ready" page)))))
    (error (format "Error listing ready Beads issues: %s" (error-message-string err)))))

(defun my/gptel--beads-issue-search-text (issue)
  "Return searchable summary/detail text for ISSUE."
  (string-join
   (delq nil
         (list (my/gptel--beads-issue-summary issue)
               (my/gptel--beads-format-scalar
                (my/gptel--beads-field issue 'description))
               (my/gptel--beads-format-scalar
                (my/gptel--beads-field issue 'design))
               (my/gptel--beads-format-scalar
                (or (my/gptel--beads-field issue 'acceptance_criteria)
                    (my/gptel--beads-field issue 'acceptance)))
               (my/gptel--beads-format-scalar
                (my/gptel--beads-field issue 'notes))))
   "\n"))

(defun my/gptel--beads-search-issues (query &optional project-root status priority type assignee labels limit all offset)
  "Search Beads issue summaries/details for literal QUERY."
  (condition-case err
      (my/gptel--beads-with-project
       project-root
       (lambda ()
         (when (or (null query) (string-empty-p (string-trim query)))
           (error "query must not be empty"))
         (let* ((filters (my/gptel--beads-filter-plist
                          status priority type assignee labels all))
                (issues (my/gptel--beads-filter-issues
                         (my/gptel--beads-list-raw filters t)
                         status priority type assignee labels all))
                (pattern (regexp-quote (downcase (string-trim query))))
                (matches (seq-filter
                          (lambda (issue)
                            (string-match-p
                             pattern
                             (downcase (my/gptel--beads-issue-search-text issue))))
                          issues))
                (page (my/gptel--beads-page matches limit offset)))
           (my/gptel--format-tool-result
            "beads_search_issues"
            `((project_root . ,default-directory)
              (query . ,query)
              (searched_issue_count . ,(length issues))
              (match_count . ,(plist-get page :total))
              ,@(my/gptel--beads-page-metadata page))
            (my/gptel--beads-format-page
             (format "Matching Beads issues for %S" query)
             page
             #'my/gptel--beads-issue-summary
             (format "No Beads issues matched %S." query))
            (my/gptel--beads-next-hint "search" page)))))
    (error (format "Error searching Beads issues: %s" (error-message-string err)))))

(defun my/gptel--beads-tool (operation &optional project-root id query status priority type assignee labels limit all offset)
  "Dispatch Beads OPERATION behind one gptel tool declaration."
  (pcase operation
    ("list" (my/gptel--beads-list-issues project-root status priority type assignee labels limit all offset))
    ("show" (if (and id (not (string-empty-p id)))
                (my/gptel--beads-show-issue id project-root)
              "Error: beads operation=show requires id."))
    ("ready" (my/gptel--beads-ready-issues project-root assignee priority limit offset))
    ("search" (my/gptel--beads-search-issues query project-root status priority type assignee labels limit all offset))
    (_ "Error: beads operation must be one of: list, show, ready, search.")))

(defun my/gptel--knowledge-tool (operation &optional scope entry old-entry name-or-file query title when-to-use steps verification caveats max-skills max-matches max-chars replace-existing)
  "Dispatch persistent-memory and skill OPERATION behind one tool declaration."
  (pcase operation
    ("remember" (my/gptel--remember scope entry old-entry))
    ("list_skills" (my/gptel--list-skills max-skills))
    ("search_skills" (my/gptel--search-skills query max-matches))
    ("load_skill" (my/gptel--load-skill name-or-file max-chars))
    ("save_skill" (my/gptel--save-skill title when-to-use steps verification caveats replace-existing))
    (_ "Error: knowledge operation must be one of: remember, list_skills, search_skills, load_skill, save_skill.")))

(defun my/gptel--knowledge-confirm-p (operation &rest _)
  "Return non-nil when knowledge OPERATION mutates durable state."
  (member operation '("remember" "save_skill")))

(defun my/gptel--fetch-tool-output (id start-line end-line)
  "Fetch a bounded line range from stored tool output ID."
  (condition-case err
      (let* ((result (my/gptel--read-stored-output-lines id start-line end-line))
             (path (alist-get 'path result))
             (total-lines (alist-get 'total_lines result))
             (range (alist-get 'range result))
             (text (alist-get 'text result)))
        (my/gptel--format-tool-result
         "fetch_tool_output"
         `((target . ,id)
           (range . ,range)
           (total_lines . ,total-lines)
           (object_path . ,(abbreviate-file-name path)))
         (if (string-empty-p text)
             (format "No stored output lines in requested range %s (total lines: %d)."
                     range total-lines)
           (format "Stored tool output %s, lines %s of %d:\n%s"
                   id range total-lines text))
         "Call tool_output operation=fetch with a different bounded start-line/end-line range, or operation=search for a specific pattern."))
    (error (format "Error fetching stored tool output: %s"
                   (error-message-string err)))))

(defun my/gptel--search-tool-output (id pattern &optional max-matches)
  "Search within stored tool output ID for regex PATTERN."
  (condition-case err
      (let* ((path (my/gptel--tool-output-object-path id))
             (max-matches (min 100 (max 1 (round (or max-matches 25)))))
             (matches nil)
             (match-count 0)
             (total-lines 0))
        (when (string-empty-p pattern)
          (error "pattern must not be empty"))
        (with-temp-buffer
          (insert-file-contents path)
          (setq total-lines (count-lines (point-min) (point-max)))
          (goto-char (point-min))
          (while (re-search-forward pattern nil t)
            (setq match-count (1+ match-count))
            (when (<= match-count max-matches)
              (let ((line-num (line-number-at-pos))
                    (line (string-trim
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))))
                (push (format "Line %d: %s" line-num line) matches)))))
        (my/gptel--format-tool-result
         "search_tool_output"
         `((target . ,id)
           (query . ,pattern)
           (match_count . ,match-count)
           (shown_count . ,(length matches))
           (max_matches . ,max-matches)
           (total_lines . ,total-lines)
           (object_path . ,(abbreviate-file-name path)))
         (if matches
             (format "Found %d match%s for %S in stored output %s%s:\n%s"
                     match-count
                     (if (= match-count 1) "" "es")
                     pattern id
                     (if (> match-count max-matches)
                         (format " (showing first %d)" max-matches)
                       "")
                     (mapconcat #'identity (nreverse matches) "\n"))
           (format "No matches for %S in stored output %s." pattern id))
         "Use tool_output operation=fetch around a reported line number, or rerun operation=search with a more specific pattern/max-matches."))
    (error (format "Error searching stored tool output: %s"
                   (error-message-string err)))))

(defun my/gptel--tool-output-tool (operation id &optional start-line end-line pattern max-matches)
  "Dispatch stored tool-output OPERATION behind one tool declaration."
  (pcase operation
    ("fetch" (my/gptel--fetch-tool-output id start-line end-line))
    ("search" (my/gptel--search-tool-output id pattern max-matches))
    (_ "Error: tool_output operation must be one of: fetch, search.")))

(defun my/gptel--edit-buffer (buffer old-str new-str &optional replace-all no-save start-line end-line)
  "Edit BUFFER using hashline or exact string replacement."
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


(defun my/gptel--show-buffer-context (buffer line-number &optional context-lines)
  "Show CONTEXT-LINES around LINE-NUMBER in BUFFER."
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
           "Call buffer_inspect operation=context with a smaller context-lines value, or workspace operation=read_file with start-line/end-line for a precise range."))))))

(defun my/gptel--search-buffer-text (buffer search-text)
  "Search for literal SEARCH-TEXT in BUFFER."
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
           "If shown_count is lower than match_count, refine search-text or use buffer_inspect operation=context around a reported line."))))))

(defun my/gptel--save-buffer (buffer)
  "Save BUFFER to its visited file."
  (my/gptel--with-buffer-safety (my/gptel--resolve-buffer buffer) "saving buffer"
    (if (buffer-file-name)
        (progn
          (save-buffer)
          (format "Saved buffer '%s' to file: %s" buffer (buffer-file-name)))
      (format "Buffer '%s' is not associated with a file. Use file_write operation=overwrite to save it to a path." buffer))))

(defun my/gptel--read-file (path &optional start-line end-line)
  "Read PATH with hashline tags and bounded pagination."
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
                 "Call workspace operation=read_file again with start-line/end-line for the next page or a narrower range.")))
          (unless existing
            (kill-buffer buf))))
    (error (format "Error reading file '%s': %s" path (error-message-string err)))))

(defun my/gptel--open-file (path)
  "Open PATH in Emacs and return its buffer name."
  (condition-case err
      (let* ((expanded (expand-file-name path))
             (buf (find-file-noselect expanded)))
        (format "Opened '%s' in buffer: %s" expanded (buffer-name buf)))
    (error (format "Error opening file '%s': %s" path (error-message-string err)))))

(defun my/gptel--list-buffers ()
  "List visible Emacs buffers compactly."
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
     "Use a specific buffer name with buffer_inspect or buffer_edit operations.")))

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
  - ls/find/tree/pwd-style file discovery -> workspace operation=list_files
  - cat/head/tail/sed-style file reading -> workspace operation=read_file or buffer_inspect operation=context
  - grep/rg/find+xargs-style text search -> workspace operation=search_project or buffer_inspect operation=search
  - git status/diff -> git operation=status/diff

Reserve this tool for commands that genuinely need an external process: tests, builds, linters/formatters, package managers, one-off system inspection, or git operations not covered by the git dispatcher."
 :args (list '(:name "command"
               :type "string"
               :description "External command to execute. Before using this, prefer workspace/buffer_inspect/git dispatchers when they can answer the question."))
 :confirm t
 :include nil
 :category "shell")


(defun my/gptel--indent-region (buffer start-line end-line)
  "Re-indent BUFFER between START-LINE and END-LINE."
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

(defun my/gptel--check-parens (buffer)
  "Check BUFFER for balanced parentheses and expressions."
  (condition-case err
      (with-current-buffer (my/gptel--resolve-buffer buffer)
        (save-excursion
          (check-parens))
        (format "Parentheses and expressions are balanced in buffer '%s'." buffer))
    (error (format "Unbalanced parentheses detected in buffer '%s': %s"
                   buffer (error-message-string err)))))


(defun my/gptel--byte-compile-file (filename)
  "Byte-compile Emacs Lisp FILENAME and return a compact result."
  (condition-case err
      (progn
        (unless (file-exists-p filename)
          (error "File not found: %s" filename))
        (with-current-buffer (get-buffer-create "*Compile-Log*")
          (let ((inhibit-read-only t))
            (erase-buffer)))
        (let* ((result (byte-compile-file filename))
               (log-output (string-trim
                            (with-current-buffer (get-buffer "*Compile-Log*")
                              (buffer-string))))
               (status (if result "PASS" "FAIL")))
          (my/gptel--format-tool-result
           "byte_compile_file"
           `((target . ,(abbreviate-file-name (expand-file-name filename)))
             (status . ,status)
             (log_chars . ,(length log-output)))
           (if result
               (if (string-empty-p log-output)
                   (format "Byte compilation PASS for '%s' with no compile log output." filename)
                 (format "Byte compilation PASS for '%s' with compile log output:\n%s"
                         filename log-output))
             (if (string-empty-p log-output)
                 (format "Byte compilation FAIL for '%s' with no compile log output." filename)
               (format "Byte compilation FAIL for '%s':\n%s" filename log-output)))
           "If output is truncated or stored, fix the first reported error/warning, rerun byte_compile, or use buffer_inspect operation=verify for the final check.")))
    (error (format "An error occurred while trying to compile '%s': %s"
                   filename (error-message-string err)))))

(defun my/gptel--search-project (pattern &optional dir file-glob case-sensitive)
  "Search project files for ripgrep PATTERN."
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

(defun my/gptel--list-project-files (&optional pattern dir)
  "List project files optionally matching regex PATTERN."
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

(defun my/gptel--workspace-tool (operation &optional path start-line end-line pattern dir file-glob case-sensitive)
  "Dispatch read-only workspace OPERATION behind one tool declaration."
  (pcase operation
    ("read_file" (my/gptel--read-file path start-line end-line))
    ("search_project" (my/gptel--search-project pattern dir file-glob case-sensitive))
    ("list_files" (my/gptel--list-project-files pattern dir))
    (_ "Error: workspace operation must be one of: read_file, search_project, list_files.")))

(defun my/gptel--create-file (path content)
  "Create new file PATH with CONTENT."
  (condition-case err
      (let ((expanded (expand-file-name path)))
        (when (file-exists-p expanded)
          (error "File already exists: %s. Use buffer_edit operation=open plus operation=edit hashline edits for existing files." expanded))
        (let ((dir (file-name-directory expanded)))
          (when (and dir (not (file-directory-p dir)))
            (mkdir dir t)))
        (with-temp-file expanded
          (insert content))
        (format "Created %s (%d bytes)" expanded (length content)))
    (error (format "Error creating file '%s': %s" path (error-message-string err)))))

(defun my/gptel--overwrite-file (path content)
  "Overwrite PATH with CONTENT."
  (condition-case err
      (let ((expanded (expand-file-name path)))
        (let ((dir (file-name-directory expanded)))
          (when (and dir (not (file-directory-p dir)))
            (mkdir dir t)))
        (with-temp-file expanded
          (insert content))
        (format "Wrote %s (%d bytes)" expanded (length content)))
    (error (format "Error writing file '%s': %s" path (error-message-string err)))))

(defun my/gptel--file-write-tool (operation path content)
  "Dispatch mutating file-write OPERATION behind one tool declaration."
  (pcase operation
    ("create" (my/gptel--create-file path content))
    ("overwrite" (my/gptel--overwrite-file path content))
    (_ "Error: file_write operation must be one of: create, overwrite.")))

(defun my/gptel--git-status ()
  "Show git status in porcelain format."
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
             "Use git operation=diff only if you need to verify a specific path.")
          (let* ((trimmed (string-trim-right output))
                 (lines (split-string trimmed "\n" t))
                 (max-show 100)
                 (shown (cl-subseq lines 0 (min (length lines) max-show))))
            (my/gptel--format-tool-result
             "git_status"
             `((target . "current git repository")
               (entry_count . ,(length lines))
               (shown_count . ,(length shown))
               (max_entries . ,max-show))
             (format "%d git status entr%s%s:\n```\n%s\n```"
                     (length lines)
                     (if (= (length lines) 1) "y" "ies")
                     (if (> (length lines) max-show)
                         (format " (showing first %d)" max-show)
                       "")
                     (mapconcat #'identity shown "\n"))
             "Use git operation=diff with staged/path arguments to inspect the relevant changes; if status was truncated, narrow the task to a path or handle the shown entries first."))))
    (error (format "Error getting git status: %s" (error-message-string err)))))

(defun my/gptel--git-diff (&optional staged path)
  "Show git diff, optionally STAGED and limited to PATH."
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
             "If truncated, call git operation=diff with a narrower path or staged scope."))))
    (error (format "Error getting git diff: %s" (error-message-string err)))))

(defun my/gptel--git-tool (operation &optional staged path)
  "Dispatch git OPERATION behind one tool declaration."
  (pcase operation
    ("status" (my/gptel--git-status))
    ("diff" (my/gptel--git-diff staged path))
    (_ "Error: git operation must be one of: status, diff.")))

(defun my/gptel--buffer-diagnostics (buffer)
  "Show Flymake diagnostics for BUFFER."
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
             (max-show 50)
             (shown-diags (cl-subseq diags 0 (min (length diags) max-show)))
             (grouped (seq-group-by #'flymake-diagnostic-type diags))
             (shown-grouped (seq-group-by #'flymake-diagnostic-type shown-diags))
             (severity-counts
              (mapconcat (lambda (group)
                           (format "%s=%d" (car group) (length (cdr group))))
                         grouped ", ")))
        (my/gptel--format-tool-result
         "buffer_diagnostics"
         `((target . ,buffer)
           (flymake_active . "yes")
           (diagnostic_count . ,(length diags))
           (shown_count . ,(length shown-diags))
           (max_diagnostics . ,max-show)
           (severity_counts . ,severity-counts))
         (format "%d diagnostic%s in '%s'%s:%s"
                 (length diags)
                 (if (= (length diags) 1) "" "s")
                 buffer
                 (if (> (length diags) max-show)
                     (format " (showing first %d)" max-show)
                   "")
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
                  shown-grouped
                  ""))
         "Use buffer_inspect operation=context around a diagnostic line, then edit and rerun diagnostics; if shown_count is lower than diagnostic_count, fix the shown diagnostics first or use a narrower validator."))))))

(defun my/gptel--verify-flymake-summary ()
  "Return a verification entry summarizing Flymake diagnostics in current buffer."
  (cond
   ((not (bound-and-true-p flymake-mode))
    '(:skip "Flymake is not active for this buffer."))
   ((null (flymake-diagnostics))
    '(:pass "Flymake is active and reports no diagnostics."))
   (t
    (let* ((diags (flymake-diagnostics))
           (grouped (seq-group-by #'flymake-diagnostic-type diags))
           (severity-counts
            (mapconcat (lambda (group)
                         (format "%s=%d" (car group) (length (cdr group))))
                       grouped ", "))
           (error-count (length (alist-get :error grouped))))
      (if (> error-count 0)
          (list :fail (format "Flymake reports %d diagnostic%s (%s)."
                              (length diags)
                              (if (= (length diags) 1) "" "s")
                              severity-counts))
        (list :pass (format "Flymake reports no errors (%d non-error diagnostic%s: %s)."
                            (length diags)
                            (if (= (length diags) 1) "" "s")
                            severity-counts)))))))

(defun my/gptel--verify-byte-compile-elisp (file)
  "Return a verification entry for byte-compiling Emacs Lisp FILE."
  (let* ((output-file (byte-compile-dest-file file))
         (output-existed (file-exists-p output-file))
         (result nil)
         (log-output ""))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create "*Compile-Log*")
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (let ((byte-compile-error-on-warn t))
            (setq result (byte-compile-file file)))
          (setq log-output
                (string-trim
                 (with-current-buffer (get-buffer "*Compile-Log*")
                   (buffer-string))))
          (cond
           ((eq result 'no-byte-compile)
            '(:skip "File declares no-byte-compile."))
           (result
            (list :pass
                  (if (string-empty-p log-output)
                      "Byte compilation succeeded with no compile log output."
                    (format "Byte compilation succeeded; compile log was non-empty:\n%s"
                            log-output))))
           (t
            (list :fail
                  (if (string-empty-p log-output)
                      "Byte compilation failed with no compile log output."
                    (format "Byte compilation failed:\n%s" log-output))))))
      (when (and (not output-existed) (file-exists-p output-file))
        (delete-file output-file)))))

(defun my/gptel--verification-status (entries)
  "Return PASS, FAIL, or SKIPPED from verification ENTRIES."
  (cond
   ((seq-some (lambda (entry) (plist-get entry :fail)) entries) "FAIL")
   ((seq-some (lambda (entry) (plist-get entry :pass)) entries) "PASS")
   (t "SKIPPED")))

(defun my/gptel--format-verification-entries (entries)
  "Return compact text for verification ENTRIES."
  (mapconcat
   (lambda (entry)
     (cond
      ((plist-get entry :fail) (format "FAIL: %s" (plist-get entry :fail)))
      ((plist-get entry :pass) (format "PASS: %s" (plist-get entry :pass)))
      ((plist-get entry :skip) (format "SKIP: %s" (plist-get entry :skip)))
      (t (format "SKIP: Unrecognized verification entry: %S" entry))))
   entries
   "\n"))

(defun my/gptel--verify-task (target skip-reason)
  "Verify TARGET buffer/file, or return SKIPPED with SKIP-REASON."
  (condition-case err
      (cond
       ((and skip-reason (not (string-empty-p (string-trim skip-reason))))
        (let* ((entries (list (list :skip (string-trim skip-reason))))
               (status (my/gptel--verification-status entries)))
          (my/gptel--format-tool-result
           "verify_task"
           `((status . ,status)
             (target . ,target))
           (format "Verification %s.\n%s"
                   status (my/gptel--format-verification-entries entries))
           "Only report task completion after explaining why verification was skipped.")))
       ((or (null target) (string-empty-p (string-trim target)))
        (let* ((entries (list '(:skip "No target buffer or file was provided.")))
               (status (my/gptel--verification-status entries)))
          (my/gptel--format-tool-result
           "verify_task"
           `((status . ,status))
           (format "Verification %s.\n%s"
                   status (my/gptel--format-verification-entries entries))
           "Call buffer_inspect operation=verify with the edited buffer/file, or pass skip_reason when no meaningful check exists.")))
       (t
        (let* ((expanded (expand-file-name target))
               (existing (or (get-buffer target)
                             (find-buffer-visiting expanded)))
               (buf (or existing
                        (and (file-readable-p expanded)
                             (find-file-noselect expanded)))))
          (unless buf
            (error "No buffer or readable file for target '%s'" target))
          (unwind-protect
              (with-current-buffer buf
                (my/gptel--ensure-readable-buffer "verify_task")
                (let* ((file (buffer-file-name))
                       (elisp-file (and file (string-match-p (rx ".el" string-end) file)))
                       (entries nil))
                  (when (and file (buffer-modified-p))
                    (push '(:fail "Buffer has unsaved changes; save before verifying file-backed checks.") entries))
                  (if elisp-file
                      (progn
                        (push (condition-case paren-err
                                  (progn
                                    (save-excursion (check-parens))
                                    '(:pass "Parentheses and expressions are balanced."))
                                (error (list :fail (format "check-parens failed: %s"
                                                           (error-message-string paren-err)))))
                              entries)
                        (push (condition-case compile-err
                                  (my/gptel--verify-byte-compile-elisp file)
                                (error (list :fail (format "Byte compilation errored: %s"
                                                           (error-message-string compile-err)))))
                              entries))
                    (push '(:skip "Target is not an Emacs Lisp file; skipped buffer_inspect operations check_parens and byte_compile.")
                          entries))
                  (push (my/gptel--verify-flymake-summary) entries)
                  (let* ((entries (nreverse entries))
                         (status (my/gptel--verification-status entries)))
                    (my/gptel--format-tool-result
                     "verify_task"
                     `((status . ,status)
                       (target . ,(or file (buffer-name)))
                       (mode . ,major-mode))
                     (format "Verification %s for %s.\n%s"
                             status
                             (or file (buffer-name))
                             (my/gptel--format-verification-entries entries))
                     (pcase status
                       ("PASS" "You may report task completion with this verification result.")
                       ("FAIL" "Fix the failing check, then rerun buffer_inspect operation=verify before reporting completion.")
                       (_ "If no meaningful check exists, report completion with the skipped verification reason."))))))
            (unless existing
              (kill-buffer buf))))))
    (error (my/gptel--format-tool-result
            "verify_task"
            `((status . "FAIL")
              (target . ,target))
            (format "Verification FAIL.\nFAIL: %s" (error-message-string err))
            "Fix the issue or call buffer_inspect operation=verify with a valid target before reporting completion."))))

(defun my/gptel--buffer-inspect-tool (operation &optional buffer line-number context-lines search-text filename target skip-reason)
  "Dispatch read-only buffer inspection and verification OPERATION."
  (pcase operation
    ("list_buffers" (my/gptel--list-buffers))
    ("context" (my/gptel--show-buffer-context buffer line-number context-lines))
    ("search" (my/gptel--search-buffer-text buffer search-text))
    ("diagnostics" (my/gptel--buffer-diagnostics buffer))
    ("check_parens" (my/gptel--check-parens buffer))
    ("byte_compile" (my/gptel--byte-compile-file filename))
    ("verify" (my/gptel--verify-task target skip-reason))
    (_ "Error: buffer_inspect operation must be one of: list_buffers, context, search, diagnostics, check_parens, byte_compile, verify.")))

(defun my/gptel--buffer-edit-tool (operation &optional path buffer old-str new-str replace-all no-save start-line end-line indent-start-line indent-end-line)
  "Dispatch buffer edit OPERATION behind one tool declaration."
  (pcase operation
    ("open" (my/gptel--open-file path))
    ("edit" (my/gptel--edit-buffer buffer old-str new-str replace-all no-save start-line end-line))
    ("save" (my/gptel--save-buffer buffer))
    ("indent" (my/gptel--indent-region buffer indent-start-line indent-end-line))
    (_ "Error: buffer_edit operation must be one of: open, edit, save, indent.")))

(defun my/gptel--buffer-edit-confirm-p (operation &rest _)
  "Return non-nil when buffer-edit OPERATION mutates a buffer/file."
  (member operation '("edit" "save" "indent")))

(defconst my/gptel--deprecated-dispatched-tool-names
  '("beads_list_issues" "beads_show_issue" "beads_ready_issues"
    "remember"
    "list_skills" "search_skills" "load_skill" "save_skill"
    "fetch_tool_output" "search_tool_output"
    "read_file" "show_buffer_context" "search_buffer_text" "list_buffers"
    "search_project" "list_project_files"
    "open_file" "edit_buffer" "save_buffer" "indent_region"
    "create_file" "overwrite_file"
    "git_status" "git_diff"
    "buffer_diagnostics" "check_parens" "byte_compile_file" "verify_task")
  "Old per-operation gptel tool names now hidden behind dispatcher tools.")

(defun my/gptel--unregister-tools (names)
  "Remove tool NAMES from `gptel--known-tools' when reloading this config."
  (setq gptel--known-tools
        (delq nil
              (mapcar
               (lambda (category)
                 (let ((tools (seq-remove (lambda (entry)
                                            (member (car entry) names))
                                          (cdr category))))
                   (when tools (cons (car category) tools))))
               gptel--known-tools))))

(my/gptel--unregister-tools my/gptel--deprecated-dispatched-tool-names)

(gptel-make-tool
 :name "beads"
 :function #'my/gptel--beads-tool
 :description "Read Beads issues via beads.el's in-process client, not by shelling out to bd. OPERATION selects list, show, ready, or literal keyword search. List-like operations return one bounded page of compact summaries; use operation=show with id for full detail."
 :args (list '(:name "operation" :type "string" :enum ["list" "show" "ready" "search"] :description "Beads operation to run: list filtered issues, show one id, ready unblocked issues, or search issue summary/details.")
             '(:name "project_root" :type "string" :description "Optional Beads project root containing .beads/. Defaults to current project/default-directory.")
             '(:name "id" :type "string" :description "Issue id for operation=show, e.g. pj_gtd_org-kv1.1.")
             '(:name "query" :type "string" :description "Literal keyword query for operation=search.")
             '(:name "status" :type "string" :description "Optional status filter for list/search, e.g. open, in_progress, closed.")
             '(:name "priority" :type "number" :description "Optional priority filter.")
             '(:name "type" :type "string" :description "Optional issue type filter, e.g. task, bug, epic.")
             '(:name "assignee" :type "string" :description "Optional assignee filter for list/search/ready.")
             '(:name "labels" :type "string" :description "Optional labels filter, as accepted by beads.el/bd.")
             '(:name "limit" :type "number" :description "Optional page size for list/search/ready. Defaults to 50 and is capped at 100.")
             '(:name "all" :type "boolean" :description "When true, include closed issues in list/search results.")
             '(:name "offset" :type "number" :description "Optional zero-based result offset for the next list/search/ready page."))
 :include nil
 :category "beads")

(gptel-make-tool
 :name "knowledge"
 :function #'my/gptel--knowledge-tool
 :description "Dispatcher for procedural skills and small durable agent memory. Read-only operations are list_skills, search_skills, and load_skill. Mutating operations remember and save_skill require confirmation; use only for intentional durable state, never secrets or raw transcripts."
 :args (list '(:name "operation" :type "string" :enum ["list_skills" "search_skills" "load_skill" "remember" "save_skill"] :description "Knowledge operation to run.")
             '(:name "scope" :type "string" :enum ["project" "user"] :description "For operation=remember: memory file to update.")
             '(:name "entry" :type "string" :description "For operation=remember: one concise durable fact/preference to remember.")
             '(:name "old_entry" :type "string" :description "For operation=remember: optional exact existing text to replace.")
             '(:name "name_or_file" :type "string" :description "For operation=load_skill: skill slug or markdown file name.")
             '(:name "query" :type "string" :description "For operation=search_skills: regex query over saved skill files.")
             '(:name "title" :type "string" :description "For operation=save_skill: short skill title.")
             '(:name "when_to_use" :type "string" :description "For operation=save_skill: concise trigger conditions.")
             '(:name "steps" :type "string" :description "For operation=save_skill: reusable procedural steps.")
             '(:name "verification" :type "string" :description "For operation=save_skill: how to verify success.")
             '(:name "caveats" :type "string" :description "For operation=save_skill: important constraints or known failure modes.")
             '(:name "max_skills" :type "number" :description "For operation=list_skills: maximum skills to list. Defaults to 25; hard maximum 100.")
             '(:name "max_matches" :type "number" :description "For operation=search_skills: maximum matching lines. Defaults to 25; hard maximum 100.")
             '(:name "max_chars" :type "number" :description "For operation=load_skill: maximum skill body characters to return.")
             '(:name "replace_existing" :type "boolean" :description "For operation=save_skill: replace an existing skill file with the same slug."))
 :confirm #'my/gptel--knowledge-confirm-p
 :category "knowledge")

(gptel-make-tool
 :name "tool_output"
 :function #'my/gptel--tool-output-tool
 :description "Fetch or search bounded ranges from stored gptel tool-output side-car references. Requires a sha256 id from :TOOL_OUTPUT_REF:. There is no unrestricted full-output fetch."
 :args (list '(:name "operation" :type "string" :enum ["fetch" "search"] :description "Use fetch for explicit line ranges, search for regex snippets.")
             '(:name "id" :type "string" :description "Stored output id, e.g. sha256:<hash>.")
             '(:name "start-line" :type "number" :description "For operation=fetch: first line to fetch (1-based, inclusive).")
             '(:name "end-line" :type "number" :description "For operation=fetch: last line to fetch (1-based, inclusive).")
             '(:name "pattern" :type "string" :description "For operation=search: regex pattern to search within stored output.")
             '(:name "max-matches" :type "number" :description "For operation=search: maximum matches to return (default 25, hard maximum 100)."))
 :include nil
 :category "emacs")

(gptel-make-tool
 :name "workspace"
 :function #'my/gptel--workspace-tool
 :description "Read-only project/file workspace dispatcher. Use operation=read_file instead of cat/head/tail/sed, operation=search_project instead of grep/rg shell commands, and operation=list_files instead of ls/find/tree."
 :args (list '(:name "operation" :type "string" :enum ["read_file" "search_project" "list_files"] :description "Workspace operation to run.")
             '(:name "path" :type "string" :description "For operation=read_file: file path to read.")
             '(:name "start-line" :type "number" :description "For operation=read_file: first line to return (1-based, inclusive).")
             '(:name "end-line" :type "number" :description "For operation=read_file: last line to return (1-based, inclusive).")
             '(:name "pattern" :type "string" :description "For operation=search_project: ripgrep regex; for operation=list_files: optional filename regex filter.")
             '(:name "dir" :type "string" :description "Directory to search/list. Defaults to projectile project root or current directory.")
             '(:name "file-glob" :type "string" :description "For operation=search_project: optional ripgrep glob, e.g. '*.el' or '*.{ts,tsx}'.")
             '(:name "case-sensitive" :type "boolean" :description "For operation=search_project: true for case-sensitive search; default is case-insensitive."))
 :include nil
 :category "workspace")

(gptel-make-tool
 :name "buffer_inspect"
 :function #'my/gptel--buffer-inspect-tool
 :description "Read-only Emacs buffer inspection and verification dispatcher. It can list buffers, show context, search within one buffer, report Flymake diagnostics, check parens, byte-compile an elisp file, or run final verification."
 :args (list '(:name "operation" :type "string" :enum ["list_buffers" "context" "search" "diagnostics" "check_parens" "byte_compile" "verify"] :description "Buffer inspection operation to run.")
             '(:name "buffer" :type "string" :description "Buffer name or file path for context/search/diagnostics/check_parens.")
             '(:name "line-number" :type "number" :description "For operation=context: target line number (1-based).")
             '(:name "context-lines" :type "number" :description "For operation=context: lines before/after to show (default 5).")
             '(:name "search-text" :type "string" :description "For operation=search: literal text to search within the buffer.")
             '(:name "filename" :type "string" :description "For operation=byte_compile: Emacs Lisp file path to compile.")
             '(:name "target" :type "string" :description "For operation=verify: buffer name or file path to verify, usually the edited file.")
             '(:name "skip_reason" :type "string" :description "For operation=verify: explicit reason to return SKIPPED when no meaningful check exists."))
 :include nil
 :category "emacs")

(gptel-make-tool
 :name "buffer_edit"
 :function #'my/gptel--buffer-edit-tool
 :description "Mutating Emacs buffer dispatcher. Use operation=open to keep a file buffer alive, operation=edit for hashline or exact-text edits, operation=save after no_save edits, and operation=indent to run major-mode indentation. edit auto-saves visited files unless no_save is true."
 :args (list '(:name "operation" :type "string" :enum ["open" "edit" "save" "indent"] :description "Buffer edit operation to run.")
             '(:name "path" :type "string" :description "For operation=open: file path to open.")
             '(:name "buffer" :type "string" :description "Buffer name or file path for edit/save/indent.")
             '(:name "old_str" :type "string" :description "For operation=edit fallback mode: exact text to replace when start_line is omitted.")
             '(:name "new_str" :type "string" :description "For operation=edit: replacement text; in hashline mode replaces the tagged line/range.")
             '(:name "replace_all" :type "boolean" :description "For operation=edit fallback mode: replace every old_str occurrence.")
             '(:name "no_save" :type "boolean" :description "For operation=edit: skip automatic save; default false.")
             '(:name "start_line" :type "string" :description "For operation=edit: preferred hashline anchor from workspace read_file output, e.g. `42:abc`.")
             '(:name "end_line" :type "string" :description "For operation=edit: optional ending hashline tag for a multi-line replacement.")
             '(:name "indent_start_line" :type "number" :description "For operation=indent: first line of region to indent (1-based, inclusive).")
             '(:name "indent_end_line" :type "number" :description "For operation=indent: last line of region to indent (1-based, inclusive)."))
 :confirm #'my/gptel--buffer-edit-confirm-p
 :category "emacs")

(gptel-make-tool
 :name "file_write"
 :function #'my/gptel--file-write-tool
 :description "Mutating file writer dispatcher. operation=create creates a new file and refuses to clobber; operation=overwrite replaces a whole file and is an exceptional escape hatch. Prefer buffer_edit hashline edits for existing files."
 :args (list '(:name "operation" :type "string" :enum ["create" "overwrite"] :description "File write operation to run.")
             '(:name "path" :type "string" :description "File path to create or overwrite.")
             '(:name "content" :type "string" :description "Full content to write."))
 :confirm t
 :category "filesystem")

(gptel-make-tool
 :name "git"
 :function #'my/gptel--git-tool
 :description "Git review dispatcher for status and diff. operation=status returns porcelain status; operation=diff returns unstaged or staged diff and can be limited to a path."
 :args (list '(:name "operation" :type "string" :enum ["status" "diff"] :description "Git operation to run.")
             '(:name "staged" :type "boolean" :description "For operation=diff: show staged changes instead of unstaged.")
             '(:name "path" :type "string" :description "For operation=diff: limit diff to a specific file or directory."))
 :include nil
 :category "git")

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

(defvar-local my/gptel--yolo-saved-confirm-tool-calls nil
  "Saved `gptel-confirm-tool-calls' state while `my/gptel-yolo-mode' is on.
The value is a cons cell (HAD-LOCAL . VALUE), or nil when YOLO mode has not
saved state in the current buffer.")

(define-minor-mode my/gptel-yolo-mode
  "Run gptel tool calls in this buffer without confirmation prompts.

This is intentionally opt-in and buffer-local.  Enable it for disposable
agent/sub-agent sessions where manual y/n approval loops are more expensive
than per-tool confirmation safety.  It only sets `gptel-confirm-tool-calls' to
nil; tool implementations and their runtime safety checks still run."
  :init-value nil
  :lighter " YOLO"
  :group 'gptel
  (if my/gptel-yolo-mode
      (progn
        (unless my/gptel--yolo-saved-confirm-tool-calls
          (setq-local my/gptel--yolo-saved-confirm-tool-calls
                      (cons (local-variable-p 'gptel-confirm-tool-calls
                                              (current-buffer))
                            gptel-confirm-tool-calls)))
        (setq-local gptel-confirm-tool-calls nil))
    (when my/gptel--yolo-saved-confirm-tool-calls
      (if (car my/gptel--yolo-saved-confirm-tool-calls)
          (setq-local gptel-confirm-tool-calls
                      (cdr my/gptel--yolo-saved-confirm-tool-calls))
        (kill-local-variable 'gptel-confirm-tool-calls))
      (kill-local-variable 'my/gptel--yolo-saved-confirm-tool-calls))))

(defcustom my/gptel-agent-yolo-mode nil
  "Non-nil means agentic gptel presets enable `my/gptel-yolo-mode'.

Default nil preserves gptel's normal per-tool confirmations.  Set this with
`setq' only when agent-* sessions and delegated sub-agents should run tools
without approval prompts.  For one buffer/session, use `my/gptel-yolo-mode'
instead."
  :type 'boolean
  :group 'gptel)

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
  (setq-local my/gptel-prompt-size-guard-enabled t)
  (when my/gptel-agent-yolo-mode
    (my/gptel-yolo-mode 1)))

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

(defcustom my/gptel--project-memory-file-name "MEMORY.md"
  "Project-local memory file name injected into agentic gptel prompts.
The file is resolved relative to the current project root.  Missing files are
ignored.  Keep this file curated: do not store secrets, raw tool output, or
chat transcripts."
  :type 'string
  :group 'gptel)

(defcustom my/gptel--user-memory-file
  (expand-file-name "gptel/USER.md"
                    (or (getenv "XDG_CONFIG_HOME")
                        (expand-file-name "~/.config")))
  "User preference memory file injected into agentic gptel prompts.
Missing files are ignored.  Keep this file curated: do not store secrets, raw
tool output, or chat transcripts."
  :type 'file
  :group 'gptel)

(defcustom my/gptel--memory-file-max-chars 2000
  "Maximum characters injected from each gptel persistent memory file."
  :type 'integer
  :group 'gptel)

(defcustom my/gptel--memory-entry-max-chars 800
  "Maximum characters accepted for a single curated gptel memory entry."
  :type 'integer
  :group 'gptel)

(defcustom my/gptel--skills-directory
  (expand-file-name "gptel-skills/" user-emacs-directory)
  "Directory where gptel procedural skill markdown files are stored.
These are durable reusable procedures, not factual memory.  Keep files concise
and do not store secrets, raw tool output, or chat transcripts."
  :type 'directory
  :group 'gptel)

(defcustom my/gptel--skill-file-max-chars 8000
  "Maximum characters accepted for one saved gptel procedural skill file."
  :type 'integer
  :group 'gptel)

(defcustom my/gptel--skill-load-max-chars 4000
  "Default maximum characters returned when loading one gptel skill."
  :type 'integer
  :group 'gptel)

(defcustom my/gptel--project-awareness-files
  '("AGENTS.md" "README.md" "README.org" "Makefile"
    "package.json" "pyproject.toml" "Cargo.toml" "go.mod")
  "Project-root files summarized in agentic gptel project context when present."
  :type '(repeat string)
  :group 'gptel)

(defcustom my/gptel--project-awareness-file-max-chars 700
  "Maximum characters injected from each project-awareness file excerpt."
  :type 'integer
  :group 'gptel)

(defcustom my/gptel--project-awareness-max-files 4
  "Maximum project-awareness file excerpts injected into agentic prompts."
  :type 'integer
  :group 'gptel)

(defcustom my/gptel--project-awareness-file-map-max-entries 50
  "Maximum project file-map entries injected into agentic prompts."
  :type 'integer
  :group 'gptel)

(defun my/gptel--project-root ()
  "Return the current project root, falling back to `default-directory'."
  (condition-case nil
      (projectile-project-root)
    (error default-directory)))

(defun my/gptel--read-bounded-memory-file (label path)
  "Return a bounded memory block for LABEL at PATH, or nil if absent."
  (when (and path (file-readable-p path))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents-literally path)
          (let* ((text (string-trim (buffer-string)))
                 (total-chars (length text))
                 (cap my/gptel--memory-file-max-chars)
                 (truncated (> total-chars cap))
                 (shown (if truncated (substring text 0 cap) text)))
            (unless (string-empty-p shown)
              (format "## %s memory (%s, chars %d%s)\n%s%s"
                      label
                      (abbreviate-file-name path)
                      total-chars
                      (if truncated (format ", capped at %d" cap) "")
                      shown
                      (if truncated
                          "\n\n[Memory truncated: summarize or prune this memory file before relying on omitted content.]"
                        "")))))
      (error (format "## %s memory (%s)\n[Memory unavailable: %s]"
                     label (abbreviate-file-name path) (error-message-string err))))))

(defun my/gptel--memory-file-for-scope (scope)
  "Return the configured memory file path for SCOPE.
SCOPE must be either `project' or `user'."
  (pcase (downcase (string-trim (or scope "")))
    ("project" (expand-file-name my/gptel--project-memory-file-name
                                  (my/gptel--project-root)))
    ("user" my/gptel--user-memory-file)
    (_ (error "scope must be either 'project' or 'user'"))))

(defun my/gptel--memory-read-raw (path)
  "Return PATH contents as a string, or an empty string when PATH is absent."
  (if (file-readable-p path)
      (with-temp-buffer
        (insert-file-contents-literally path)
        (buffer-string))
    ""))

(defun my/gptel--format-memory-entry (text)
  "Return curated memory entry TEXT with date metadata."
  (format "- %s: %s\n"
          (format-time-string "%Y-%m-%d")
          (string-trim text)))

(defun my/gptel--memory-cap-refusal (path chars action)
  "Return an actionable refusal for PATH with CHARS after attempted ACTION."
  (format "Refused to %s %s because it would contain %d chars, over the configured memory cap of %d. Summarize or prune this memory file first, then retry with one small curated fact or preference."
          action
          (abbreviate-file-name path)
          chars
          my/gptel--memory-file-max-chars))

(defun my/gptel--remember (scope entry &optional old-entry)
  "Append or update curated persistent memory ENTRY for SCOPE.
When OLD-ENTRY is non-empty, replace that exact text with ENTRY.  Otherwise,
append ENTRY as a dated memory bullet."
  (condition-case err
      (let* ((path (my/gptel--memory-file-for-scope scope))
             (entry (string-trim (or entry "")))
             (old-entry (and old-entry (string-trim old-entry)))
             (old-entry (and old-entry (not (string-empty-p old-entry)) old-entry))
             (existing (my/gptel--memory-read-raw path))
             (existing-chars (length (string-trim existing)))
             (operation (if old-entry "update" "append"))
             candidate)
        (cond
         ((string-empty-p entry)
          "Error: entry must not be empty. Provide one small curated fact or preference, not a transcript.")
         ((> (length entry) my/gptel--memory-entry-max-chars)
          (format "Error: entry is %d chars, over the per-entry cap of %d. Summarize it into one small durable fact or preference, then retry."
                  (length entry) my/gptel--memory-entry-max-chars))
         ((and (not old-entry)
               (> existing-chars my/gptel--memory-file-max-chars))
          (my/gptel--memory-cap-refusal path existing-chars operation))
         (old-entry
          (let ((count 0)
                (case-fold-search nil))
            (with-temp-buffer
              (insert existing)
              (goto-char (point-min))
              (while (search-forward old-entry nil t)
                (setq count (1+ count)))
              (cond
               ((zerop count)
                (format "Error: old_entry was not found in %s. Re-read the memory file and pass exact text to update, or omit old_entry to append a new dated entry."
                        (abbreviate-file-name path)))
               ((> count 1)
                (format "Error: old_entry matched %d times in %s. Provide more exact surrounding text so the update is unambiguous."
                        count (abbreviate-file-name path)))
               (t
                (goto-char (point-min))
                (search-forward old-entry nil t)
                (replace-match (my/gptel--format-memory-entry entry) t t)
                (setq candidate (buffer-string))
                (if (> (length (string-trim candidate)) my/gptel--memory-file-max-chars)
                    (my/gptel--memory-cap-refusal path (length (string-trim candidate)) operation)
                  (let ((dir (file-name-directory path))
                        (coding-system-for-write 'utf-8-unix))
                    (when (and dir (not (file-directory-p dir)))
                      (make-directory dir t))
                    (with-temp-file path
                      (insert candidate)))
                  (my/gptel--format-tool-result
                   "remember"
                   `((scope . ,scope)
                     (operation . ,operation)
                     (target . ,(abbreviate-file-name path))
                     (chars . ,(length (string-trim candidate)))
                     (cap_chars . ,my/gptel--memory-file-max-chars))
                   (format "Updated curated %s memory in %s."
                           scope (abbreviate-file-name path))
                   "Use remember for small durable facts/preferences only; summarize or prune if the memory approaches the cap.")))))))
         (t
          (setq candidate
                (concat (if (string-empty-p existing)
                            ""
                          (concat (string-remove-suffix "\n" existing) "\n"))
                        (my/gptel--format-memory-entry entry)))
          (if (> (length (string-trim candidate)) my/gptel--memory-file-max-chars)
              (my/gptel--memory-cap-refusal path (length (string-trim candidate)) operation)
            (let ((dir (file-name-directory path))
                  (coding-system-for-write 'utf-8-unix))
              (when (and dir (not (file-directory-p dir)))
                (make-directory dir t))
              (with-temp-file path
                (insert candidate)))
            (my/gptel--format-tool-result
             "remember"
             `((scope . ,scope)
               (operation . ,operation)
               (target . ,(abbreviate-file-name path))
               (chars . ,(length (string-trim candidate)))
               (cap_chars . ,my/gptel--memory-file-max-chars))
             (format "Appended curated %s memory to %s."
                     scope (abbreviate-file-name path))
             "Use remember for small durable facts/preferences only; summarize or prune if the memory approaches the cap.")))))
    (error (format "Error updating memory: %s" (error-message-string err)))))

(defun my/gptel--skill-files ()
  "Return saved gptel skill markdown files under `my/gptel--skills-directory'."
  (when (file-directory-p my/gptel--skills-directory)
    (directory-files-recursively my/gptel--skills-directory "\\.md\\'")))

(defun my/gptel--skill-slug (title)
  "Return a filesystem-safe skill slug derived from TITLE."
  (let* ((title (downcase (string-trim (or title ""))))
         (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" title))
         (slug (replace-regexp-in-string "\`-+\|-+\'" "" slug)))
    (unless (and slug (not (string-empty-p slug)))
      (error "title must contain at least one alphanumeric character"))
    slug))

(defun my/gptel--skill-path-for-title (title)
  "Return target skill path for TITLE."
  (expand-file-name (concat (my/gptel--skill-slug title) ".md")
                    my/gptel--skills-directory))

(defun my/gptel--skill-resolve-path (name-or-file)
  "Resolve NAME-OR-FILE to a saved skill path under the skills directory."
  (let* ((name (string-trim (or name-or-file "")))
         (filename (file-name-nondirectory name))
         (filename (if (string-suffix-p ".md" filename)
                       filename
                     (concat (my/gptel--skill-slug filename) ".md")))
         (path (expand-file-name filename my/gptel--skills-directory))
         (root (file-name-as-directory
                (file-truename (expand-file-name my/gptel--skills-directory)))))
    (unless (file-readable-p path)
      (error "skill not found: %s" (abbreviate-file-name path)))
    (let ((truename (file-truename path)))
      (unless (string-prefix-p root truename)
        (error "skill path must stay under %s" (abbreviate-file-name root))))
    path))

(defun my/gptel--skill-section-summary (section text)
  "Return first non-empty line under markdown SECTION in TEXT, or nil."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (when (re-search-forward (format "^## %s[[:space:]]*$" (regexp-quote section)) nil t)
      (let ((start (point))
            (end (or (save-excursion
                       (when (re-search-forward "^## " nil t)
                         (match-beginning 0)))
                     (point-max))))
        (goto-char start)
        (catch 'summary
          (while (< (point) end)
            (let ((line (string-trim (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))))
              (unless (or (string-empty-p line)
                          (string-prefix-p "#" line))
                (throw 'summary line)))
            (forward-line 1))
          nil)))))

(defun my/gptel--skill-title-from-text (path text)
  "Return skill title from TEXT, falling back to PATH basename."
  (if (string-match (rx string-start "# " (group (+ not-newline))) text)
      (match-string 1 text)
    (file-name-base path)))

(defun my/gptel--list-skills (&optional max-skills)
  "Return a bounded list of saved procedural skills."
  (condition-case err
      (let* ((files (sort (or (my/gptel--skill-files) nil) #'string<))
             (limit (min 100 (max 1 (round (or max-skills 25)))))
             (shown (cl-subseq files 0 (min (length files) limit)))
             (entries
              (mapcar
               (lambda (path)
                 (with-temp-buffer
                   (insert-file-contents-literally path nil 0 2000)
                   (let* ((text (buffer-string))
                          (title (my/gptel--skill-title-from-text path text))
                          (when-to-use (or (my/gptel--skill-section-summary "When to use" text)
                                           "(no trigger summary)")))
                     (format "- %s (%s): %s"
                             title
                             (file-name-nondirectory path)
                             (truncate-string-to-width when-to-use 180 nil nil t)))))
               shown)))
        (my/gptel--format-tool-result
         "list_skills"
         `((target . ,(abbreviate-file-name my/gptel--skills-directory))
           (skill_count . ,(length files))
           (shown_count . ,(length shown))
           (max_skills . ,limit))
         (if entries
             (format "Saved procedural skills%s:\n%s"
                     (if (> (length files) limit)
                         (format " (showing first %d of %d)" limit (length files))
                       "")
                     (mapconcat #'identity entries "\n"))
           (format "No saved procedural skills found in %s."
                   (abbreviate-file-name my/gptel--skills-directory)))
         "Use knowledge operation=search_skills for a keyword/regex query, operation=load_skill for one relevant file, or operation=save_skill after completing a reusable procedure."))
    (error (format "Error listing skills: %s" (error-message-string err)))))

(defun my/gptel--search-skills (query &optional max-matches)
  "Search saved skills for QUERY and return bounded line snippets."
  (condition-case err
      (let ((query (string-trim (or query "")))
            (limit (min 100 (max 1 (round (or max-matches 25)))))
            (matches nil)
            (match-count 0)
            (files (sort (or (my/gptel--skill-files) nil) #'string<)))
        (when (string-empty-p query)
          (error "query must not be empty"))
        (dolist (path files)
          (with-temp-buffer
            (insert-file-contents-literally path)
            (goto-char (point-min))
            (while (re-search-forward query nil t)
              (setq match-count (1+ match-count))
              (when (<= match-count limit)
                (let ((line (string-trim
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position)))))
                  (push (format "%s:%d: %s"
                                (file-name-nondirectory path)
                                (line-number-at-pos)
                                (truncate-string-to-width line 220 nil nil t))
                        matches))))))
        (my/gptel--format-tool-result
         "search_skills"
         `((target . ,(abbreviate-file-name my/gptel--skills-directory))
           (query . ,query)
           (file_count . ,(length files))
           (match_count . ,match-count)
           (shown_count . ,(length matches))
           (max_matches . ,limit))
         (if matches
             (format "Found %d skill match%s for %S%s:\n%s"
                     match-count
                     (if (= match-count 1) "" "es")
                     query
                     (if (> match-count limit)
                         (format " (showing first %d)" limit)
                       "")
                     (mapconcat #'identity (nreverse matches) "\n"))
           (format "No saved skills matched %S." query))
         "Use knowledge operation=load_skill with a reported file name to retrieve one bounded skill body."))
    (error (format "Error searching skills: %s" (error-message-string err)))))

(defun my/gptel--load-skill (name-or-file &optional max-chars)
  "Load one saved procedural skill by NAME-OR-FILE with a bounded body."
  (condition-case err
      (let* ((path (my/gptel--skill-resolve-path name-or-file))
             (requested (and max-chars (round max-chars)))
             (hard-max (* 2 my/gptel--skill-load-max-chars))
             (cap (min hard-max (max 1 (or requested my/gptel--skill-load-max-chars))))
             text total shown truncated)
        (with-temp-buffer
          (insert-file-contents-literally path)
          (setq text (string-trim (buffer-string)))
          (setq total (length text))
          (setq truncated (> total cap))
          (setq shown (if truncated (substring text 0 cap) text)))
        (my/gptel--format-tool-result
         "load_skill"
         `((target . ,(abbreviate-file-name path))
           (skill_chars . ,total)
           (skill_cap_chars . ,cap)
           (skill_truncated . ,(if truncated "yes" "no")))
         (concat shown
                 (when truncated
                   "\n\n[Skill truncated: load a narrower or more concise skill, or prune the saved skill file.]"))
         "Use the loaded procedure as guidance, not proof; verify against the current project before acting."))
    (error (format "Error loading skill: %s" (error-message-string err)))))

(defun my/gptel--save-skill (title when-to-use steps verification caveats &optional replace-existing)
  "Save a procedural skill markdown file with a consistent minimal format."
  (condition-case err
      (let* ((title (string-trim (or title "")))
             (when-to-use (string-trim (or when-to-use "")))
             (steps (string-trim (or steps "")))
             (verification (string-trim (or verification "")))
             (caveats (string-trim (or caveats "")))
             (path (my/gptel--skill-path-for-title title))
             (body (format "# %s\n\nSaved: %s\n\n## When to use\n%s\n\n## Steps\n%s\n\n## Verification\n%s\n\n## Caveats\n%s\n"
                           title
                           (format-time-string "%Y-%m-%d")
                           when-to-use
                           steps
                           verification
                           caveats)))
        (cond
         ((or (string-empty-p title)
              (string-empty-p when-to-use)
              (string-empty-p steps)
              (string-empty-p verification)
              (string-empty-p caveats))
          "Error: title, when_to_use, steps, verification, and caveats are all required for a maintainable skill.")
         ((> (length body) my/gptel--skill-file-max-chars)
          (format "Error: skill is %d chars, over the per-skill cap of %d. Summarize the procedure and remove raw transcripts/tool output before saving."
                  (length body) my/gptel--skill-file-max-chars))
         ((and (file-exists-p path) (not replace-existing))
          (format "Error: skill already exists at %s. Load it first and pass replace_existing=true only if replacing the whole concise procedure is intentional."
                  (abbreviate-file-name path)))
         (t
          (make-directory (file-name-directory path) t)
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file path
              (insert body)))
          (my/gptel--format-tool-result
           "save_skill"
           `((target . ,(abbreviate-file-name path))
             (chars . ,(length body))
             (cap_chars . ,my/gptel--skill-file-max-chars)
             (replaced . ,(if replace-existing "yes" "no")))
           (format "Saved procedural skill '%s' to %s."
                   title (abbreviate-file-name path))
           "Use knowledge operations search_skills/list_skills/load_skill to retrieve this procedure later; use operation=remember for durable facts or preferences instead."))))
    (error (format "Error saving skill: %s" (error-message-string err)))))

(defun my/gptel--memory-context-string ()
  "Return bounded persistent memory context for agentic system prompts."
  (let* ((root (my/gptel--project-root))
         (project-memory (expand-file-name my/gptel--project-memory-file-name root))
         (blocks (delq nil
                       (list
                        (my/gptel--read-bounded-memory-file "Project" project-memory)
                        (my/gptel--read-bounded-memory-file "User" my/gptel--user-memory-file)))))
    (when blocks
      (concat "<persistent-memory>\n"
              "Curated memory only. Treat as guidance, not proof. Do not add secrets, raw tool output, or chat transcripts.\n\n"
              (mapconcat #'identity blocks "\n\n")
              "\n</persistent-memory>"))))

(defun my/gptel--project-awareness-file-paths (root)
  "Return existing project-awareness file paths under ROOT."
  (let ((paths (seq-filter #'file-readable-p
                           (mapcar (lambda (name) (expand-file-name name root))
                                   my/gptel--project-awareness-files))))
    (cl-subseq paths 0 (min (max 0 my/gptel--project-awareness-max-files)
                            (length paths)))))

(defun my/gptel--project-awareness-file-excerpt (root path)
  "Return a bounded project-awareness excerpt for PATH under ROOT."
  (condition-case err
      (let* ((cap (max 0 my/gptel--project-awareness-file-max-chars))
             (attrs (file-attributes path))
             (bytes (file-attribute-size attrs))
             (text (with-temp-buffer
                     (insert-file-contents-literally path nil 0 (1+ cap))
                     (string-trim (buffer-string))))
             (chars (length text))
             (truncated (or (> bytes cap) (> chars cap)))
             (shown (if (> chars cap) (substring text 0 cap) text)))
        (format "### %s (bytes %d, shown_chars %d%s)\n%s"
                (file-relative-name path root)
                bytes
                (length shown)
                (if truncated ", truncated yes" ", truncated no")
                shown))
    (error (format "### %s\n[Project-awareness file unavailable: %s]"
                   (file-relative-name path root) (error-message-string err)))))

(defun my/gptel--project-awareness-file-section (root)
  "Return bounded useful-file excerpts for ROOT, or nil."
  (let* ((paths (my/gptel--project-awareness-file-paths root))
         (total-present
          (length (seq-filter #'file-readable-p
                              (mapcar (lambda (name) (expand-file-name name root))
                                      my/gptel--project-awareness-files)))))
    (when paths
      (format "## Useful project files (shown %d of %d candidates, per_file_cap_chars %d)\n%s"
              (length paths)
              total-present
              my/gptel--project-awareness-file-max-chars
              (mapconcat (lambda (path)
                           (my/gptel--project-awareness-file-excerpt root path))
                         paths
                         "\n\n")))))

(defun my/gptel--project-awareness-top-level-files (root)
  "Return bounded top-level file-map entries for ROOT without recursion."
  (when (file-directory-p root)
    (sort
     (mapcar (lambda (path)
               (concat (file-name-nondirectory (directory-file-name path))
                       (if (file-directory-p path) "/" "")))
             (seq-filter (lambda (path)
                           (not (string-prefix-p "." (file-name-nondirectory path))))
                         (directory-files root t "\\`[^.]")))
     #'string<)))

(defun my/gptel--project-awareness-file-map (root)
  "Return (SOURCE FILES) for a deterministic bounded project file map."
  (condition-case nil
      (let ((files (projectile-project-files root)))
        (list "projectile" (sort files #'string<)))
    (error (list "top-level" (my/gptel--project-awareness-top-level-files root)))))

(defun my/gptel--project-awareness-file-map-section (root)
  "Return a bounded project file-map section for ROOT."
  (pcase-let* ((`(,source ,files) (my/gptel--project-awareness-file-map root))
               (files (or files nil))
               (limit (max 1 my/gptel--project-awareness-file-map-max-entries))
               (shown (cl-subseq files 0 (min (length files) limit))))
    (when shown
      (format "## Project file map (%s, shown %d of %d, truncated %s)\n%s"
              source
              (length shown)
              (length files)
              (if (> (length files) limit) "yes" "no")
              (mapconcat #'identity shown "\n")))))

(defun my/gptel--project-awareness-string (root)
  "Return a compact bounded project-awareness block for ROOT."
  (let ((sections (delq nil (list (my/gptel--project-awareness-file-section root)
                                  (my/gptel--project-awareness-file-map-section root)))))
    (when sections
      (concat "<project-awareness>\n"
              "Deterministic bounded hints only; use workspace operation=read_file/search_project for authoritative details.\n\n"
              (mapconcat #'identity sections "\n\n")
              "\n</project-awareness>"))))

(defun my/gptel--project-context-string ()
  "Return a formatted project-context block for agentic system prompts.
Injected into system messages at request-send time via the preset lambda."
  (let* ((project-root (my/gptel--project-root))
         (root (abbreviate-file-name project-root))
         (buf-name (buffer-name))
         (buf-file (buffer-file-name))
         (awareness (my/gptel--project-awareness-string project-root)))
    (concat "<project-context>\n"
            "Project root: " root "\n"
            "Active buffer: " buf-name
            (when buf-file (concat " (" (abbreviate-file-name buf-file) ")"))
            "\n</project-context>"
            (when awareness
              (concat "\n\n" awareness)))))

(defun my/gptel--agent-context-string ()
  "Return bounded context blocks for agentic system prompts."
  (string-join (delq nil (list (my/gptel--project-context-string)
                               (my/gptel--memory-context-string)))
               "\n\n"))

(defconst my/gptel--agent-read-system
  "You are an Emacs coding assistant in read-only research mode.

TOOLS: slim read-only preset -- scoped search/read plus git review. No edits,
no shell, no broad tool inventory.

TOOL SELECTION: prefer the native Emacs/project tools. They are faster, keep
context compact, and avoid wasting tokens on shell transcript boilerplate.

If this task needs broad file maps, buffer diagnostics, skills/memory,
tool-output side-car navigation, or full Beads list/search, switch to the
`agent-read-full' preset intentionally.

WORKFLOW:
1. Use workspace operation=search_project with a specific regex and optional dir/file-glob to find likely files.
2. Use workspace operation=read_file to inspect exact file ranges; paginate with start-line/end-line for re-reads.
3. Use beads operation=show only when the user already supplied a specific issue id.
4. Use git operation=status/diff when the task is about current changes.
5. If you cannot proceed without a heavier tool, say which full preset/tool is needed instead of improvising.

PARALLELIZE: when reads are independent (e.g. reading several files), issue them in a single message.

[project_context]"
  "System prompt for the `agent-read' preset.")

(defconst my/gptel--agent-read-full-system
  "You are an Emacs coding assistant in broad read-only research mode.

TOOLS: workspace read/search, buffer inspection, skills, Beads, tool-output
navigation, and git review only -- no edits, no shell.

TOOL SELECTION: prefer the native Emacs/project tools. They are faster, keep
context compact, and avoid wasting tokens on shell transcript boilerplate.

WORKFLOW:
1. Use workspace operation=list_files/search_project to discover relevant files.
2. Use workspace operation=read_file to read (full file first; paginate with start-line/end-line only for re-reads).
3. Use buffer_inspect operation=context to zoom in on a specific line range.
4. Use buffer_inspect operation=search and workspace operation=search_project for cross-referencing symbols.
5. Use knowledge operation=list_skills/search_skills/load_skill when a reusable procedural guide may apply; skills are procedures, not factual memory.
6. Use git operation=status/diff to understand recent changes.
7. If a tool returns :TOOL_OUTPUT_REF:, use tool_output operation=search or bounded operation=fetch ranges to inspect it; never ask for a full raw replay.
8. Use buffer_inspect operation=verify for final diagnostics when a concrete target is available, or operations check_parens/diagnostics for exploratory static analysis.

PARALLELIZE: when reads are independent (e.g. reading several files), issue them in a single message.

[project_context]"
  "System prompt for the `agent-read-full' preset.")

(defconst my/gptel--agent-edit-system
  "You are an Emacs coding assistant operating directly on the user's open buffers.

TOOLS: slim edit preset -- scoped search/read/edit/verify plus git review.
No shell, delegation, skills, memory, full-file overwrite, or new file
creation.

TOOL SELECTION: prefer the native Emacs/project tools. They are faster, keep
context compact, and avoid wasting tokens on shell transcript boilerplate.

If this task needs creating files, delegation, skills, memory, Beads
list/search, tool-output side-car navigation, or other heavy
tools, switch to `agent-edit-full' intentionally. If it needs arbitrary shell,
switch to `agent-shell'.

CRITICAL: buffer_edit operation=edit auto-saves the buffer after each edit.  In this slim
preset, leave no_save unset/false; switch to `agent-edit-full' if you need to
batch several edits before one explicit save.

FULL-FILE WRITES: do not rewrite existing files wholesale.  For existing
files, use buffer_edit operation=open plus operation=edit hashline edits.  This slim preset cannot
create brand-new files; switch to `agent-edit-full' when new files are
intentionally required.

WORKFLOW:
1. Use workspace operation=search_project with a specific regex and optional dir/file-glob to find likely files.
2. Use workspace operation=read_file (full file first!) before editing; re-read a narrower range when needed.
3. Use buffer_edit operation=open to load a file into a live buffer; pass the returned buffer name to operation=edit.
4. Prefer buffer_edit operation=edit hashline edits: copy start_line/end_line tags like `42:abc` from workspace read_file output and put the replacement in new_str.
   - Hashline edits verify the current line hash, recover nearby shifted lines, and fail safely with refreshed context if stale.
   - For single-line edits, pass start_line only; for ranges, pass both start_line and end_line.
   - In hashline mode, old_str can be the empty string.
   - Use exact old_str replacement only as a fallback when hashline tags are unavailable.
   - In fallback mode, old_str must uniquely match the target text; pass replace_all=true only when intentionally replacing every occurrence.
5. Batch coherent edits first, then run the narrowest useful verification once before declaring completion: call buffer_inspect operation=verify on the edited buffer/file and get PASS. If no meaningful automated check exists, pass skip_reason and report SKIPPED honestly.
   - Do not run check_parens/byte_compile after every intermediate edit; that wastes turns and transcript space.
   - Verify earlier only when the result will directly guide the next edit, such as uncertain syntax, an unclear stale-edit recovery, or debugging a failing check.
6. Use git operation=status/diff to review your changes before finishing.
7. If a needed step is impossible with this slim tool set, stop and state which full preset/tool is required.

PARALLELIZE: when reads are independent, issue them in a single message.

[project_context]"
  "System prompt for the `agent-edit' preset.")

(defconst my/gptel--agent-edit-full-system
  "You are an Emacs coding assistant operating directly on the user's open buffers.

TOOLS: full native Emacs file/search/edit tools plus delegation, Beads,
skills, memory, diagnostics, and git review -- no shell.

TOOL SELECTION: prefer the native Emacs/project tools. They are faster, keep
context compact, and avoid wasting tokens on shell transcript boilerplate.

CRITICAL: buffer_edit operation=edit auto-saves the buffer after each edit
(unless no_save=true). Use buffer_edit operation=save explicitly only when deliberately batching
several no_save=true edits before a single save.

FULL-FILE WRITES: do not rewrite existing files wholesale.  For existing
files, use buffer_edit operation=open plus operation=edit hashline edits.
file_write operation=create is only for brand-new files and refuses to clobber
existing paths.

WORKFLOW:
1. Use workspace operation=list_files/search_project to discover relevant files.
2. Use workspace operation=read_file (full file first!) or buffer_inspect operation=context before editing.
3. Use delegate_agent with subagent_type=agent-read for independent, context-cheap research.
4. Use buffer_edit operation=open to load a file into a live buffer; pass the returned buffer name to operation=edit.
5. Prefer buffer_edit operation=edit hashline edits: copy start_line/end_line tags like `42:abc` from workspace read_file output and put the replacement in new_str.
   - Hashline edits verify the current line hash, recover nearby shifted lines, and fail safely with refreshed context if stale.
   - For single-line edits, pass start_line only; for ranges, pass both start_line and end_line.
   - In hashline mode, old_str can be the empty string.
   - Use exact old_str replacement only as a fallback when hashline tags are unavailable.
   - In fallback mode, old_str must uniquely match the target text; pass replace_all=true only when intentionally replacing every occurrence.
6. If a tool returns :TOOL_OUTPUT_REF:, use tool_output operation=search or bounded operation=fetch ranges to inspect it; never ask for a full raw replay.
7. Use knowledge operation=list_skills/search_skills/load_skill when a reusable procedural guide may apply; use operation=save_skill only after completing a reusable workflow worth distilling.
8. Use knowledge operation=remember only for confirmed small durable facts/preferences; never store secrets, raw tool output, transcripts, or speculation.
9. Batch coherent edits first, then run the narrowest useful verification once before declaring completion: call buffer_inspect operation=verify on the edited buffer/file and get PASS. If no meaningful automated check exists, pass skip_reason and report SKIPPED honestly.
   - Do not run check_parens/byte_compile after every intermediate edit; that wastes turns and transcript space.
   - Verify earlier only when the result will directly guide the next edit, such as uncertain syntax, an unclear stale-edit recovery, or debugging a failing check.
10. Use buffer_inspect operation=check_parens/byte_compile/diagnostics for narrower follow-up diagnostics when verification fails, when early feedback will directly inform the next edit, or when more detail is needed.
11. Use git operation=status/diff to review your changes before finishing.

PARALLELIZE: when reads are independent, issue them in a single message.

[project_context]"
  "System prompt for the `agent-edit-full' preset.")

(defconst my/gptel--agent-shell-system
  "You are an Emacs coding assistant with full shell access.

TOOL SELECTION: native Emacs tools are the default. They are faster, keep
context compact, and avoid wasting tokens on shell transcript boilerplate.
Do not shell out for routine file discovery, file reading, or text search.

CRITICAL: buffer_edit operation=edit auto-saves the buffer after each edit
(unless no_save=true). Use buffer_edit operation=save explicitly only when deliberately batching
several no_save=true edits before a single save.

FULL-FILE WRITES: do not rewrite existing files wholesale.  For existing
files, use buffer_edit operation=open plus operation=edit hashline edits.
file_write operation=create is only for brand-new files and refuses to clobber
existing paths.

WORKFLOW:
1. Use workspace operation=list_files/search_project to discover relevant files.
2. Use workspace operation=read_file (full file first!) or buffer_inspect operation=context before editing.
3. Use delegate_agent with subagent_type=agent-read for independent, context-cheap research.
4. Use buffer_edit operation=open to load a file into a live buffer; pass the returned buffer name to operation=edit.
5. Prefer buffer_edit operation=edit hashline edits: copy start_line/end_line tags like `42:abc` from workspace read_file output and put the replacement in new_str.
   - Hashline edits verify the current line hash, recover nearby shifted lines, and fail safely with refreshed context if stale.
   - In hashline mode, old_str can be the empty string.
   - Use exact old_str replacement only as a fallback when hashline tags are unavailable.
6. If a tool returns :TOOL_OUTPUT_REF:, use tool_output operation=search or bounded operation=fetch ranges to inspect it; never ask for a full raw replay.
7. Use knowledge operation=list_skills/search_skills/load_skill when a reusable procedural guide may apply; use operation=save_skill only after completing a reusable workflow worth distilling.
8. Use knowledge operation=remember only for confirmed small durable facts/preferences; never store secrets, raw tool output, transcripts, or speculation.
9. Batch coherent edits first, then run the narrowest useful verification once before declaring completion: call buffer_inspect operation=verify on the edited buffer/file and get PASS. If no meaningful automated check exists, pass skip_reason and report SKIPPED honestly.
   - Do not run check_parens/byte_compile after every intermediate edit; that wastes turns and transcript space.
   - Verify earlier only when the result will directly guide the next edit, such as uncertain syntax, an unclear stale-edit recovery, or debugging a failing check.
10. Use buffer_inspect operation=check_parens/byte_compile/diagnostics for narrower follow-up diagnostics when verification fails, when early feedback will directly inform the next edit, or when more detail is needed.
11. Reserve run_shell_command for commands that genuinely need an external process:
   tests, builds, linters/formatters, package managers, one-off system inspection,
   or git operations not covered by git operation=status/diff.
   NEVER use run_shell_command for ls/find/tree/pwd/cat/head/tail/sed/grep/rg-style work:
   use workspace / buffer_inspect operations instead.
12. Use git operation=status/diff to review your changes before finishing.

PARALLELIZE: when operations are independent, issue them in a single message.

[project_context]"
  "System prompt for the `agent-shell' preset.")

;; --- Agentic presets ---

(defconst my/gptel--agent-read-slim-tools
  '("workspace" "beads" "git")
  "Slim `agent-read' tool roster. Keep under 8 tools for token-cheap sessions.")

(defconst my/gptel--agent-edit-slim-tools
  '("workspace" "buffer_edit" "buffer_inspect" "git")
  "Slim `agent-edit' tool roster. Keep under 8 tools for token-cheap sessions.")

(defconst my/gptel--agent-read-full-tools
  '("workspace" "buffer_inspect" "tool_output"
    "beads" "knowledge" "git")
  "Broad read-only `agent-read-full' tool roster for intentional heavy sessions.")

(defconst my/gptel--agent-edit-full-tools
  '("workspace" "buffer_inspect" "tool_output"
    "beads" "knowledge" "delegate_agent"
    "buffer_edit" "file_write" "git")
  "Broad edit `agent-edit-full' tool roster for intentional heavy sessions.")

(gptel-make-preset 'agent-read
  :description "Slim read-only agent: scoped search/read and git review. No edits, no shell."
  :pre #'my/gptel--agent-preset-preflight-setup
  :backend "Claude"
  :model 'claude-sonnet-4-6
  :system (lambda ()
            (string-replace "[project_context]"
                            (my/gptel--agent-context-string)
                            my/gptel--agent-read-system))
  :tools my/gptel--agent-read-slim-tools)

(gptel-make-preset 'agent-read-full
  :description "Full read-only agent: broad search/read, Beads, skills, diagnostics, and git review."
  :pre #'my/gptel--agent-preset-preflight-setup
  :backend "Claude"
  :model 'claude-sonnet-4-6
  :system (lambda ()
            (string-replace "[project_context]"
                            (my/gptel--agent-context-string)
                            my/gptel--agent-read-full-system))
  :tools my/gptel--agent-read-full-tools)

(gptel-make-preset 'agent-edit
  :description "Slim edit agent: scoped search/read/edit/verify. No shell or heavy helpers."
  :pre #'my/gptel--agent-edit-preflight-setup
  :backend "Claude"
  :model 'claude-sonnet-4-6
  :system (lambda ()
            (string-replace "[project_context]"
                            (my/gptel--agent-context-string)
                            my/gptel--agent-edit-system))
  :tools my/gptel--agent-edit-slim-tools)

(gptel-make-preset 'agent-edit-full
  :description "Full edit agent: read, modify buffers, create files, delegate, use skills/Beads, verify. No shell."
  :pre #'my/gptel--agent-edit-preflight-setup
  :backend "Claude"
  :model 'claude-sonnet-4-6
  :system (lambda ()
            (string-replace "[project_context]"
                            (my/gptel--agent-context-string)
                            my/gptel--agent-edit-full-system))
  :tools my/gptel--agent-edit-full-tools)

(gptel-make-preset 'agent-shell
  :description "Shell-capable agent: full edit plus arbitrary shell commands."
  :parents 'agent-edit-full
  :system (lambda ()
            (string-replace "[project_context]"
                            (my/gptel--agent-context-string)
                            my/gptel--agent-shell-system))
  :tools '(:append ("run_shell_command")))

(gptel-make-preset 'git-review
  :description "Git review agent: status and diff only -- no edits, no file reads."
  :backend "Claude"
  :model 'claude-sonnet-4-6
  :system "You are a code reviewer.

WORKFLOW:
1. Start by calling git operation=status to see the working-tree state.
2. Call git operation=diff (and staged=true if anything is staged) to fetch the actual changes -- do NOT wait for the user to paste output.
3. Issue independent git calls in parallel in a single message when possible.
4. If git diff returns :TOOL_OUTPUT_REF:, use tool_output operation=search or bounded operation=fetch ranges to inspect the relevant stored diff.
5. Then provide concise, actionable feedback: what changed, what looks risky, what's missing (tests, docs, edge cases)."
  :tools '("git" "tool_output"))

(my/gptel--update-subagent-tool-enum)

;; GPTel config block end

;; Integrations
(require 'gptel-integrations)

(provide 'init-gptel)
