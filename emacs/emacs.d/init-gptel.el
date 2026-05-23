;;; init-gptel.el --- GPTel configuration (extracted from emacs_asahi)  -*- lexical-binding: t; -*-
; Created on: Sat 13 Sep 2025
; Last Updated: Sat 23 May 2026

;;; GPTel specific configurations ;;;
;;
(require 'gptel)

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
            gemini-3.1-flash-lite-preview))

;; Configure OpenAI subscription (OAuth)
(gptel-make-openai-oauth "OpenAI-pro")

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
  :models '(grok-4.20-0309-reasoning
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

(defun my/gptel--render-numbered-lines (label start-line end-line)
  "Render the current buffer as numbered lines, optionally paginated.
LABEL is shown in the header (typically a file path or buffer name).
START-LINE and END-LINE are 1-based and inclusive.  When START-LINE is
nil it defaults to 1; when END-LINE is nil it defaults to a 500-line
window starting at START-LINE.  Each output line is prefixed with
`<lineno>: ' to match the conventional agent Read tool format."
  (save-excursion
    (let* ((total (count-lines (point-min) (point-max)))
           (start (max 1 (or start-line 1)))
           (default-end (+ start 499))
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
            (let ((line-text (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
              (push (format "%d: %s" n line-text) lines))
            (forward-line 1)
            (setq n (1+ n)))
          (format "%s (lines %d-%d of %d):\n%s"
                  label start end total
                  (mapconcat 'identity (nreverse lines) "\n"))))))))

(defun my/gptel--buffer-edit-string (buffer old-str new-str replace-all)
  "Replace OLD-STR with NEW-STR in current buffer (named BUFFER).
If OLD-STR matches more than once and REPLACE-ALL is nil, return an
error string instead of replacing.  Returns a result string."
  (let ((case-fold-search nil)
        (count 0))
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
              count (if (= count 1) "" "s") buffer)))))

(defun my/gptel--buffer-insert (buffer text start-line position)
  "Insert TEXT verbatim in BUFFER at START-LINE.
POSITION should be \"before\" or \"after\"; defaults to \"before\".
TEXT is inserted exactly as provided -- no indentation adjustment.
Use the indent_region tool afterwards if reformatting is needed.
Assumes current buffer is the target buffer.  Returns a result string."
  (let ((total-lines (count-lines (point-min) (point-max))))
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
  (let ((total-lines (count-lines (point-min) (point-max))))
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
  (let ((total-lines (count-lines (point-min) (point-max))))
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

;; custom tools for use in 'gptel' mode

(gptel-make-tool
 :name "edit_buffer"
 :function (lambda (buffer old-str new-str &optional replace-all no-save)
             (condition-case err
                 (with-current-buffer (my/gptel--resolve-buffer buffer)
                   (let ((result (my/gptel--buffer-edit-string buffer old-str new-str replace-all)))
                     (when (and (not no-save) (buffer-file-name))
                       (save-buffer)
                       (setq result (concat result " (saved)")))
                     result))
               (error (format "Error in edit_buffer: %s" (error-message-string err)))))
 :description "Replace exact text in an Emacs buffer, then auto-save if the buffer visits a file.  Finds OLD_STR (must be unique unless REPLACE_ALL is true) and replaces it with NEW_STR.  Preferred over edit_buffer_by_line.  Tip: include enough surrounding context in OLD_STR to make it unique."
 :args (list '(:name "buffer"
               :type "string"
               :description "Buffer name or file path.  Use list_buffers to find valid names.")
             '(:name "old_str"
               :type "string"
               :description "Exact text to find in the buffer.  Whitespace and newlines must match exactly.  Must be unique within the buffer unless replace_all is true.")
             '(:name "new_str"
               :type "string"
               :description "Text to replace old_str with.  Use the empty string to delete.")
             '(:name "replace_all"
               :type "boolean"
               :description "When true, replace every occurrence of old_str.  Default: false (error if old_str matches more than once).")
             '(:name "no_save"
               :type "boolean"
               :description "When true, skip the automatic save.  Default: false (changes are saved immediately if the buffer visits a file)."))
 :confirm t
 :category "emacs")

(gptel-make-tool
 :name "edit_buffer_by_line"
 :function (lambda (buffer operation &optional text start-line end-line position no-save)
             (condition-case err
                 (with-current-buffer (my/gptel--resolve-buffer buffer)
                   (let ((result
                          (save-excursion
                            (pcase operation
                              ("insert"
                               (my/gptel--buffer-insert buffer text start-line position))
                              ("replace"
                               (if (not (and start-line end-line text))
                                   "Error: Replace operation requires start-line, end-line, and text parameters"
                                 (my/gptel--buffer-replace buffer text start-line end-line)))
                              ("delete"
                               (if (not (and start-line end-line))
                                   "Error: Delete operation requires start-line and end-line parameters"
                                 (my/gptel--buffer-delete buffer start-line end-line)))
                              (_ (format "Error: Unknown operation '%s'. Use 'insert', 'replace', or 'delete'" operation))))))
                     (when (and (not no-save)
                                (buffer-file-name)
                                (not (string-prefix-p "Error" result)))
                       (save-buffer)
                       (setq result (concat result " (saved)")))
                     result))
               (error (format "Error in edit_buffer_by_line: %s" (error-message-string err)))))
 :description "FALLBACK: edit a buffer by line number (insert/replace/delete), then auto-save if the buffer visits a file (unless no_save=true).  Prefer edit_buffer (string-replace) for almost all edits.  Use this only when string-replace is not viable, e.g., inserting into an empty file at a specific line.  Note: line numbers shift after edits, so re-read the buffer between calls."
 :args (list '(:name "buffer"
               :type "string"
               :description "The name of the buffer to modify.")
             '(:name "operation"
               :type "string"
               :description "Operation: 'insert', 'replace', or 'delete'.")
             '(:name "text"
               :type "string"
               :description "Text to insert/replace (not used for delete).")
             '(:name "start-line"
               :type "number"
               :description "Line number (1-based). For insert: target line. For replace/delete: start of range.")
             '(:name "end-line"
               :type "number"
               :description "End line number (1-based). Required for replace/delete operations.")
             '(:name "position"
               :type "string"
               :description "For insert only: 'before' (default) or 'after' the specified line.")
             '(:name "no_save"
               :type "boolean"
               :description "When true, skip the automatic save.  Default: false (changes are saved immediately if the buffer visits a file)."))
 :confirm t
 :category "emacs")

(gptel-make-tool
 :name "show_buffer_context"
 :function (lambda (buffer line-number &optional context-lines)
             (my/gptel--with-buffer-safety (my/gptel--resolve-buffer buffer) "showing context"
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
                   (format "Context around line %d in buffer '%s' (lines %d-%d of %d):\n%s"
                           line-number buffer start-line end-line total-lines
                           (mapconcat 'identity (nreverse lines) "\n"))))))
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
 :category "emacs")

(gptel-make-tool
 :name "search_buffer_text"
 :function (lambda (buffer search-text)
             (my/gptel--with-buffer-safety (my/gptel--resolve-buffer buffer) "searching buffer"
               (save-excursion
                 (goto-char (point-min))
                 (let ((matches '()))
                   (while (search-forward search-text nil t)
                     (let ((line-num (line-number-at-pos))
                           (line-content (string-trim (thing-at-point 'line t))))
                       (push (format "Line %d: %s" line-num line-content) matches)))
                   (if matches
                       (format "Found %d occurrences of '%s' in buffer '%s':\n%s"
                               (length matches) search-text buffer
                               (mapconcat 'identity (nreverse matches) "\n"))
                     (format "Text '%s' not found in buffer '%s'" search-text buffer))))))
 :description "Search for text in a buffer and return all matching line numbers and content.  Useful before editing to confirm the target text exists and is unique."
 :args (list '(:name "buffer"
               :type "string"
               :description "The name of the buffer to search in.")
             '(:name "search-text"
               :type "string"
               :description "The text to search for (e.g., a function name or key phrase)."))
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
 :description "Save buffer changes to disk.  edit_buffer and edit_buffer_by_line auto-save after each edit, so this is mainly needed when batching multiple edits with no_save=true before a single save."
 :args (list '(:name "buffer"
               :type "string"
               :description "The name of the buffer to save."))
 :confirm t
 :category "emacs")

(gptel-make-tool
 :name "read_file"
 :description "[READ-ONLY] Open a file in Emacs and return its contents with each line prefixed by `<lineno>: ' (matches the conventional Read-tool format).  By default returns the first 500 lines; pass START-LINE and/or END-LINE to paginate through larger files."
 :args (list '(:name "path"
               :type "string"
               :description "Path to the file (e.g. ~/notes/todo.txt).")
             '(:name "start-line"
               :type "number"
               :description "First line to return (1-based, inclusive).  Defaults to 1.")
             '(:name "end-line"
               :type "number"
               :description "Last line to return (1-based, inclusive).  Defaults to start-line + 499 (a 500-line window)."))
 :function (lambda (path &optional start-line end-line)
             (condition-case err
                 (let ((existing (get-file-buffer path))
                       (buf (find-file-noselect path)))
                   (unwind-protect
                       (with-current-buffer buf
                         (my/gptel--render-numbered-lines path start-line end-line))
                     (unless existing
                       (kill-buffer buf))))
               (error (format "Error reading file '%s': %s" path (error-message-string err)))))
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
 :category "filesystem")

(gptel-make-tool
 :name "list_buffers"
 :function (lambda ()
             (let* ((bufs (seq-filter
                           (lambda (b)
                             (not (string-prefix-p " " (buffer-name b))))
                           (buffer-list)))
                    (entries
                     (mapcar (lambda (buf)
                               (with-current-buffer buf
                                 (format "  %-35s  %-20s  %7d bytes  %s"
                                         (buffer-name)
                                         (symbol-name major-mode)
                                         (buffer-size)
                                         (or (buffer-file-name) "(no file)"))))
                             bufs)))
               (format "%d buffers:\n%s"
                       (length entries)
                       (mapconcat #'identity entries "\n"))))
 :description "List all visible Emacs buffers with their name, major mode, size, and associated file path.  Use this to find the exact buffer name to pass to edit_buffer and similar tools.  Buffer names and file paths are both accepted by any tool with a 'buffer' argument."
 :args '()
 :category "emacs")

(gptel-make-tool
 :name "run_shell_command"
 :function (lambda (command)
             (condition-case err
                 (let* ((temp-buffer (generate-new-buffer " *shell-output*"))
                        (exit-code (call-process-shell-command command nil temp-buffer t))
                        (output (with-current-buffer temp-buffer (buffer-string))))
                   (kill-buffer temp-buffer)
                   (let ((trimmed-output (string-trim output)))
                     (format "```bash\n%s\n```\n\n**Exit code:** %d\n\n**Output:**\n```\n%s\n```"
                             command exit-code
                             (if (string-empty-p trimmed-output)
                                 "(no output)"
                               trimmed-output))))
               (error (format "Command: %s\nError: %s" command (error-message-string err)))))
 :description "Run a shell command and return its output and exit code.  Executes via the system shell with no sandboxing -- confirm before running destructive or side-effecting commands."
 :args (list '(:name "command"
               :type "string"
               :description "The shell command to execute."))
 :confirm t
 :category "shell")


(gptel-make-tool
 :name "indent_region"
 :function (lambda (buffer start-line end-line)
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
                           start-line end-line buffer major-mode))))))
 :description "Re-indent lines START-LINE through END-LINE in BUFFER using the buffer's major-mode indentation logic (calls `indent-region').  Use AFTER edit_buffer / edit_buffer_by_line if inserted code needs reformatting.  Note: indentation is the major mode's interpretation, which may differ from what you wrote -- re-read the buffer afterwards to confirm."
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
                                   (list "--line-number" "--no-heading" "--color=never"
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
                        ((= exit-status 1)
                         (format "No matches for '%s' in %s"
                                 pattern (abbreviate-file-name search-dir)))
                        ((not (zerop exit-status))
                         (format "Error: rg exited with status %d.\n%s"
                                 exit-status trimmed))
                        (t
                         (let ((lines (split-string trimmed "\n" t))
                               (max-show 50))
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
                                              "\n")))))))))
               (error (format "Error searching project: %s" (error-message-string err)))))
 :description "Search for a regex pattern across project files using ripgrep. Respects .gitignore. Returns matching lines with file path and line number."
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
                                        "\n"))))
               (error (format "Error listing files: %s" (error-message-string err)))))
 :description "List files in the project, optionally filtered by a regex pattern. Respects .gitignore when projectile is available."
 :args (list '(:name "pattern"
               :type "string"
               :description "Optional regex to filter filenames (e.g. '\\.el$', 'src/').")
             '(:name "dir"
               :type "string"
               :description "Directory to list (default: projectile project root or current directory)."))
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
                       "Working tree clean. No staged, unstaged, or untracked changes."
                     (format "```\n%s\n```" (string-trim output))))
               (error (format "Error getting git status: %s" (error-message-string err)))))
 :description "Show git status in porcelain format. See https://git-scm.com/docs/git-status#_porcelain_format_format for key."
 :args nil
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
                       (format "No %schanges%s."
                               (if staged "staged " "")
                               (if path (format " for %s" path) ""))
                     (format "```diff\n%s\n```" (string-trim output))))
               (error (format "Error getting git diff: %s" (error-message-string err)))))
 :description "Show git diff of unstaged changes (or staged if STAGED is non-nil). Optionally limit to PATH."
 :args (list '(:name "staged"
               :type "boolean"
               :description "Show staged changes instead of unstaged.")
             '(:name "path"
               :type "string"
               :description "Limit diff to a specific file or directory."))
 :category "git")

(gptel-make-tool
 :name "buffer_diagnostics"
 :function (lambda (buffer)
             (condition-case err
                 (with-current-buffer (my/gptel--resolve-buffer buffer)
                   (cond
                    ((not (bound-and-true-p flymake-mode))
                     (format "flymake-mode is not active in buffer '%s'." buffer))
                    ((null (flymake-diagnostics))
                     (format "No diagnostics in buffer '%s'." buffer))
                    (t
                     (let* ((diags (flymake-diagnostics))
                            (grouped (seq-group-by #'flymake-diagnostic-type diags)))
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
                                ""))))))
               (error (format "Error checking diagnostics: %s" (error-message-string err)))))
 :description "Show flymake diagnostics (errors, warnings, notes) for a buffer, grouped by severity. Requires flymake-mode to be active in the buffer."
 :args (list '(:name "buffer"
               :type "string"
               :description "The name of the buffer to check diagnostics for."))
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
                      (funcall callback partial)))
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

CRITICAL: edit_buffer and edit_buffer_by_line both auto-save the buffer after
each edit (unless no_save=true).  Use save_buffer explicitly only when
deliberately batching several no_save=true edits before a single save.

WORKFLOW:
1. Use list_project_files or search_project to discover relevant files.
2. Use read_file (full file first!) or show_buffer_context before editing.
3. Use delegate_agent with subagent_type=agent-read for independent, context-cheap research.
4. Use open_file to load a file into a live buffer; pass the returned buffer name to edit_buffer.
5. Prefer edit_buffer (string replace) over edit_buffer_by_line -- line numbers shift after every edit.
   - old_str must uniquely match the target text. Include enough surrounding context.
   - Do NOT include the `<n>: ' line-number prefix that read_file adds -- strip it from old_str.
   - Pass replace_all=true only when intentionally replacing every occurrence.
6. Use check_parens / byte_compile_file / buffer_diagnostics to verify after edits.
7. Use git_status / git_diff to review your changes before finishing.

PARALLELIZE: when reads are independent, issue them in a single message.

[project_context]"
  "System prompt for the `agent-edit' preset.")

(defconst my/gptel--agent-shell-system
  "You are an Emacs coding assistant with full shell access.

CRITICAL: edit_buffer and edit_buffer_by_line both auto-save the buffer after
each edit (unless no_save=true).  Use save_buffer explicitly only when
deliberately batching several no_save=true edits before a single save.

WORKFLOW:
1. Use list_project_files or search_project to discover relevant files.
2. Use read_file (full file first!) or show_buffer_context before editing.
3. Use delegate_agent with subagent_type=agent-read for independent, context-cheap research.
4. Use open_file to load a file into a live buffer; pass the returned buffer name to edit_buffer.
5. Prefer edit_buffer (string replace) over edit_buffer_by_line -- line numbers shift after every edit.
   - old_str must uniquely match the target text. Include enough surrounding context.
   - Do NOT include the `<n>: ' line-number prefix that read_file adds -- strip it from old_str.
6. Use check_parens / byte_compile_file / buffer_diagnostics to verify after edits.
7. Reserve run_shell_command for git, build tools, package managers, and system commands.
   NEVER use run_shell_command for file operations -- use read_file / search_project / list_project_files.
8. Use git_status / git_diff to review your changes before finishing.

PARALLELIZE: when operations are independent, issue them in a single message.

[project_context]"
  "System prompt for the `agent-shell' preset.")

;; --- Agentic presets ---

(gptel-make-preset 'agent-read
  :description "Read-only agent: search, read, plan, review. No edits, no shell."
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
  :pre #'my/gptel--update-subagent-tool-enum
  :backend "Claude"
  :model 'claude-sonnet-4-6
  :system (lambda ()
            (string-replace "[project_context]"
                            (my/gptel--project-context-string)
                            my/gptel--agent-edit-system))
  :tools '("read_file" "show_buffer_context" "search_buffer_text"
           "search_project" "list_project_files" "list_buffers"
           "delegate_agent"
           "open_file" "edit_buffer" "edit_buffer_by_line" "save_buffer"
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
