;;; init-gptel.el --- GPTel configuration (extracted from emacs_asahi)
; Created on: Sat 13 Sep 2025
; Last Updated: Sun 14 Sep 2025

;;; GPTel specific configurations ;;;
;;
(setq gptel-default-mode 'org-mode)

;; Configure Anthropic Claude
;; API key will be read from '~/.authinfo.gpg'
(gptel-make-anthropic "Claude"
  :stream t
  :key gptel-api-key)

;; Configure Google Gemini
;; API key will be read from '~/.authinfo.gpg'
(gptel-make-gemini "Gemini"
  :stream t
  :key gptel-api-key
  :models '(gemini-2.5-pro))

;; Configure Deepseek
;; API key will be read from '~/.authinfo.gpg'
(gptel-make-deepseek "DeepSeek"
  :stream t
  :key gptel-api-key)

;; Configure Github Copilot Chat
;; no API key; browser auth login required
(gptel-make-gh-copilot "Copilot")

;; Configure xAI Grok
;; API key will be read from '~/.authinfo.gpg'
(gptel-make-xai "xAI"
  :stream t
  :key gptel-api-key
  :models '(grok-4-0709))

;; Configure Alibaba Qwen3
;; API key will be read from '~/.authinfo.gpg'
(gptel-make-openai "Alibaba"
  :stream t
  :key gptel-api-key
  :protocol "https"
  :host "dashscope-intl.aliyuncs.com"
  :endpoint "/compatible-mode/v1/chat/completions"
  :models '(qwen3-coder-plus))

;; Helper functions for gptel custom tools

(defmacro my/gptel--with-buffer-safety (buffer-expr error-label &rest body)
  "Execute BODY with current-buffer set to BUFFER-EXPR.
On error, return a formatted error message using ERROR-LABEL."
  (declare (indent 2))
  `(condition-case err
       (with-current-buffer ,buffer-expr
         ,@body)
     (error (format "Error %s: %s" ,error-label (error-message-string err)))))

(defun my/gptel--indent-text-to-column (text target-col)
  "Indent each non-empty line in TEXT to match TARGET-COL.
If TEXT already starts with whitespace, return it unchanged."
  (if (string-match "^[ \t]" text)
      text
    (mapconcat (lambda (line)
                 (if (string-empty-p (string-trim line))
                     line
                   (concat (make-string target-col ?\s) line)))
               (split-string text "\n")
               "\n")))

(defun my/gptel--check-insertion-context (prev-line current-line)
  "Return a warning string if inserting between PREV-LINE and CURRENT-LINE
would break syntactic constructs.  Returns nil if the context is safe."
  (when (and current-line prev-line)
    (cond
     ((and (string-match ")\\s-*$" prev-line)
           (not (string-match ";;" prev-line))
           (not (string-match ";;" current-line)))
      (format "Warning: Inserting between case pattern '%s' and its closing ';;' may break syntax. "
              (string-trim prev-line)))
     ((and (string-match "\\(if\\|while\\|for\\|case\\)\\s-" current-line)
           (not (string-match ";;\\s-*$" current-line)))
      (format "Warning: Line appears to be an incomplete statement: '%s'. "
              (string-trim current-line))))))

(defun my/gptel--buffer-insert (buffer text start-line position)
  "Insert TEXT in BUFFER at START-LINE, either before or after the line.
POSITION should be \"before\" or \"after\"; defaults to \"before\".
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
      (let* ((current-line (string-trim (thing-at-point 'line t)))
             (prev-line (save-excursion
                          (forward-line -1)
                          (string-trim (thing-at-point 'line t))))
             (warning-msg (my/gptel--check-insertion-context prev-line current-line))
             (target-indent (save-excursion
                              (beginning-of-line)
                              (skip-chars-forward " \t")
                              (current-column)))
             (formatted-text (my/gptel--indent-text-to-column text target-indent)))
        (if (string= position "after")
            (progn (end-of-line) (insert "\n" formatted-text))
          (progn (beginning-of-line) (insert formatted-text "\n")))
        (format "%sInserted text %s line %d in buffer %s"
                (or warning-msg "")
                (or position "before")
                start-line
                buffer))))))

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
 :name "modify_buffer"
 :function (lambda (buffer operation &optional text start-line end-line position)
             (condition-case err
                 (with-current-buffer (get-buffer-create buffer)
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
                       (_ (format "Error: Unknown operation '%s'. Use 'insert', 'replace', or 'delete'" operation)))))
               (error (format "Error in modify_buffer: %s" (error-message-string err)))))
 :description "[STEP 3 of 4] Modify an Emacs buffer by inserting, replacing, or deleting text at specific lines. Use this AFTER search_buffer_text (step 1) and show_buffer_context (step 2), then follow with save_buffer (step 4). IMPORTANT: 1) Only add NEW content that doesn't already exist. 2) Indentation is automatically matched to surrounding code. 3) Don't break syntax - avoid inserting between incomplete code blocks. 4) Based on context analysis, choose safe insertion boundaries."
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
               :description "For insert only: 'before' (default) or 'after' the specified line."))
 :category "emacs")

(gptel-make-tool
 :name "show_buffer_context"
 :function (lambda (buffer line-number &optional context-lines)
             (my/gptel--with-buffer-safety (get-buffer buffer) "showing context"
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
                           (line-content (string-trim (thing-at-point 'line t))))
                       (push (format "%s%d: %s"
                                     (if (= current-line-num line-number) ">>> " "    ")
                                     current-line-num
                                     line-content) lines))
                     (forward-line 1))
                   (format "Context around line %d in buffer '%s' (lines %d-%d of %d):\n%s"
                           line-number buffer start-line end-line total-lines
                           (mapconcat 'identity (nreverse lines) "\n"))))))
 :description "[STEP 2 of 4] Show the context around a specific line in a buffer to analyze code structure, indentation, and existing content. Use this AFTER search_buffer_text (step 1) and BEFORE modify_buffer (step 3) to choose the optimal insertion point and understand proper indentation."
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
             (my/gptel--with-buffer-safety (get-buffer buffer) "searching buffer"
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
 :description "[STEP 1 of 4] Search for existing text in a buffer to avoid creating duplicates. Use this FIRST before any modifications to check if similar code already exists. Follow with show_buffer_context (step 2), modify_buffer (step 3), then save_buffer (step 4)."
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
             (my/gptel--with-buffer-safety (get-buffer buffer) "saving buffer"
               (if (buffer-file-name)
                   (progn
                     (save-buffer)
                     (format "Saved buffer '%s' to file: %s" buffer (buffer-file-name)))
                 (format "Buffer '%s' is not associated with a file. Use write-file to save it." buffer))))
 :description "[STEP 4 - SAVE] Save buffer changes to file. Use this AFTER modify_buffer (step 3) to persist your changes to disk. Always save after making modifications unless you plan to make multiple changes first."
 :args (list '(:name "buffer"
               :type "string"
               :description "The name of the buffer to save."))
 :category "emacs")

(gptel-make-tool
 :name "read_file"
 :description "[READ-ONLY] Open a file in Emacs and return its contents. Use this to examine file content before making modifications."
 :args (list '(:name "path" :type string :description "Path to the file (e.g. ~/notes/todo.txt)"))
 :function (lambda (path)
             (condition-case err
                 (let ((existing (get-file-buffer path))
                       (buf (find-file-noselect path)))
                   (unwind-protect
                       (with-current-buffer buf (buffer-string))
                     (unless existing
                       (kill-buffer buf))))
               (error (format "Error reading file '%s': %s" path (error-message-string err)))))
 :category "filesystem")

(gptel-make-tool
 :name "execute_safe_command"
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
 :description "Run a shell command safely and return both the command you used and its output."
 :args (list '(:name "command"
               :type "string"
               :description "The shell command to execute"))
 :category "shell")


(gptel-make-tool
 :name "check_parens"
 :function (lambda (buffer)
             (condition-case err
                 (with-current-buffer (get-buffer buffer)
                   (unless (buffer-live-p (get-buffer buffer))
                     (error "Buffer '%s' is not live." buffer))
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
 :category "emacs")

;; GPTel config block end

;; Integrations
(require 'gptel-integrations)

(provide 'init-gptel)
