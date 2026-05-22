;;; lint-harness.el --- Batch lint load-path setup  -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal harness for `bin/elisp-lint'.  This runs under `emacs --batch -Q',
;; so it must not load the user's init files or install/build packages.  It
;; only exposes already-installed package artifacts to byte compilation.

;;; Code:

(require 'seq)

(defun lint-harness--existing-subdirs (directory)
  "Return existing non-hidden subdirectories of DIRECTORY."
  (when (file-directory-p directory)
    (seq-filter #'file-directory-p
                (directory-files directory t "\\`[^.]"))))

(defun lint-harness--add-package-dir (directory)
  "Add DIRECTORY to `load-path' and load its generated autoloads if present."
  (add-to-list 'load-path directory t)
  (let* ((name (file-name-nondirectory (directory-file-name directory)))
         (autoloads (expand-file-name (concat name "-autoloads.el") directory)))
    (when (file-readable-p autoloads)
      (load autoloads nil 'nomessage))))

(defun lint-harness--add-package-subdirs (directory)
  "Expose package subdirectories under DIRECTORY to byte compilation."
  (dolist (subdir (lint-harness--existing-subdirs directory))
    (lint-harness--add-package-dir subdir)))

(let ((user-dir (file-name-as-directory
                 (or (getenv "ELISP_LINT_USER_EMACS_DIRECTORY")
                     user-emacs-directory))))
  (lint-harness--add-package-subdirs
   (expand-file-name "straight/build" user-dir))
  (lint-harness--add-package-subdirs
   (or (getenv "ELISP_LINT_PACKAGE_USER_DIR")
       (expand-file-name "elpa" user-dir))))

(provide 'lint-harness)

;;; lint-harness.el ends here
