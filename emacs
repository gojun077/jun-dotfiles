;; -*- mode: emacs-lisp -*-

(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                       '("melpa" . "http://melpa.org/packages")
                       '("marmalade" . "http://marmalade-repo.org/packages/")))

(require 'cl)
; (defvar gojun-pkglist
;   '(fill-column-indicator
;     auctex
;     color-theme-sanityinc-solarized
;     markdown-mode
;     org-trello
;     oz
;     paredit
;     racket-mode
;     rw-hunspell
;     rw-ispell
;     rw-language-and-country-codes)
; )

; (defun check-package-not-installed ()
;   (loop for p in gojun-pkglist
;         when (not (package-installed-p p)) do (return t)
;         finally (return nil)))
; (when (check-package-not-installed)
;   ;; Check for new packages (package versions)
;   (message "%s" "Get latest versions of all packages...")
;   (package-refresh-contents)
;   (message "%s" " done.")
;   ;; Install the missing packages
;   (dolist (p gojun-pkglist)
;     (when (not (package-installed-p p))
;       (package-install p))))
; 
(add-to-list 'load-path "~/.emacs.d/elpa") ;;personal elisp libs


(require 'rw-hunspell)
(require 'rw-language-and-country-codes)
(require 'rw-ispell)
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode 1)
 '(custom-safe-themes
   (quote
    ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(default-input-method "korean-hangul")
 '(global-whitespace-mode t)
 '(indent-tabs-mode nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(menu-bar-mode nil)
 '(org-todo-keyword-faces
   (quote
    (("Backlog" . "black")
     ("Queue" . "blue")
     ("WIP" . "red")
     ("Done" . "green")
     ("Cancelled" . "gray"))))
 '(org-todo-keywords
   (quote
    ((sequence "Backlog" "Queue" "WIP" "DONE" "Cancelled"))))
 '(package-selected-packages
   (quote
    (tracking rw-language-and-country-codes rw-ispell rw-hunspell quack paredit org-trello geiser fill-column-indicator color-theme-solarized color-theme-sanityinc-solarized auto-complete)))
 '(python-shell-interpreter "ipython3")
 '(python-shell-interpreter-args "-i")
 '(rw-hunspell-default-dictionary "en_US_hunspell")
 '(rw-hunspell-dicpath-list (quote ("/usr/share/hunspell")))
 '(rw-hunspell-make-dictionary-menu t)
 '(rw-hunspell-use-rw-ispell t)
 '(scroll-bar-mode nil)
 '(sh-basic-offset 2) ; offset for shellscript mode
 '(sh-indentation 2)  ; indentation for shellscript mode
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(defun xftp (&optional frame)
  "Return t if FRAME support XFT font backend."
  (let ((xft-supported))
    (mapc (lambda (x) (if (eq x 'xft) (setq xft-supported t)))
          (frame-parameter frame 'font-backend))
    xft-supported))

(when (xftp)
  (let ((fontset "fontset-default"))
    (set-fontset-font fontset 'latin
                      '("monofur" . "unicode-bmp"))
    (set-fontset-font fontset 'hangul
                      '("NanumGothic" . "unicode-bmp"))
    (set-face-attribute 'default nil
                        :font fontset
                        :height 110)))

(set-default 'preview-scale-function 1.2) ;; AUCTEX preview-latex font

;; GLOBAL VARIABLES

;formatting for C code
(setq c-default-style "linux"
          c-basic-offset 4)

(setq make-backup-files nil)               ;; disable temp files
(setq auto-save-default nil)               ;; disable autosaving
(setq ispell-program-name "hunspell")      ;; specify dictionary binary
(setq ispell-dictionary "en_US_hunspell")  ;; specify dictionary
(put 'upcase-region 'disabled nil)         ;; enable chg region to upper
(put 'downcase-region 'disabled nil)       ;; enable chg region to lower


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
