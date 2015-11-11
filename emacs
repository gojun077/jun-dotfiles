;; -*- mode: emacs-lisp -*-

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/elpa") ;;personal elisp libs

(require 'cl)
(defvar gojun-pkglist
  '(color-theme-sanityinc-solarized
    fill-column-indicator
    flycheck
    markdown-mode
    org-trello
    oz
    paredit
    racket-mode
    rw-hunspell
    rw-ispell
    rw-language-and-country-codes)
"list of packages to ensure are installed at launch")

(defun gojun-pkglist-installed-p ()
  (loop for p in gojun-pkglist
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (gojun-pkglist-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p gojun-pkglist)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'gojun-pkglist)

(require 'flycheck)
(require 'rw-hunspell)
(require 'rw-language-and-country-codes)
(require 'rw-ispell)
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;;mode settings
(column-number-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode t)
(tool-bar-mode 0)

;;Font settings
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
;; AUCTEX preview-latex font
(set-default 'preview-scale-function 1.2)

;;======================
;;   GLOBAL VARIABLES
;;======================
;formatting for C code
(setq c-default-style "linux" c-basic-offset 4)
;shell script mode formatting
(setq sh-basic-offset 2)
(setq sh-indentation 2)
;python settings
(setq python-shell-interpreter "ipython3")
(setq python-shell-interpreter-args "-i")
;C-\ language toggle
(setq default-input-method "korean-hangul")
(setq initial-frame-alist (quote ((fullscreen . maximized))))
(setq make-backup-files nil)               ;; disable temp files
(setq auto-save-default nil)               ;; disable autosaving
(setq ispell-program-name "hunspell")      ;; specify dictionary binary
(setq ispell-dictionary "en_US_hunspell")  ;; specify dictionary
(put 'upcase-region 'disabled nil)         ;; enable chg region to upper
(put 'downcase-region 'disabled nil)       ;; enable chg region to lower
;org-trello-mode formatting
(setq org-todo-keyword-faces
 (quote
  (("Backlog" . "black")
   ("Queue" . "blue")
   ("WIP" . "red")
   ("Done" . "green")
   ("Cancelled" . "gray"))))
;org-trello-mode keywords
(setq org-todo-keywords
 (quote
  ((sequence "Backlog" "Queue" "WIP" "DONE" "Cancelled"))))
;org-trello-mode keybinding
(setq org-trello-current-prefix-keybinding "C-c o")
;spellcheck dictionary settings
(setq rw-hunspell-default-dictionary "en_US_hunspell")
(setq rw-hunspell-dicpath-list (quote ("/usr/share/hunspell")))
(setq rw-hunspell-make-dictionary-menu t)
(setq rw-hunspell-use-rw-ispell t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Commands to run when Emacs launched in graphical mode
(when (display-graphic-p)
  (global-whitespace-mode 1) ;only run in graphical session
  (load-theme 'sanityinc-solarized-light))
;; Commands to run when Emacs launched in terminal mode
(unless (display-graphic-p)
  (load-theme 'adwaita))
