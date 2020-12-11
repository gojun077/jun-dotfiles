; -*- mode: emacs-lisp -*-

;; jun's_emacs_file --- Summary
;; Jun Go gojun077@gmail.com
;; Last Updated 2020.08.08

;;; Code:
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/elpa") ;;personal elisp libs


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(defvar gojun-pkglist
 '(ansible
   color-theme-sanityinc-solarized
   exec-path-from-shell
   fill-column-indicator
   flycheck
   flycheck-pyflakes
   flycheck-gometalinter
   go-mode
   go-playground
   magit
   markdown-mode
   rainbow-delimiters
   racket-mode
   realgud
   scala-mode
   smartparens
   visual-fill-column
   web-mode
   yaml-mode)
 "List of packages to ensure are installed at launch.")

(dolist (package gojun-pkglist)
  (use-package package))

(require 'flycheck)
(require 'flycheck-pyflakes)
(require 'smartparens-config)


;; turn on flychecking globally
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)

;; hooks for racket-mode
(add-hook 'racket-mode-hook #'rainbow-delimiters-mode)
(add-hook 'racket-mode-hook #'smartparens-mode)

;; make #! script files executable on save (chmod +x)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; use visual-fill-column mode with visual-line-mode
(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)

;; Capitalize keywords in SQL mode
(add-hook 'sql-mode-hook 'sqlup-mode)
;; Capitalize keywords in an interactive session (e.g. psql)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)

;; truncate long lines in sqli mode
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(defun go-mode-setup ()
  ;; run 'go fmt' on .go source files before save
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "env GOOS=linux GOARCH=amd64 go build -v && go test -v && go vet"))
)
(add-hook 'go-mode-hook 'go-mode-setup)

;; mode settings
; show col and line numbers
(column-number-mode 1)
; don't show menu bar
(menu-bar-mode 0)
; highlight parens
(show-paren-mode t)




;; Font settings
(let ((fontset "fontset-default"))
  (set-fontset-font fontset 'latin
                    '("monofur" . "unicode-bmp"))
  (set-fontset-font fontset 'hangul
                    '("NanumGothic" . "unicode-bmp"))
  (set-face-attribute 'default nil
                      :font fontset
                      :height 140))

;;=========================
;;   CUSTOM MENU OPTIONS
;;=========================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))
 '(package-selected-packages
   '(exec-path-from-shell use-package rainbow-delimiters smartparens magit visual-fill-column flycheck-gometalinter sqlup-mode flycheck-pyflakes go-mode go-playground color-theme-solarized web-mode ein yaml-mode rw-language-and-country-codes racket-mode markdown-mode flycheck fill-column-indicator color-theme-sanityinc-solarized ansible))
 '(python-shell-completion-native-disabled-interpreters '("pypy ipython3")))


;;======================
;;   GLOBAL VARIABLES
;;======================

; ensure GPG password input is handled by emacs during emacs session
(setenv "GPG_AGENT_INFO" nil)
; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
; But when I must use TAB, set width to 2 chars
(setq-default tab-width 2)
; formatting for C code
(setq c-default-style "linux" c-basic-offset 4)
; shell script mode formatting
(setq sh-basic-offset 2)
(setq sh-indentation 2)
; Python settings
; Note: recent versions of ipython and python interpreters
; don't support readline() and doctests do not show up
; in the emacs inferior process running python/ipython.
; For the time being, run python from CLI as follows:
;
; python -m doctest myprog.py -v
;
(setq python-shell-completion-native nil)
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt -i --pprint")
(setq ansi-color-for-comint-mode t)
(setq python-check-command "/usr/bin/pyflakes3")
(setq flycheck-python-pyflakes-executable "/usr/bin/pyflakes3")

; C-\ language toggle
(setq default-input-method "korean-hangul")
; Start emacs maximized
(setq initial-frame-alist (quote ((fullscreen . maximized))))
; create backups in $HOME/tmp
; bkup files will have '!' in place of directory separator '/'
(setq backup-directory-alist '(("" . "~/tmp")))
; Enable temp files and autosaving
(setq make-backup-files t)
(setq auto-save-default t)
; Enable C-l, C-u change region to lower/upper case
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
; Enable ido-mode
(ido-mode 1)
; enable ido flex matching
(setq ido-enable-flex-matching t)
; enable ido everywhere
(setq ido-everywhere t)
; use xetex to render pdf from LaTeX
(setq TeX-engine 'xetex)

; set PATH for emacs shell
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/go/bin:$HOME/goproj/bin"))
; set exec-path for emacs to include GOLANG binaries
(setq exec-path (append exec-path '("/usr/local/go/bin"
                                    "$HOME/go/bin")))
;; (use-package exec-path-from-shell
;;   :ensure t
;;   :defer  2
;;   :config
;;   (dolist (var '("GOPATH"))
;;     (add-to-list 'exec-path-from-shell-variables var))
;;   (exec-path-from-shell-initialize))

; bind 'M-x magit-status' to 'C-x g'
(global-set-key (kbd "C-x g") 'magit-status)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Commands to run when Emacs launched in graphical mode
(when (display-graphic-p)
; only run whitespace mode in graphical session
  (global-whitespace-mode 1)
  (load-theme 'sanityinc-solarized-light)
  ; don't show scrollbar
  (scroll-bar-mode -1)
  ; don't show toolbar
  (tool-bar-mode -1)
  ;; AUCTEX preview-latex font
  (set-default 'preview-scale-function 1.2))

;; Commands to run when Emacs launched in terminal mode
(unless (display-graphic-p)
  (load-theme 'adwaita))

(provide 'emacs)
;;; emacs ends here
