; -*- mode: emacs-lisp -*-

;; 'early-init.el' for Asahilinux Fedora Remix
;; Created on: Sat 28 Jun 2025
;; Created by: gopeterjun@naver.com
;; Last Updated: Sat 28 Jun 2025
;;
;; Settings loaded before package system and GUI are initialized
;; These configs are for Emacs 30.1 and higher.
;;; Code:

;; use straight.el instead of package.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; prevent package.el loading packages prior to their init-file loading
(setq package-enable-at-startup nil)

; don't show menu bar
(menu-bar-mode 0)
; don't show scrollbar
(scroll-bar-mode 0)
