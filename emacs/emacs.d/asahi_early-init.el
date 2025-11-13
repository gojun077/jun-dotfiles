; -*- mode: emacs-lisp -*-

;; 'early-init.el' for Asahilinux Fedora Remix
;; Created on: Sat 28 Jun 2025
;; Created by: gopeterjun@naver.com
;; Last Updated: Sat 13 Sep 2025
;;
;; Settings loaded before package system and GUI are initialized
;; These configs are for Emacs 30.1 and higher.
;;; Code:


;; Completely disable package.el
(setq package-enable-at-startup nil)
(setq package--init-file-ensured t)
(setq package-quickstart nil)

;; enable 'use-package-report'. Run with 'M-x use-package-report'
(setq use-package-compute-statistics t)
;; Optional noise control:
(setq use-package-verbose t
      use-package-minimum-reported-time 0.01)  ;; seconds

;; reduce startup Garbage Collection for clearer, faster startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; run emacs startup profiler with 'EMACS_PROFILE_STARTUP=1 emacs'
(when (getenv "EMACS_PROFILE_STARTUP")
  (require 'profiler)
  (profiler-start 'cpu+mem)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (profiler-stop)
              (unless (daemonp) (profiler-report)))))

;; print how long it takes for emacs to start
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1)
            (message "Emacs ready in %s (%.3fs, %d GCs)"
                     (emacs-init-time)
                     (float-time
                      (time-subtract
                       after-init-time
                       before-init-time))
                     gcs-done))
          t)  ;; append so it runs after the default startup message

;; --- use straight.el instead of package.el ---
;;
;; Disable straight.el autoload caching - lexical binding is not
;; available when straight-cache-autoloads is enabled, and this
;; can cause  'symbol value void' errors when loading packages
(setq straight-cache-autoloads nil)
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

; don't show menu bar
(menu-bar-mode 0)
; don't show scrollbar
(scroll-bar-mode 0)
