;;; init-ui.el --- GUI theme/font/UI tweaks -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded by emacs_asahi.  Handles GUI-only setup; safe under daemon
;; via `after-make-frame-functions'.
;;; Code:

(defface my-minibuffer-face
  '((t :inherit default))
  "Face for Minibuffer.")

(defun my/apply-fonts ()
  "Apply default/Korean/mode-line/minibuffer font faces."
  (let ((default-font "MonofurNerdFontMono")
        (korean-font "NanumGothic"))
    ;; Default font
    (set-face-attribute 'default nil
                        :family default-font
                        :height 150)
    ;; Korean font
    (set-fontset-font t 'hangul (font-spec :family korean-font))
    ;; Mode-line font
    (if (facep 'mode-line-active)
        (set-face-attribute 'mode-line-active nil
                            :family default-font
                            :height 110)
      (set-face-attribute 'mode-line nil
                          :family default-font
                          :height 110))
    (set-face-attribute 'mode-line-inactive nil
                        :family default-font
                        :height 110)
    ;; Minibuffer font
    (set-face-attribute 'my-minibuffer-face nil
                        :family default-font
                        :height 120)))

(defun my/gui-setup (&optional frame)
  "Per-frame UI tweaks.  Safe under daemon via `after-make-frame-functions'.
FRAME, when given, is the newly-created frame to configure."
  (with-selected-frame (or frame (selected-frame))
    (if (display-graphic-p)
        (progn
          (scroll-bar-mode -1)
          (tool-bar-mode -1)
          (setq mouse-autoselect-window t)
          ;; Theme must be loaded before fonts to avoid theme overriding faces
          (load-theme 'leuven t)
          (my/apply-fonts))
      ;; Terminal frame
      (load-theme 'adwaita t))))

;; Apply minibuffer face to every minibuffer
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (face-remap-add-relative 'default 'my-minibuffer-face)))

;; Daemon-aware setup: per-frame hook for daemon, one-shot otherwise
(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/gui-setup)
  (my/gui-setup))

;; font names on Linux: 'MonofurNerdFontMono', 'FiraMono'

(provide 'init-ui)
;;; init-ui.el ends here
