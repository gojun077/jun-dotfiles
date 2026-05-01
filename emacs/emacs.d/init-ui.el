;;; init-ui.el --- GUI theme/font/UI tweaks -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded by emacs_asahi.  Handles theme + font setup.
;;
;; NOTE on themes and frames:
;; Emacs themes are GLOBAL session state -- `load-theme' and
;; `enable-theme' affect every frame in the session.  You CANNOT
;; give a GUI emacsclient frame and an `emacsclient -t' (TTY) frame
;; different themes from the same daemon.  See:
;;   https://emacs.stackexchange.com/q/2096
;;
;; Consequence: TTY emacsclient frames spawned from this daemon will
;; render with the same `leuven' theme as GUI frames.  If you ever
;; want differentiated GUI vs TTY rendering, the supported paths are:
;;   (a) use a theme whose face specs include display predicates like
;;       `(((type tty)) ...)' / `(((type graphic)) ...)' so a single
;;       global theme renders differently per frame, or
;;   (b) run two separate Emacs daemons (e.g. `emacs --bg-daemon=gui'
;;       and `emacs --bg-daemon=tty'), each with its own theme.
;;; Code:

(defface my-minibuffer-face
  '((t :inherit default))
  "Face for Minibuffer.")

(defun my/apply-fonts ()
  "Apply default/Korean/mode-line/minibuffer font faces.
Safe to call multiple times; safe on TTY frames (font attrs are
ignored by terminals)."
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

;; Global UI tweaks (whole session -- themes/toolbars are global).
(setq mouse-autoselect-window t)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Theme: loaded ONCE, globally.  See commentary above.
(load-theme 'leuven t)

;; Apply minibuffer face to every minibuffer
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (face-remap-add-relative 'default 'my-minibuffer-face)))

;; Font setup: under daemon the initial "frame" has no display, and
;; some font attribute lookups need a graphic frame to take effect
;; reliably.  So defer to `after-make-frame-functions' under daemon;
;; apply directly otherwise.
(defun my/apply-fonts-on-graphic-frame (frame)
  "Apply fonts when FRAME is graphic."
  (when (display-graphic-p frame)
    (with-selected-frame frame
      (my/apply-fonts))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/apply-fonts-on-graphic-frame)
  (my/apply-fonts))

;; font names on Linux: 'MonofurNerdFontMono', 'FiraMono'

(provide 'init-ui)
;;; init-ui.el ends here
