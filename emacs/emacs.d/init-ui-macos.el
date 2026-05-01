;;; init-ui-macos.el --- macOS GUI theme/font/UI tweaks -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded by emacs_mac.  macOS-specific counterpart to init-ui.el:
;; the Nerd Font ttf names on macOS differ from Linux, the default
;; font size is larger (Retina), and macOS frames have a menu-bar
;; that we want disabled.
;;
;; NOTE on themes and frames:
;; Emacs themes are GLOBAL session state -- `load-theme' and
;; `enable-theme' affect every frame in the session.  You CANNOT
;; give a GUI emacsclient frame and an `emacsclient -t' (TTY) frame
;; different themes from the same daemon.  See:
;;   https://emacs.stackexchange.com/q/2096
;;
;; Consequence: TTY emacsclient frames spawned from this daemon will
;; render with the same `ef-cyprus' theme as GUI frames.  If you ever
;; want differentiated GUI vs TTY rendering, the supported paths are:
;;   (a) use a theme whose face specs include display predicates like
;;       `(((type tty)) ...)' / `(((type graphic)) ...)' so a single
;;       global theme renders differently per frame, or
;;   (b) run two separate Emacs daemons (e.g. `emacs --bg-daemon=gui'
;;       and `emacs --bg-daemon=tty'), each with its own theme.
;;
;; Nerd Font ttf names on macOS are slightly different from Linux:
;; 'Monofur Nerd Font Mono' (macOS) vs 'MonofurNerdFontMono' (Linux).
;;; Code:

(defface my-minibuffer-face
  '((t :inherit default))
  "Face for Minibuffer.")

(defun my/apply-fonts-macos ()
  "Apply default/Korean/mode-line/minibuffer font faces for macOS.
Safe to call multiple times; safe on TTY frames (font attrs are
ignored by terminals)."
  (let ((default-font "Monofur Nerd Font Mono")
        (korean-font "NanumGothic"))
    ;; Default font (Retina-friendly larger height)
    (set-face-attribute 'default nil
                        :family default-font
                        :height 170)
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
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Org-heading scaling: ef-themes 2.0+ is built atop modus-themes and,
;; with `ef-themes-take-over-modus-themes-mode' enabled, respects the
;; `modus-themes-headings' alist.  This gives a single API for tuning
;; heading sizes/weights across both theme families.
;;
;; Each entry is `(LEVEL . (PROPS))' where PROPS may include a weight
;; symbol (e.g. `bold', `semibold', `light'), a float for `:height',
;; and the `variable-pitch' symbol to switch font family.
(setq modus-themes-headings
      '((1 . (bold 1.6))
        (2 . (bold 1.4))
        (3 . (semibold 1.25))
        (4 . (semibold 1.15))
        (5 . (semibold 1.1))
        (6 . (semibold 1.05))
        (agenda-date . (semibold 1.3))
        (agenda-structure . (light 1.5))
        (t . (semibold))))

;; Make ef-themes pick up `modus-themes-*' customization options
;; (introduced in ef-themes 2.0).
(when (fboundp 'ef-themes-take-over-modus-themes-mode)
  (ef-themes-take-over-modus-themes-mode 1))

;; Theme: ef-cyprus (light, Mediterranean palette).  Loaded ONCE,
;; globally.  See commentary above for why per-frame theming is not
;; supported.
(load-theme 'ef-cyprus :no-confirm)

;; Apply minibuffer face to every minibuffer
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (face-remap-add-relative 'default 'my-minibuffer-face)))

;; Font setup: under daemon the initial "frame" has no display, and
;; some font attribute lookups need a graphic frame to take effect
;; reliably.  So defer to `after-make-frame-functions' under daemon;
;; apply directly otherwise.
(defun my/apply-fonts-on-graphic-frame-macos (frame)
  "Apply fonts when FRAME is graphic."
  (when (display-graphic-p frame)
    (with-selected-frame frame
      (my/apply-fonts-macos))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              #'my/apply-fonts-on-graphic-frame-macos)
  (my/apply-fonts-macos))

(provide 'init-ui-macos)
;;; init-ui-macos.el ends here
