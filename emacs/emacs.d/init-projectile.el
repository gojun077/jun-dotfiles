;;; init-projectile.el --- Projectile config -*- lexical-binding: t; -*-
;;; Commentary:
;; Extracted from emacs_asahi.  Assumes straight.el and use-package
;; are already initialized (straight by early-init.el, use-package by
;; the main init).
;;; Code:

;; Disable built-in project.el in favor of projectile
(straight-use-package '(project :type built-in :no-build t))

(use-package projectile
  :straight t
  :init (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/Documents/repos")))

(provide 'init-projectile)
;;; init-projectile.el ends here
