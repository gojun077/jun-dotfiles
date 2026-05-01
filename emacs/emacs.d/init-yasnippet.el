;;; init-yasnippet.el --- YASnippet configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; YASnippet setup with custom snippets directory under ~/.emacs.d/snippets/.
;; Loaded by emacs_asahi and emacs_mac.
;;
;; Snippet creation workflow:
;;   1. M-x yas-new-snippet: create a snippet interactively
;;   2. M-x yas-visit-snippet-file: edit an existing snippet
;;   3. M-x yas-reload-all: reload snippets after adding files manually
;;
;; For find-file directory-abbreviation snippets (expand via TAB in the
;; C-x C-f minibuffer), place snippet files under snippets/minibuffer-mode/.
;; Example snippet (snippets/minibuffer-mode/dotfiles):
;;
;;   # key: dotfiles
;;   # name: ~/dotfiles directory
;;   # --
;;   ~/dotfiles/
;;
;; Then TAB in the find-file minibuffer after typing "dotfiles" will expand
;; it to "~/dotfiles/".
;;; Code:

(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-global-mode 1)
  ;; yas-global-mode skips minibuffers by default; enable explicitly
  (add-hook 'minibuffer-setup-hook #'yas-minor-mode))

;; suppress warning from delete-minibuffer-contents during expansion;
;; deferred so custom-set-variables won't overwrite it
(add-hook 'after-init-hook
          (lambda ()
            (add-to-list 'warning-suppress-types '(yasnippet backquote-change))))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
