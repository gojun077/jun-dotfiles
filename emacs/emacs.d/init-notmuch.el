;;; init-notmuch.el --- Remote notmuch mail configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Run the notmuch 0.38.3 Emacs frontend locally while all mail and the
;; matching notmuch CLI/database remain on ocloud-arm.

;;; Code:

(declare-function straight-use-package "straight")
(declare-function notmuch-search-next-thread "notmuch")
(declare-function notmuch-search-tag "notmuch" (tag-changes &optional beg end only-matched))
(declare-function notmuch-show-next-open-message "notmuch-show" (&optional pop-at-end))
(declare-function notmuch-show-tag-message "notmuch-show" (&rest tag-changes))
(defvar notmuch-command)
(defvar notmuch-fcc-dirs)
(defvar notmuch-search-mode-map)
(defvar notmuch-saved-searches)
(defvar notmuch-show-mode-map)
(defvar notmuch-show-logo)

(straight-use-package
 '(notmuch
   :type git
   :host github
   :repo "notmuch/notmuch"
   :tag "0.38.3"
   :files ("emacs/*.el")))

(autoload 'notmuch "notmuch" "Open the Notmuch mail interface." t)
(autoload 'notmuch-hello "notmuch-hello" "Show the Notmuch welcome screen." t)
(autoload 'notmuch-search "notmuch" "Search mail with Notmuch." t)

(defun pj/notmuch-search-delete-thread ()
  "Hide the current thread with the `deleted' tag and advance."
  (interactive)
  (notmuch-search-tag '("+deleted"))
  (notmuch-search-next-thread))

(defun pj/notmuch-show-delete-message ()
  "Hide the current message with the `deleted' tag and advance."
  (interactive)
  (notmuch-show-tag-message "+deleted")
  (notmuch-show-next-open-message t))

(with-eval-after-load 'notmuch
  (define-key notmuch-search-mode-map (kbd "d")
              #'pj/notmuch-search-delete-thread))

(with-eval-after-load 'notmuch-show
  (define-key notmuch-show-mode-map (kbd "d")
              #'pj/notmuch-show-delete-message))

(setq notmuch-command (expand-file-name "~/.local/bin/notmuch-remote"))
;; Gmail will place sent messages in All Mail during the next mbsync run.
;; Avoid creating another copy through notmuch Fcc.
(setq notmuch-fcc-dirs nil)
(setq notmuch-show-logo nil)
(setq notmuch-saved-searches
      '((:name "Unread" :query "tag:unread" :key "u")
        (:name "Flagged" :query "tag:flagged" :key "f")
        (:name "Recent" :query "date:1_month.." :key "r")
        (:name "All mail" :query "*" :key "a")
        (:name "Deleted" :query "tag:deleted" :key "d")
        (:name "Mail processing"
         :query "tag:archive-pending or tag:gmail-trash-pending or tag:purge-pending"
         :key "p")
        (:name "Mail errors"
         :query "tag:archive-error or tag:purge-error" :key "e")))

(provide 'init-notmuch)
;;; init-notmuch.el ends here
