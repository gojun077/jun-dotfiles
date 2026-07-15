;;; init-notmuch.el --- Remote notmuch mail configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Run the notmuch 0.38.3 Emacs frontend locally while all mail and the
;; matching notmuch CLI/database remain on ocloud-arm.

;;; Code:

(declare-function straight-use-package "straight")
(defvar notmuch-command)
(defvar notmuch-fcc-dirs)
(defvar notmuch-saved-searches)
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
        (:name "Trash" :query "path:\"[Gmail]/Trash/**\"" :key "t")))

(provide 'init-notmuch)
;;; init-notmuch.el ends here
