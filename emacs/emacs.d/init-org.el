;;; init-org.el --- org configuration (extracted from emacs_asahi)
; Original path to file: ~/.emacs.d/init-org.el
;
; Created on: Mon 15 Sep 2025
; Last Updated: Mon 15 Sep 2025
;
;------------------------------------------------------------
; org-mode settings
;------------------------------------------------------------
(setq org-directory "~/Documents/repos/personal/org")
(setq org-timer-default-timer 25)
; play sound when org-timer completes
(add-hook 'org-timer-done-hook (lambda () (interactive)
                                 (shell-command "/usr/bin/aplay $HOME/Downloads/media/ShipsBell.wav")))
(setq org-todo-keywords
      '((sequence "TODO(t!)" "Anywhere(!)" "Read/Review(!)" "Calls(!)"
                  "Computer(!)" "Errands(!)" "Home(!)" "Office(!)" "|"
                  "DONE(d!)")
        (sequence "|" "Canceled(c@)")))

; add timestamp CLOSED: '<YYYY-MM-DD DoW HH:MM>' for DONE status
(setq org-log-done 'time)

;; org-export settings
; see https://orgmode.org/manual/Export-Settings.html
(setq org-export-with-clocks t)
(setq org-export-with-drawers t)

; add 'CREATED: [timestamp]' when new TODO state is changed
(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (save-excursion
              (org-back-to-heading)
              (let ((text (buffer-substring (point) (progn (outline-next-heading) (point)))))
                (if (string-match "CREATED: " text)
                    ;; if string was found, noop
                    nil
                  ;; else, (1) move cursor back to current heading
                  (outline-previous-heading)
                  ;;(end-of-line)
                  ;; (2) insert timestamp on next line
                  (forward-line 1)
                  (insert (concat "CREATED: "
                      (format-time-string "[%Y-%m-%d %a %H:%M]"
                                          (current-time))
                      "\n")))))))

(setq org-refile-targets
      '(("~/Documents/repos/encrypted/gpj-org-mode-files/next_actions.org" :maxlevel . 1)
        ("~/Documents/repos/encrypted/gpj-org-mode-files/projects.org" :level . 1)
        ("~/Documents/repos/encrypted/gpj-org-mode-files/waiting_for.org" :level . 1)
        ("~/Documents/repos/encrypted/gpj-org-mode-files/someday_maybe.org" :level . 1)
        ("~/Documents/repos/personal/org/capture.org" :level . 1)
        ("~/Documents/repos/encrypted/gpj-org-mode-files/closed_cards.org" :level . 1)))
(setq org-default-notes-file (concat org-directory "/capture.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file "")
         "** %a %?\n %i\n")
        ("w" "Web site" entry (file "")
         "** %a :website:\n\n%U %?\n\n%:initial")))
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-timestamp-if-done t)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c i g c") #'org-id-get-create)
(setq org-id-link-to-org-use-id 'create-if-interactive)
