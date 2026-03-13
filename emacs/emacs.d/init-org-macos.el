;;; init-org-macos.el --- org configuration for MacOS
; Original path to file: ~/.emacs.d/init-org.el
;
; Created on: Sun 21 Sep 2025
; Last Updated: Fri 13 Mar 2026
;
;------------------------------------------------------------
; org-mode settings
;------------------------------------------------------------
(setq org-directory "~/Documents/repos/personal/org")
(setq org-timer-default-timer 25)
; play sound when org-timer completes
(add-hook 'org-timer-done-hook (lambda () (interactive)
                                 (shell-command "/usr/bin/afplay $HOME/Downloads/media/ShipsBell.wav")))
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
              ;; Determine the end of the current entry's body (before the next heading)
              (let ((end (save-excursion (outline-next-heading) (point))))
                ;; Check if a "CREATED:" line already exists in this entry
                (unless (save-excursion
                          (re-search-forward "  CREATED: \\[" end t))
                  ;; Move to the line right after the heading
                  (forward-line 1)
                  (beginning-of-line)
                  ;; Look for a properties drawer in the entry body
                  (if (re-search-forward "^\\s-*:PROPERTIES:\\s-*$" end t)
                      ;; Drawer found – find its closing :END: line
                      (if (re-search-forward "^\\s-*:END:\\s-*$" end t)
                          (progn
                            ;; Go to the end of the :END: line, then to the next line
                            (goto-char (match-end 0))
                            (forward-line 1)
                            (beginning-of-line)
                            ;; Insert the timestamp (indented by 2 spaces)
                            (insert (concat "  CREATED: "
                                            (format-time-string "[%Y-%m-%d %a %H:%M]")
                                            "\n")))
                        ;; Malformed drawer (no :END:) – fall back to inserting after heading
                        (progn
                          (forward-line 1)   ; return to line after heading
                          (beginning-of-line)
                          (insert (concat "  CREATED: "
                                          (format-time-string "[%Y-%m-%d %a %H:%M]")
                                          "\n"))))
                    ;; No properties drawer – insert directly after the heading line
                    (insert (concat "  CREATED: "
                                    (format-time-string "[%Y-%m-%d %a %H:%M]")
                                    "\n"))))))))


(setq org-refile-targets
      '(("~/Documents/repos/encrypted/pj-gtd-org/next_actions.org" :maxlevel . 1)
        ("~/Documents/repos/encrypted/pj-gtd-org/projects.org" :level . 1)
        ("~/Documents/repos/encrypted/pj-gtd-org/waiting_for.org" :level . 1)
        ("~/Documents/repos/encrypted/pj-gtd-org/someday_maybe.org" :level . 1)
        ("~/Documents/repos/personal/org/capture.org" :level . 1)
        ("~/Documents/repos/encrypted/pj-gtd-org/closed_cards.org" :level . 1)))
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
