;; ORG MODE SETUP
;; which files should org-agenda read?
(setq org-agenda-files (list
                        "~/drive/org/agenda"
                        "~/drive/org/work/agenda"))

(defun my-prettify-symbols-compose-predicate (start end _match)
  "Explicitly allow any occurrence of the non-breaking space to be composed."
  (let ((result (prettify-symbols-default-compose-p start end _match)))
    (or result (string-equal (buffer-substring start end) "\\nbsp{}"))))

(defun my-prettify-symbols-setup ()
  "Set up `prettify-symbols-mode' for `org-mode' buffers."
  (make-variable-buffer-local 'prettify-symbols-unprettify-at-point)
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (setq prettify-symbols-compose-predicate #'my-prettify-symbols-compose-predicate)
  (setq prettify-symbols-alist `(("\\nbsp{}" . ,(string-to-char "~"))))
  (prettify-symbols-mode 1))

(defun my-convert-org-to-docx-with-pandoc ()
  "Use Pandoc to convert .org to .docx.
Comments:
- The `-N' flag numbers the headers lines.
- Use the `--from org' flag to have this function work on files
  that are in Org syntax but do not have a .org extension"
  (interactive)
  (message "exporting .org to .docx")
  (shell-command
   (concat "pandoc -N --from org " (buffer-file-name)
           " -o "
           (file-name-sans-extension (buffer-file-name))
           (format-time-string "-%Y-%m-%d-%H%M%S") ".docx")))


(defun my-org-setup ()
  (org-indent-mode) ;; Keeps org items like text under headings, lists, nicely indented
  (visual-line-mode 1) ;; Nice line wrapping
  (org-superstar-mode) ;; Replace headline markers w/ bullets
  (toc-org-enable) ;; easily generate and keep ToC updated
  (org-appear-mode) ;; only show certain markers when needed
  (org-fancy-priorities-mode)
  ;; (centered-cursor-mode) ;; Enable centered cursor mode
  (smartparens-mode 0) ;; Disable smartparents
  (hl-prog-extra-mode)  ;; Highlighting with regexps
  (flyspell-mode)
  )

(use-package ox-slack)

(use-package org
  :pin gnu
  :hook
  (org-mode . my-org-setup)
  (org-mode . my-prettify-symbols-setup)
  :diminish org-indent-mode
  :diminish visual-line-mode
  :bind
  (:map org-mode-map
        ("C-c o l" . org-store-link)
        ("C-c a" . org-agenda))
  :config
  ;; Disable persistent cache.
  ;; High risk of cache corruption/desync leading to silent re-parsing loops
  ;; and massive CPU/GC usage during idle timers.
  (setq org-element-use-cache nil)
  (setq org-ellipsis "  ⤵ ")
  (setq org-src-fontify-natively t) ;; Syntax highlighting in org src blocks
  (setq org-highlight-latex-and-related '(native)) ;; Highlight inline LaTeX
  (setq org-startup-folded 'show2levels) ;; Org files start up folded by default
  (setq org-image-actual-width nil)
  (setq org-startup-with-inline-images t)
  (setq org-fontify-whole-heading-line t)
  (setq org-cycle-separator-lines 1)
  (setq org-fold-core-style 'text-properties)
  (setq org-catch-invisible-edits 'show-and-error) ;; 'smart
  (setq org-src-tab-acts-natively t)
  (setq org-clock-sound (expand-file-name "drive/ding.wav" (getenv "HOME"))) ;; TODO: automate this so that it always grabs the right sound even if drive isn't mounted
  ;; Save clock data and state changes and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks
  ;; with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (setq org-clock-out-when-done t)
  ;; Enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  ;; Include current clocking task in clock reports
  (setq org-clock-report-include-clocking-task t)
  ;; use pretty things for the clocktable
  (setq org-pretty-entities t)
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
  ;; Do not prompt to resume an active clock, just resume it
  (setq org-clock-persist-query-resume nil)
  
  )

;; M-Ret can split lines on items and tables but not headlines and not on anything else (unconfigured)
(setq org-M-RET-may-split-line '((headline) (item . t) (table . t) (default)))
(setq org-loop-over-headlines-in-active-region nil)

;; Opens links to other org file in same frame (rather than splitting)
(setq org-link-frame-setup '((file . find-file)))

(setq org-log-done t
      org-log-into-drawer t)

(setq org-tags-column 1)

(setq org-todo-keywords '((type
                           "TODO(t)" "WAITING(w)" "INPROG-TODO(i)" "HW(h)"
                           "STUDY(s)" "SOMEDAY" "READ(r)" "PROJ(p)" "CONTACT(c)"
                           "|" "DONE(d)" "CANCELLED(C)")))
(setq org-lowest-priority ?F)  ;; Gives us priorities A through F
(setq org-default-priority ?E) ;; If an item has no priority, it is considered [#E].

(setq org-priority-faces
      '((65 nil :inherit fixed-pitch :foreground "red2" :weight medium)
        (66 . "Gold1")
        (67 . "Goldenrod2")
        (68 . "PaleTurquoise3")
        (69 . "DarkSlateGray4")
        (70 . "PaleTurquoise4")))

(setq org-habit-preceding-days 66
      org-habit-following-days 1
      org-habit-show-habits-only-for-today nil
      org-habit-today-glyph ?⨯
      org-habit-completed-glyph ?✓
      org-habit-graph-column 40)

;; Uses custom time stamps
(setq org-time-stamp-custom-formats '("<%A, %B %d, %Y" . "<%m/%d/%y %a %I:%M %p>"))

;; Weekly view in agenda is pretty silly imo
(setq org-agenda-span 10
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-3d")


(setq org-agenda-restore-windows-after-quit t)

;; Only show upcoming deadlines for tomorrow or the day after tomorrow. By default it shows
;; 14 days into the future, which seems excessive.
(setq org-deadline-warning-days 7)
;; If something is done, don't show its deadline
(setq org-agenda-skip-deadline-if-done t)
;; If something is done, don't show when it's scheduled for
(setq org-agenda-skip-scheduled-if-done t)
;; If something is scheduled, don't tell me it is due soon
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

(setq org-agenda-timegrid-use-ampm 1
      org-agenda-time-grid nil)

(setq org-agenda-block-separator ?-)
(setq org-agenda-current-time-string "<----------------- Now")

(setq org-agenda-scheduled-leaders '("" "")
      org-agenda-deadline-leaders '("Due:" "Due in %1d day: " "Due %1d d. ago: "))

(setq org-agenda-prefix-format '((agenda . " %i %-1:i%?-2t% s")
                                 (todo . "  ")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))

;; CAPTURE TEMPLATES
;; (add-to-list 'org-capture-templates
;;              ;; TODO: fix to add habit capture template and others
;;              ;; see: https://www.reddit.com/r/emacs/comments/7zqc7b/comment/duwpow3/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
;;              '("h" "Habit" entry (file+headline "~/drive/org/agenda/habits.org" "Habits")
;;          "* TODO %^{Habit}
;;         SCHEDULED: %(format-time-string \"%Y-%m-%d\" (org-read-date nil t))
;;         :PROPERTIES:
;;         :STYLE: habit
;;         :END:")
;;              )
;; )

(use-package toc-org
  :commands toc-org-enable
  )

(use-package org-super-agenda
  :after org
  :config
  (setq org-super-agenda-header-map nil) ;; takes over 'j'
  (setq org-super-agenda-header-prefix "◦ ") ;; There are some unicode "THIN SPACE"s after the ◦
  ;; Hide the thin width char glyph. This is dramatic but lets me not be annoyed
  (add-hook 'org-agenda-mode-hook
            #'(lambda () (setq-local nobreak-char-display nil)))
  (org-super-agenda-mode))

(use-package org-superstar
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                          ("INPROG-TODO" . 9744)
                                          ("HW" . 9744)
                                          ("STUDY" . 9744)
                                          ("SOMEDAY" . 9744)
                                          ("READ" . 9744)
                                          ("PROJ" . 9744)
                                          ("CONTACT" . 9744)
                                          ("DONE" . 9745))))

;; Removes gap when you add a new heading
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

;; (use-package org-gcal
;;   :defer t
;;   :config
;;   (setq org-gcal-down-days '20					;; Only fetch events 20 days into the future
;;         org-gcal-up-days '10					;; Only fetch events 10 days into the past
;;         org-gcal-recurring-events-mode 'top-level
;;         org-gcal-remove-api-cancelled-events t) ;; No prompt when deleting removed events
;;   ;; NOTE - org-gcal ids and calendar configuation is set in 'private.el' for sake of security/privacy.
;;   )

(use-package org-appear
  :commands (org-appear-mode)
  :init
  (setq org-hide-emphasis-markers t		;; A default setting that needs to be t for org-appear
        org-appear-autoemphasis t		;; Enable org-appear on emphasis (bold, italics, etc)
        org-appear-autolinks nil		;; Don't enable on links
        org-appear-autosubmarkers t))	;; Enable on subscript and superscript

(use-package ox-reveal
  :defer 5)

(setq org-modules '(org-habit))

(eval-after-load 'org
  '(org-load-modules-maybe t))

(use-package org-fancy-priorities
  :after (org all-the-icons)
  :hook (org-mode        . org-fancy-priorities-mode)
  :hook (org-agenda-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list `(,(all-the-icons-faicon "flag"     :height 1.1 :v-adjust 0.0)
                                    ,(all-the-icons-faicon "arrow-up" :height 1.1 :v-adjust 0.0)
                                    ,(all-the-icons-faicon "square"   :height 1.1 :v-adjust 0.0))))

;; hugo org-mode integration for blogging
(use-package ox :straight nil)

(use-package ox-hugo
  :straight ( :host github
              :repo "kaushalmodi/ox-hugo"
              :branch "main")
  :after ox)



(use-package org-roam
  :straight (:build t)
  :defer t
  :custom
  (org-roam-directory (expand-file-name "drive/org/roam/" (getenv "HOME")))
  ;; intentionally not shared in remote (shared) drive
  ;; see: https://org-roam.discourse.group/t/org-roam-db-across-multiple-machines/332
  (org-roam-db-location (expand-file-name ".emacs.d/org-roam.db" (getenv "HOME"))) 
  (org-roam-completion-everywhere t)
  (org-roam-v2-ack t)
  (org-roam-db-autosync-mode 1)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)))

(use-package org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package org-pomodoro
  :commands (org-pomodoro)
  :config
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))
  :bind (("C-c C-u" . org-pomodoro))
  )

(use-package org-jira
  :straight (:host github :repo "ahungry/org-jira")
  :custom
  (org-jira-working-dir (my/directory-ensure org-directory "jira/")))

(provide '60-org)
