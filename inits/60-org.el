;; ORG MODE SETUP
;; which files should org-agenda read?
;; TODO: should we use roam?
(setq org-agenda-files (list "~/drive/org/agenda"))

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
  ;; (centered-cursor-mode) ;; Enable centered cursor mode
  (smartparens-mode 0) ;; Disable smartparents
  (hl-prog-extra-mode)  ;; Highlighting with regexps
  (flyspell-mode)
  (org-superstar-mode) ;; Replace headline markers w/ bullets
  (toc-org-enable) ;; easily generate and keep ToC updated
  (org-appear-mode) ;; only show certain markers when needed
  (org-fancy-priorities-mode)
  )

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
  (setq org-ellipsis "  ⤵ ")
  (setq org-src-fontify-natively t) ;; Syntax highlighting in org src blocks
  (setq org-highlight-latex-and-related '(native)) ;; Highlight inline LaTeX
  (setq org-startup-folded 'show2levels) ;; Org files start up folded by default
  (setq org-image-actual-width nil)
  (setq org-startup-with-inline-images t)
  (setq org-fontify-whole-heading-line t)
  (setq org-cycle-separator-lines 1)
  (setq org-catch-invisible-edits 'show-and-error) ;; 'smart
  (setq org-src-tab-acts-natively t)

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
        org-habit-following-days 0
        org-habit-show-habits-only-for-today nil
        org-habit-today-glyph ?📅;;‖
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
                                   (search . " %i %-12:c"))))

(use-package toc-org
  :straight t
  :commands toc-org-enable
  )

(use-package org-super-agenda
  :straight t
  :after org
  :config
  (setq org-super-agenda-header-map nil) ;; takes over 'j'
  (setq org-super-agenda-header-prefix "◦ ") ;; There are some unicode "THIN SPACE"s after the ◦
  ;; Hide the thin width char glyph. This is dramatic but lets me not be annoyed
  (add-hook 'org-agenda-mode-hook
            #'(lambda () (setq-local nobreak-char-display nil)))
  (org-super-agenda-mode))

(use-package org-superstar
  :straight t
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
;;   :straight t
;;   :defer t
;;   :config
;;   (setq org-gcal-down-days '20					;; Only fetch events 20 days into the future
;;         org-gcal-up-days '10					;; Only fetch events 10 days into the past
;;         org-gcal-recurring-events-mode 'top-level
;;         org-gcal-remove-api-cancelled-events t) ;; No prompt when deleting removed events
;;   ;; NOTE - org-gcal ids and calendar configuation is set in 'private.el' for sake of security/privacy.
;;   )

(use-package org-appear
  :straight t
  :commands (org-appear-mode)
  :init
  (setq org-hide-emphasis-markers t		;; A default setting that needs to be t for org-appear
        org-appear-autoemphasis t		;; Enable org-appear on emphasis (bold, italics, etc)
        org-appear-autolinks nil		;; Don't enable on links
        org-appear-autosubmarkers t))	;; Enable on subscript and superscript

(use-package ox-reveal
  :straight t
  :defer 5)

(setq org-modules '(org-habit))

(eval-after-load 'org
  '(org-load-modules-maybe t))

(use-package org-fancy-priorities
  :after (org all-the-icons)
  :straight t
  :hook (org-mode        . org-fancy-priorities-mode)
  :hook (org-agenda-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list `(,(all-the-icons-faicon "flag"     :height 1.1 :v-adjust 0.0)
                                    ,(all-the-icons-faicon "arrow-up" :height 1.1 :v-adjust 0.0)
                                    ,(all-the-icons-faicon "square"   :height 1.1 :v-adjust 0.0))))

;; hugo org-mode integration for blogging
(use-package ox-hugo
  :straight t
  :after ox)

(use-package org-download
  :straight t)

(provide '60-org)
