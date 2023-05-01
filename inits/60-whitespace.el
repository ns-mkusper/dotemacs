;; Visualize and transform whitespace.
(use-package whitespace
  :straight t
  :after (company)
  :functions (my/on-off-whitespace-before-company)
  ;; :diminish whitespace-mode
  :custom (whitespace-line-column nil)  ; Set to fill-column.
  :config (progn (delete 'newline-mark whitespace-style) ; Don't paint $ at eol.
                 (delete 'lines whitespace-style) ; Don't mark whole long lines.
                 (when (< emacs-major-version 27) ; Mark end of too long lines.
                   (add-to-list 'whitespace-style 'lines-tail))

                 ;; Workaround to not show dots in popup menus.
                 (defvar-local my/ws-enabled nil)
                 (defun my/whitespace-mode-off ()
                   (setq-local my/ws-enabled whitespace-mode)
                   (when my/ws-enabled
                     (whitespace-mode -1)))
                 (defun my/whitespace-mode-on ()
                   (when my/ws-enabled
                     (whitespace-mode t)))
                 ;; company:
                 (defun my/on-off-whitespace-before-company (command)
                   (when (string= "show" command)
                     (my/whitespace-mode-off))
                   (when (string= "hide" command)
                     (my/whitespace-mode-on)))
                 (advice-add 'company-call-frontends
                             :before #'my/on-off-whitespace-before-company)
                 ;; popup:
                 (defadvice popup-create (before my/popup-suspend-ws activate)
                   "Suspend whitespace-mode while popups are visible."
                   (my/whitespace-mode-off))
                 (defadvice popup-delete (after my/popup-restore-ws activate)
                   "Restore whitespace-mode when all popups have closed."
                   (my/whitespace-mode-on))

                 (if (display-graphic-p)
                     (custom-set-faces
                      '(whitespace-line
                        ((t (:inherit whitespace-line
                                      :weight normal
                                      :foreground nil
                                      :background nil
                                      :box
                                      (:line-width 1 :color "dark red"))))))
                   (custom-set-faces    ; else
                    '(whitespace-line ((t (:inherit whitespace-line
                                                    :background nil
                                                    :underline t))))))

                 ;; Workaround for
                 ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=36837>, fixed
                 ;; in 28.1.
                 (defun my/ws-load-local-vars-first ()
                   "Loads fill-column before enabling whitespace-mode."
                   ;; We don't use 'lines-tail in Emacs >= 27.
                   (when (< emacs-major-version 27)
                     (hack-local-variables))
                   (whitespace-mode))

                 (defun my/ws-maybe-cleanup ()
                   "Run `whitespace-cleanup' if `my/reformat-save' is t."
                   (when my/reformat-save
                     (whitespace-cleanup))))
  :bind ("C-c w" . whitespace-mode)
  ;; :hook ((prog-mode   . my/ws-load-local-vars-first)
  ;;        (conf-mode   . my/ws-load-local-vars-first)
  ;;        (text-mode   . my/ws-load-local-vars-first))
  )


(use-package whitespace-cleanup-mode
  :straight t
  ;; :after (whitespace)
  ;; :init
  ;; (global-whitespace-cleanup-mode)
  :custom
  (whitespace-cleanup-mode-only-if-initially-clean t)
  :hook
  ((prog-mode . whitespace-cleanup-mode))
  ((yaml-mode . whitespace-cleanup-mode)))

(provide '60-whitespace)
