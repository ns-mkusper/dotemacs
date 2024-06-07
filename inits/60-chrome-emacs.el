(use-package atomic-chrome
  :demand t
  :straight (atomic-chrome
             :repo "KarimAziev/atomic-chrome"
             :type git
             :host github)
  :commands (atomic-chrome-start-server)
  :config
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-buffer-open-style 'full)
  (setq-default atomic-chrome-extension-type-list '(atomic-chrome))
  (setq-default atomic-chrome-url-major-mode-alist
                '(("github.com" . gfm-mode)
                  ("gitlab.com" . gfm-mode)
                  ("leetcode.com" . python-mode)))
  ;; (add-hook 'atomic-chrome-edit-mode-hook 'my/atomic-chrome-hooks)

  (general-unbind 'atomic-chrome-edit-mode-map
    :with 'atomic-chrome-close-current-buffer
    ;; [remap my/kill-this-buffer]
    )

  ;; (general-unbind 'atomic-chrome-edit-mode-map
  ;;   :with 'ignore
  ;;   [remap my/quiet-save-buffer])

  ;; (defun my/atomic-chrome-hooks ()
  ;;   (interactive)
  ;;   (focus-emacs)
  ;;   (olivetti-mode +1))

  (atomic-chrome-start-server))

(provide '60-chrome-emacs)
