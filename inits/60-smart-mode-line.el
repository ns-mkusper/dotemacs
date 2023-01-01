(use-package smart-mode-line
  :straight t
  :config
  (setq sml/no-confirm-load-theme t
        sml/theme 'respectful)
  (sml/setup))

(provide '60-smart-mode-line)
