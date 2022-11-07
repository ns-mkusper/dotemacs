(use-package whitespace-cleanup-mode
  :config (setq 'whitespace-cleanup-mode t)
  :hook
  (prog-mode . whitespace-cleanup))

(provide '60-whitespace-cleanup-mode)
