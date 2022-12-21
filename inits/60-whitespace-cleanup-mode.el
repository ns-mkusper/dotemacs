(use-package whitespace-cleanup-mode
  :straight t
  :config
  (global-whitespace-cleanup-mode)
  (setq whitespace-cleanup-mode-only-if-initially-clean t)
  :hook
  (prog-mode . whitespace-cleanup))

(provide '60-whitespace-cleanup-mode)
