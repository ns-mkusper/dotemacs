(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :custom (typescript-indent-level 2))

(provide '60-typescript)
