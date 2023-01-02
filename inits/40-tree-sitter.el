(use-package tree-sitter
  :if (executable-find "tree-sitter")
  :straight t
  :config
  (global-tree-sitter-mode)
  :straight t)

(use-package tree-sitter-langs
  :if (executable-find "tree-sitter")
  :straight t
  :after tree-sitter)

(provide '60-tree-sitter)
