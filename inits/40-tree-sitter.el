(use-package tree-sitter
  :if (executable-find "tree-sitter")
  :straight t
  :config
  (global-tree-sitter-mode)
  :straight t)

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter
  :hook ((c-mode c++-mode java-mode rustic-mode python-mode json-mode yaml-mode go-mode terraform-mode) . tree-sitter-hl-mode))

(provide '60-tree-sitter)
