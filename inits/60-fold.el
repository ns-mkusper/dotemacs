;; Code Folding
(use-package ts-fold-indicators
  :straight (ts-fold-indicators :type git :host github :repo "emacs-tree-sitter/ts-fold"))

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :config
  (add-hook 'tree-sitter-after-on-hook #'ts-fold-indicators-mode)
  (my-defkeymap
   "my-ts-fold" "C-c f"
   '("O" . ts-fold-open-all)
   '("o" . ts-fold-open-recursively)
   '("C" . ts-fold-close-all)
   '("c" . ts-fold-close)
   '("z" . ts-fold-toggle)))



(provide '60-fold)
