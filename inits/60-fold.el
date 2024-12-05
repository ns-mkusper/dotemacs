;; Code Folding
(use-package treesit-fold-indicators
  :straight (treesit-fold-indicators :type git :host github :repo "emacs-tree-sitter/treesit-fold"))

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :config
  ;; TODO: Figure out why this causes company to be insanely slow
  ;; (add-hook 'tree-sitter-after-on-hook #'treesit-fold-indicators-mode)
  (my/defkeymap
   "my-treesit-fold" "C-c f"
   '("O" . treesit-fold-open-all)
   '("o" . treesit-fold-open-recursively)
   '("C" . treesit-fold-close-all)
   '("c" . treesit-fold-close)
   '("z" . treesit-fold-toggle)
   '("i" . treesit-fold-indicators-mode)))



(provide '60-fold)
