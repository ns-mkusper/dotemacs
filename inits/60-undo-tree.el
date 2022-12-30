(use-package undo-tree
  :straight t
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (setq undo-tree-auto-save-history t)
  :init
  (global-undo-tree-mode))

(provide '60-undo-tree)
