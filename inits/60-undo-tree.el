(use-package undo-tree
  :straight t
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

  :init
  (global-undo-tree-mode))

(provide '60-undo-tree)
