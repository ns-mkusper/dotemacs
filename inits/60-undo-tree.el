(use-package undo-tree
  :straight t
  :init
  (global-undo-tree-mode)
  :custom
  ;; Strict memory limits to prevent serialization freezes.
  (setq undo-tree-limit (* 20 1024 1024))        ; 20MB limit (RAM)
  (setq undo-tree-strong-limit (* 30 1024 1024)) ; 30MB hard limit

  ;; Re-enable auto-save ONLY if limits are set and cache is clear.
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history")))

  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-enable-undo-in-region t))


(provide '60-undo-tree)
