(use-package undo-tree
  :straight t
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  ;; Limit in-memory undo size to ~20MB (default 80MB).
  ;; Prevents massive serialization overhead (375MB+ allocs) on save which triggers aggressive GC.
  (setq undo-tree-limit 20000000)
  (setq undo-tree-strong-limit 30000000)

  ;; Temporarily disable history saving to stop current IO/GC lag.
  ;; Re-enable after deleting ~/.emacs.d/undo-tree-history/
  ;; (setq undo-tree-auto-save-history nil)

  ;; To safely re-enable history saving later:
  ;; 1. Run: rm -rf ~/.emacs.d/undo-tree-history/
  ;; 2. Uncomment the limit below to prevent future bloat.
  ;; (setq undo-tree-history-size-limit (* 5 1024 1024)) ; 5MB limit
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-enable-undo-in-region t))


(provide '60-undo-tree)
