(use-package undo-tree
  :straight t
  :init
  (global-undo-tree-mode)
  :custom
  ;; 1. Use .* to match all files.
  ;; 2. Ensure the path is absolute and expanded.
  (undo-tree-history-directory-alist `((".*" . ,(expand-file-name "undo-tree-history" user-emacs-directory))))

  (undo-tree-auto-save-history t)
  (undo-tree-limit (* 20 1024 1024))
  (undo-tree-strong-limit (* 30 1024 1024))
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-enable-undo-in-region t)

  :config
  ;; Create the directory if it doesn't exist
  (let ((undo-dir (expand-file-name "undo-tree-history" user-emacs-directory)))
    (unless (file-exists-p undo-dir)
      (make-directory undo-dir t))))


(provide '60-undo-tree)
