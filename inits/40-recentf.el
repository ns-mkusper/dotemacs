(use-package recentf
  :config
  ;; This block only runs when recentf loads, so variables exist.
  (setq recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude 'file-remote-p)
  (recentf-mode 1))

(provide '40-recentf)
