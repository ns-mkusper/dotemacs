(use-package vdiff
  :straight t
  :config
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
    ;; automatically highlight word differences
  (setq vdiff-auto-refine t)
)

;; use vdiff on magit instead of ediff
;; (use-package vdiff-magit
;;   :straight t
;;   :bind (:map magit-mode-map
;;               ("e" . vdiff-magit-dwim)
;;               ("E" . vdiff-magit))
;;   :config
;;   (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
;;   (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
;;   (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
;;   (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))


(provide '60-vdiff)
