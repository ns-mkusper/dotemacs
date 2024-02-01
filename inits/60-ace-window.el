(use-package ace-window
  :commands ace-window
  :straight t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window))

  ;; :config
  ;; (add-to-list 'aw-ignored-buffers " *example*")
  )

(provide '60-ace-window)
