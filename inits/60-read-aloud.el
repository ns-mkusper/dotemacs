(use-package read-aloud
  :straight t
  :config
  (setq read-aloud-engine "flite")
  :bind
  ("C-c C-v" . read-aloud-this)
  )

(provide '60-read-aloud)
