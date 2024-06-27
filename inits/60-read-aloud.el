(use-package read-aloud
  :straight t
  :custom
  (read-aloud-engines '("flite" (cmd "flite" args ("--setf" "duration_stretch=.9"))))
  (read-aloud-engine "flite")
  :bind
  ("C-c C-v" . read-aloud-this)
  )

(provide '60-read-aloud)
