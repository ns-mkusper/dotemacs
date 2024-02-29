;;; Docker

(use-package dockerfile-mode
  :straight t
  :defer t
  :mode (("Dockerfile\\'" . dockerfile-mode)
         ("Dockerfile.dev'" . dockerfile-mode))
  )

(provide '60-docker)
