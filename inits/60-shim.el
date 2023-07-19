;; shim applies pyenv, jenv, goenv, nodenv, etc
(use-package shim
  :straight (:host github :repo "twlz0ne/shim.el")
  :demand t
  :hook
  ((java-mode
    kotlin-mode
    python-mode
    go-mode
    typescript-mode) . shim-mode)
  :config
  (shim-init-ruby)
  (shim-init-python)
  (shim-init-node)
  (shim-init-java)
  (shim-init-go)
  (shim-register-mode 'java 'kotlin-mode)
  (shim-register-mode 'node 'js2-mode))


(provide '60-shim)
