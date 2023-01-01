(use-package doom-modeline
  :straight t
  :after all-the-icons
  :hook ('after-init-hook)
  :custom
  (doom-modeline-buffer-encoding t)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-project-detection 'projectile)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-line-numbers-style 'relative)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-hud nil)
  (doom-modeline-height 10)
  :config
  (doom-modeline-mode))

(provide '60-doom-modeline)
