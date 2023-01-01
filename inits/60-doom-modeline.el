(use-package doom-modeline
  :straight t
  :hook ('after-init-hook)
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-encoding t)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-project-detection 'projectile)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-line-numbers-style 'relative)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-hud t)
  (doom-modeline-height 10)
)

(provide '60-doom-modeline)
