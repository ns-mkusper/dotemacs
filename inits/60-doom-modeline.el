(use-package doom-modeline
  :straight t
  :hook ('after-init-hook)
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-encoding t)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-project-detection 'projectile)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project) ; or truncate-with-project, truncate-except-project, relative-from-project relative-to-project
  (doom-line-numbers-style 'relative)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-hud t)
  (doom-modeline-height 10)
  (doom-modeline-lsp t)
  (doom-modeline-env-enable-python t)
  (doom-modeline-env-enable-ruby t)
  (doom-modeline-env-enable-perl t)
  (doom-modeline-env-enable-go t)
  (doom-modeline-env-enable-elixir t)
  (doom-modeline-env-enable-rust t)
  )

(provide '60-doom-modeline)
