(use-package desktop
  :defer 2
  :config
  (setq desktop-restore-eager 5)
  (setq desktop-load-locked-desktop t)
  (desktop-save-mode +1))

(provide '50-desktop)
