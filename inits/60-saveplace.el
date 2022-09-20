(use-package saveplace
  :ensure t
  :init (setq save-place-limit 100)
  :config (save-place-mode))

(provide '60-saveplace)
