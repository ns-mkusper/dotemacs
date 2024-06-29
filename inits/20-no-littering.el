(use-package no-littering
  :straight t
  :init
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  :config
  (no-littering-theme-backups))

(provide '20-no-littering)
