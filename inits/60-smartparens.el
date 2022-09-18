;; paredit like behavior in non-lispy languages
;; TODO: mayebe use instead of paredit in lispy langs?
(use-package smartparens
  :diminish smartparens-mode ;; Do not show in modeline
  :ensure t
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-strict-mode t)
  (show-smartparens-global-mode t)
  (setq sp-show-pair-from-inside t)
  :custom-face
  (sp-show-pair-match-face ((t (:foreground "White")))) ;; Could also have :background "Grey"
  )
