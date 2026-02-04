;; paredit like behavior in non-lispy languages
;; TODO: mayebe use instead of paredit in lispy langs?
(use-package smartparens
  :diminish smartparens-mode ;; Do not show in modeline
  :straight t
  :init
  (require 'smartparens-config)
  :config
  ;; Increase the delay so it doesn't run while spamming undo
  (setq sp-show-pair-delay 0.2)
  (smartparens-global-strict-mode t)
  (show-smartparens-global-mode t)
  (setq sp-show-pair-from-inside t)
  :hook
  ((prog-mode . smartparens-strict-mode)
   (markdown-mode . smartparens-strict-mode))

  :custom-face
  (sp-show-pair-match-face ((t (:foreground "White")))) ;; Could also have :background "Grey"
  )

(provide '60-smartparens)
