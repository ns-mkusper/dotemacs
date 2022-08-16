;; visual indenting
(use-package fill-column-indicator
  :ensure t
  :init
  (setq fci-rule-column 80)
  (setq fci-rule-use-dashes t)
  (setq fci-dash-pattern 0.5)
  (setq fci-rule-width 1)
  (setq fci-rule-color "grey40")
  )

(provide '20-fill-column-indicator)
