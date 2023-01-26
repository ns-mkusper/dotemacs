;; visual indenting
(use-package fill-column-indicator
  :straight t
  :commands (fci-mode)
  :init
  (setq fci-rule-column 100)
  (setq fci-rule-use-dashes t)
  (setq fci-dash-pattern 0.5)
  (setq fci-rule-width 1)
  (setq fci-rule-color "grey40")
  )

(provide '20-fill-column-indicator)
