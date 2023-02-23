(use-package csv-mode
  :straight t
  ;; Always enter CSV mode in align mode; makes it easier to read.
  :hook (csv-mode . csv-align-mode)
  :init
  ;; show persistent header line
  (csv-header-line)
)

(provide '60-csv)
