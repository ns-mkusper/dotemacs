;; R

(use-package ess
  :ensure t
  :init
  (require 'ess-site)
  ;; Only set R path on Windows
  (when (eq system-type 'windows-nt)
    (setq inferior-R-program-name "C:/Program Files/R/R-4.5.2/bin/x64/R.exe")))

(provide '60-r)
