(use-package ripgrep
  :if (executable-find "rg")
  :bind (("C-M-g" . ripgrep-regexp))
  :straight t
  :defer t
  :config
  (setq ripgrep-arguments '("--hidden" "--smart-case")))

(provide '60-ripgrep)
