(use-package ripgrep
  :if (executable-find "rg")
  :bind (("C-M-g" . ripgrep-regexp))
  :straight t
  :defer t)

(provide '60-ripgrep)
