(use-package devdocs
  :straight t
  :config
  (global-set-key (kbd "C-h D") 'devdocs-lookup))

(use-package devdocs-browser
  :straight t
  :after devdocs
  :defer t)


(provide '60-devdocs)
