(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package company-restclient
  :defer t)

(use-package ob-restclient
  :defer t)


(provide '60-rest)
