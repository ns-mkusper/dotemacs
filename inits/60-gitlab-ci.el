(use-package gitlab-ci-mode
  :straight t
  :mode "\\.gitlab-ci.yml\\'")

(use-package gitlab-ci-mode-flycheck
  :straight t
  :after flycheck gitlab-ci-mode
  :init
  (gitlab-ci-mode-flycheck-enable))

(provide '60-gitlab-ci)
