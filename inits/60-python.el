(use-package elpy
  :ensure t
  :bind ("M-." . elpy-goto-definition)
  :init
  (elpy-enable)

  (setq elpy-rpc-python-command "python")
  (setq python-shell-interpreter "python")
  (setq elpy-rpc-timeout 120)
  )

(use-package pyenv-mode
  :ensure t
  :config
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))

  (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set))

(provide '60-python)
