;; (use-package elpy
;;   :ensure t
;;   :bind
;;   (:map elpy-mode-map
;;         ("M-." . elpy-goto-definition)
;;         )
;;   :init
;;   (elpy-enable)
;;   :config
;;   (setq elpy-rpc-python-command "python")
;;   (setq python-shell-interpreter "python")
;;   (setq elpy-rpc-timeout 120)
;;   )

(defun my/python-before-save-fn ()
  "Format buffer and organize imports when saving anything using lsp-mode."
  (lsp-organize-imports)
  (lsp-format-buffer))

(use-package pyenv-mode
  :ensure t
  :init
  (add-to-list 'exec-path (expand-file-name "~/.pyenv/shims"))
  (add-to-list 'exec-path (expand-file-name "~/.pyenv/bin"))
  (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.pyenv/shims")))
  (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.pyenv/bin")))
  :config
  (pyenv-mode
   pyvenv-tracking-mode)

  ;; use with projectile
  ;; https://github.com/pythonic-emacs/pyenv-mode#projectile-integration
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))

  (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set))

;; Enable poetry
(use-package poetry
  :ensure t
  :config
  (poetry-tracking-mode) ;; This auto load related venv when opening the file
  )

(use-package py-yapf
  :ensure t
  :hook (python-mode . py-yapf-enable-on-save)
  )

(use-package pyvenv
  :ensure t)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(provide '60-python)
