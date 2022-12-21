;; (use-package elpy
;;   :straight t
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

;; TODO: Move to use-package block
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq-default python-indent-guess-indent-offset nil)
            (setq python-indent-offset 4)
            (setq highlight-indentation-offset 4)))

(use-package pyenv-mode
  :after python
  :hook
  ((python-mode . pyenv-mode)
   (projectile-switch-project . projectile-pyenv-mode-set))
  :straight t
  :init
  (add-to-list 'exec-path (expand-file-name "~/.pyenv/shims"))
  (add-to-list 'exec-path (expand-file-name "~/.pyenv/bin"))
  (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.pyenv/shims")))
  (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.pyenv/bin")))
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  :custom (pyenv-mode-set "3.10.0")
  :preface
  ;; use with projectile
  ;; https://github.com/pythonic-emacs/pyenv-mode#projectile-integration
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset)))))

(use-package pyvenv
  :after python
  :hook (python-mode . pyvenv-mode)
  :straight t
  :custom
  (pyvenv-default-virtual-env-name "env")
  (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:"
                                                         pyvenv-virtual-env-name "]"))))

;; Enable poetry
(use-package poetry
  :straight t
  :config
  (poetry-tracking-mode) ;; This auto load related venv when opening the file
  )

(use-package blacken
  :straight t
  :if (executable-find "black")
  :after python
  :commands (blacken-mode blacken-buffer)
  :diminish
  :init
  (setq blacken-line-length 100)
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

(use-package pyvenv
  :straight t)

(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(provide '60-python)
