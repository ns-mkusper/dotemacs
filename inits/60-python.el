(use-package python
  :straight t
  :init
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq-default python-indent-guess-indent-offset nil)
  (setq python-indent-offset 4)
  (setq highlight-indentation-offset 4)
  (setq fill-column 100)
  (fci-mode)
  :hook ((python-mode . eglot-ensure))
  :bind (:map python-mode-map
              ;; Force the execution of the main block
              ("C-c C-c" .  (lambda () (interactive) (python-shell-send-buffer t)))))

(use-package poetry
  :defer t
  :straight (:build t)
  :commands (poetry-venv-toggle
             poetry-tracking-mode)
  :config
  (setq poetry-tracking-strategy 'switch-buffer)
  (add-hook 'python-mode-hook #'poetry-tracking-mode))

(use-package with-venv
  :straight t)

;; (use-package py-isort
;;   :after python
;;   :straight t
;;   :hook ((python-mode . pyvenv-mode)
;;          (before-save . py-isort-before-save)))

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



(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-mode))))  ; or lsp-deferred

;; flymake-ruff integration
(use-package flymake-ruff
  :straight (flymake-ruff
             :type git
             :host github
             :repo "erickgnavar/flymake-ruff"))

(provide '60-python)
