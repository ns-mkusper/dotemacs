;; LSP Integration
(use-package eglot
  :straight t
  :config
  (setq read-process-output-max (* 1024 1024))
  ;; (push :documentHighlightProvider eglot-ignored-server-capabilities)
  ;; Enable LSP support by default in programming buffers
  (add-hook 'prog-mode-hook #'eglot-ensure)

  (setq eldoc-echo-area-use-multiline-p t)
  (setq eldoc-documentation-function #'eglot-eldoc-function)

  )

(use-package eglot-booster
  :straight (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(use-package flycheck-eglot
  :straight t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(provide '40-eglot)
