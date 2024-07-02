;; LSP Integration
(use-package eglot
  :straight t
  :bind (:map eglot-mode-map
              ("C-c h" . eldoc-doc-buffer)
              ("C-c f r" . xref-find-references)
              ("C-c f d" . eglot-find-declaration ;; xref-find-definitions
               )
              ("C-c f D" . xref-find-definitions-other-window)
              ("C-c f t" . eglot-find-typeDefinition)
              ("C-c f i" . eglot-find-implementation)
              ("C-c =" . eglot-format-buffer)
              ("C-c c" . eglot-completion-at-point)
              ("C-c r" . eglot-rename)
              ("C-c a" . eglot-code-actions))
  :config
  (setq read-process-output-max (* 1024 1024))
  ;; (push :documentHighlightProvider eglot-ignored-server-capabilities)
  ;; Enable LSP support by default in programming buffers
  (add-hook 'prog-mode-hook #'eglot-ensure)

  (setq eldoc-echo-area-use-multiline-p t)
  (setq eldoc-documentation-function #'eldoc-documentation-compose)
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
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
c
