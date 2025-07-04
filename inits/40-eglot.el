;; LSP Integration
(use-package eglot
  :init
  (fset #'jsonrpc--log-event #'ignore) ;; performance boost
  ;; TODO: does this actually help?
  (setf (plist-get eglot-events-buffer-config :size) 0) ;; improved perf

  :straight t
  :hook (prog-mode . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c h" . eldoc-doc-buffer)
              ("C-c l t" . eglot-find-typeDefinition)
              ("C-c l i" . eglot-find-implementation)
              ("C-c =" . eglot-format-buffer)
              ("C-c c" . eglot-completion-at-point)
              ("C-c r" . eglot-rename)
              ("C-c l a" . eglot-code-actions))
  :custom-face (eglot-highlight-symbol-face ((t (:inherit 'highlight :background "DimGray"))))

  :custom


  :config
  (setq read-process-output-max (* 1024 1024))

  ;; (push :documentHighlightProvider eglot-ignored-server-capabilities)
  ;; Enable LSP support by default in programming buffers
  ;; (add-hook 'prog-mode-hook #'eglot-ensure)

  (setq eldoc-echo-area-use-multiline-p t)
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '(toml-mode . ("taplo" "lsp")))
  (add-to-list 'eglot-server-programs '(toml-ts-mode . ("taplo" "lsp")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio"))) ;; TODO: figure out how to setup tree-sitter-modes properly with eglot
  )

(use-package eldoc-box
  :straight (:build t)
  :hook (prog-mode . eldoc-box-hover-at-point-mode)
  :after eldoc)


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
