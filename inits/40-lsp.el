;;; LSP
;; IMPORTANT: lsp-install-server all the below languages
(use-package lsp-mode
  :ensure t
  :custom
  ;; (create-lockfiles nil)
  (lsp-auto-guess-root t)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq read-process-output-max (* 1024 1024)) ; 1mb
  :hook
  ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
  (prog-major-mode . lsp-prog-major-mode-enable)
  (rust-mode . lsp)
  (rustic-mode . lsp)
  (python-mode . lsp)
  (go-mode . lsp)
  (java-mode . lsp)
  (c++-mode . lsp)
  (bash-mode . lsp)
  (c-mode . lsp)
  ;; if you want which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  :bind
  (:map lsp-mode-map
        ("C-c r" . lsp-rename))
  :commands lsp
  )

;; optional integrations
(use-package lsp-ui
  :ensure t
  :custom
  ;; (scroll-margin 0)
  ;; lsp-ui-doc
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'at-point) ;; top, bottom, or at-point
  (lsp-ui-doc-max-width 300)
  (lsp-ui-doc-max-height 50)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-doc-delay 1.4)
  ;; (lsp-ui-doc-alignment 'window) ;; window or frame
  (lsp-ui-doc-show-with-cursor t)
  ;; (lsp-ui-doc-show-with-mouse nil)
  ;; lsp-ui-flycheck
  (lsp-ui-flycheck-enable t)
  ;; lsp-ui-sideline
  ;; (lsp-ui-sideline-enable nil)
  ;; (lsp-ui-sideline-ignore-duplicate t)
  ;; (lsp-ui-sideline-show-symbol t)
  ;; (lsp-ui-sideline-show-hover t)
  ;; (lsp-ui-sideline-show-diagnostics nil)
  ;; (lsp-ui-sideline-show-code-actions nil)
  ;; lsp-ui-imenu
  ;; (lsp-ui-imenu-enable nil)
  ;; (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  ;; (lsp-ui-peek-enable t)
  ;; (lsp-ui-peek-peek-height 20)
  ;; (lsp-ui-peek-list-width 50)
  ;; (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
  ;; :hook
  ;; (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-mode-map
        ("C-c C-r" . lsp-ui-peek-find-references)
        ("C-c C-j" . lsp-ui-peek-find-definitions)
        ("C-c i" . lsp-ui-peek-find-implementation)
        ("C-c m" . lsp-ui-imenu)
        ("C-c s" . lsp-ui-sideline-mode)
        ("C-c d" . lsp-ui-doc-show)
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        )
  :commands lsp-ui-mode)
;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; for refactoring that requires insertions
(use-package yasnippet
  :ensure t
  :hook ((lsp-mode . yas-minor-mode)))

;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t
  :disabled
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  )
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
(use-package dap-go
  :enable t
  :disabled
  )
(use-package dap-python
  ;; pip install "ptvsd>=4.2"
  :enable t
  :disabled
  )

(provide '40-lsp)
