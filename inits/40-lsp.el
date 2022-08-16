;;; LSP
;; lsp-install-server all the below languages
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ; 1mb
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (rust-mode . lsp)
         (rustic-mode . lsp)
         (python-mode . lsp)
         (go-mode . lsp)
         (java-mode . lsp)
         (c++-mode . lsp)
         (bash-mode . lsp)
         (c-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optional integrations
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
                                        ;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

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
