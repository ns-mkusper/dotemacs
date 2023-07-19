;;; LSP
;; IMPORTANT: lsp-install-server all the below languages

(use-package lsp-mode
  :straight t
  :custom
  ;; (create-lockfiles nil)
  (lsp-auto-guess-root t)
  (lsp-clients-clangd-args '("-j=2"
                             "--background-index"
                             "--clang-tidy"
                             "--compile-commands-dir=build"
                             "--log=error"
                             "--pch-storage=memory"))
  (lsp-enable-xref t)
  ;; lens is a cool feature but laggy, best to enable on-demand
  (lsp-lens-auto-enable nil)
  (lsp-lens-enable nil)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq read-process-output-max (* 1024 1024)) ; 1mb
  ;; MANUALLY-INSTALLED ENGINE SETTING
  (setq lsp-terraform-server "terraform-ls")
  ;; (add-to-list 'lsp-language-id-configuration '(".*\\.pas$" . "pascal"))
  :config
  (put 'lsp-clients-clangd-args 'safe-local-variable #'listp)

  :hook
  (prog-major-mode . lsp-prog-major-mode-enable)
  ;; add new modes here to enable lsp integration
  ((zig-mode
    rust-mode
    rustic-mode
    python-mode
    go-mode
    java-mode
    c++-mode
    sh-mode
    c-mode
    terraform-mode
    shell-script-mode
    kotlin-mode
    yaml-mode
    lua-mode
    common-lisp-mode
    ;; pascal-mode
    ) . lsp-deferred)
  ;; if you want which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  :bind
  (:map lsp-mode-map
        ("C-c r" . lsp-rename)
        ("M-?" . lsp-find-references)
        ("C-c C-c a" . lsp-execute-code-action)
        ("C-c C-c r" . lsp-rename)
        ("C-c C-c q" . lsp-workspace-restart)
        ("C-c C-c Q" . lsp-workspace-shutdown))
  :commands lsp
  )

;; optional integrations
(use-package lsp-ui
  :straight t
  :custom
  ;; (scroll-margin 0)
  ;; lsp-ui-doc
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'at-point) ;; top, bottom, or at-point
  (lsp-ui-doc-max-width 300)
  (lsp-ui-doc-max-height 100)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-doc-delay 2)
  ;; (lsp-ui-doc-alignment 'window) ;; window or frame
  (lsp-ui-doc-show-with-cursor t)
  ;; (lsp-ui-doc-show-with-mouse nil)
  ;; lsp-ui-flycheck
  (lsp-ui-flycheck-enable t)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable nil)
  (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 30)
  (lsp-ui-peek-list-width 100)
  (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
  ;; :hook
  ;; (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-mode-map
        ("C-c C-r" . lsp-ui-peek-find-references)
        ("C-c C-j" . lsp-ui-peek-find-definitions)
        ("C-c i" . lsp-ui-peek-find-implementation)
        ("C-c m" . lsp-ui-imenu)
        ("C-c s" . lsp-ui-sideline-mode)
        ("C-c d" . lsp-describe-thing-at-point)

        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        )
  :commands lsp-ui-mode)

;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy
  :straight t
  :after lsp-mode
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :straight t
  :after lsp-mode
  :commands lsp-treemacs-errors-list)

(use-package lsp-pascal
  :straight t
  :after lsp-mode)

(provide '40-lsp)
