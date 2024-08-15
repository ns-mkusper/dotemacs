;; RUST
(defun my/rustic-before-save-fn ()
  "Format buffer and organize imports when saving anything using eglot."
(eglot-format-buffer))

(use-package rustic
  :straight t
  ;; :after (flycheck eglot)
  :mode
  ("\\.rs\\'" . rustic-mode)
  ;; :preface
  ;; (defun my/rustic-mode-hook-fn ()
  ;;   "Needed for lsp-format-buffer to indent with 4 spaces"
  ;;   (setq tab-width 4
  ;;      indent-tabs-mode nil))
  ;; (defun my/switch-to-cargo-window ()
  ;;   "Switch to the *cargo* window."
  ;;   (other-window 1))
  :init
  ;; to use rustic-mode even if rust-mode also installed
  (setq rust-mode-treesitter-derive t)
  (setq auto-mode-alist (delete '("\\.rs\\'" . rust-mode) auto-mode-alist)
        rustic-lsp-server 'rust-analyzer)
  :hook
  ;; (rustic-mode . hs-minor-mode) ;; fold mode
  (rustic-mode . eldoc-mode) ;; code tracing
  (rustic-mode . company-mode) ;; autocomplete
  ;; (rustic-mode . cargo-minor-mode)
  (rustic-mode . flycheck-mode)
  ;; (flycheck-mode . flycheck-rust-setup)
  (rustic-mode . (lambda () (setq indent-tabs-mode nil))) ;; set indent
  ;; (rustic-mode . my/rustic-mode-hook-fn)
  (rustic-before-save . my/before-save-fn) ;; use lsp format

  :custom
  (rustic-lsp-server 'rust-analyzer)
  (rustic-lsp-client 'eglot)
  (rustic-format-on-save nil)    ;; slow on windows
  (lsp-rust-rls-server-command 'rust-analyzer)
  :config
  (push 'rustic-clippy flycheck-checkers)
  ;; prevents lsp-mode prompt
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  ;; (advice-add 'rustic-cargo-check :after #'my/switch-to-cargo-window)
  ;; (advice-add 'rustic-cargo-run :after #'my/switch-to-cargo-window)
  ;; (advice-add 'rustic-cargo-build :after #'my/switch-to-cargo-window)
  (add-hook 'before-save-hook (lambda () (when (eq 'rustic-mode major-mode)
                                           (my/rustic-before-save-fn))))
  ;; (add-hook 'lsp-after-open-hook (lambda ()
  ;;                                  (when (lsp-find-workspace 'rust-analyzer nil)
  ;;                                    (lsp-rust-analyzer-inlay-hints-mode))))
  ;; :bind (:map rustic-mode-map
  ;;             ("C-c C-c s" . lsp-rust-analyzer-status))
  )


(provide '60-rust)
