;; RUST
(defun my/rustic-before-save-fn ()
  "Format buffer and organize imports when saving anything using lsp-mode."
  (lsp-organize-imports)
    (lsp-format-buffer))

(use-package rustic
  :ensure t
  :after (flycheck lsp-mode)
  :mode
  ("\\.rs\\'" . rustic-mode)
  :preface
  ;; (defun my/rustic-mode-hook-fn ()
  ;;   "Needed for lsp-format-buffer to indent with 4 spaces"
  ;;   (setq tab-width 4
  ;; 	  indent-tabs-mode nil))
  (defun my/switch-to-cargo-window ()
    "Switch to the *cargo* window."
    (other-window 1))
  :init
  ;; to use rustic-mode even if rust-mode also installed
  (setq auto-mode-alist (delete '("\\.rs\\'" . rust-mode) auto-mode-alist)
        rustic-lsp-server 'rust-analyzer)
  :hook
  (rustic-mode . global-linum-mode) ;; line display
  (rustic-mode . hs-minor-mode) ;; fold mode
  (rustic-mode . eldoc-mode) ;; code tracing
  (rustic-mode . company-mode) ;; autocomplete
  ;; (rustic-mode . cargo-minor-mode)
  (rustic-mode . lsp-mode)
  (rustic-mode . flycheck-mode)
  ;; (flycheck-mode . flycheck-rust-setup)
  (rustic-mode . (lambda () (setq indent-tabs-mode nil))) ;; set indent
  ;; (rustic-mode . my/rustic-mode-hook-fn)
  (rustic-before-save . my/before-save-fn)

  :config
  (setq rustic-lsp-server 'rust-analyzer
        rustic-lsp-client 'lsp-mode
        rustic-format-on-save nil) ;; lsp-format-buffer is way less intrusive
  (advice-add 'rustic-cargo-check :after #'my/switch-to-cargo-window)
  (advice-add 'rustic-cargo-run :after #'my/switch-to-cargo-window)
  (advice-add 'rustic-cargo-build :after #'my/switch-to-cargo-window)
  (add-hook 'before-save-hook (lambda () (when (eq 'rustic-mode major-mode)
        				   (my/rustic-before-save-fn))))
  (add-hook 'lsp-after-open-hook (lambda ()
				   (when (lsp-find-workspace 'rust-analyzer nil)
				     (lsp-rust-analyzer-inlay-hints-mode))))
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)))

(provide '60-rust)
