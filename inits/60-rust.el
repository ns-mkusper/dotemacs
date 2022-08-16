;; RUST
(use-package rustic
  :ensure t
  :after (flycheck)
  :mode
  ("\\.rs\\'" . rustic-mode)
  :hook
  (rustic-mode . global-linum-mode) ;; line display
  (rustic-mode . hs-minor-mode) ;; fold mode
  (rustic-mode . eldoc-mode) ;; code tracing
  (rustic-mode . company-mode) ;; autocomplete
  (rustic-mode . cargo-minor-mode)
  (rustic-mode . lsp-mode)
  (rustic-mode . flycheck-mode)
;  (flycheck-mode . flycheck-rust-setup)
  (rustic-mode . (lambda () (setq indent-tabs-mode nil))) ;; set indent
  :config
  (setq rust-format-on-save t) ;; set format
  (setq rustic-lsp-client 'lsp
        rustic-format-on-save t)
  )

(provide '60-rust)
