;; RUST
(use-package rustic
  :ensure t
  :after (flycheck lsp-mode)
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
  ;; (flycheck-mode . flycheck-rust-setup)
  (rustic-mode . (lambda () (setq indent-tabs-mode nil))) ;; set indent
  :init
  (setq rustic-lsp-server 'rust-analyzer
        rustic-lsp-client 'lsp-mode
        rustic-format-on-save t)
  )

(use-package cargo
  :ensure t
  :commands (cargo-minor-mode)
  :if (executable-find "cargo")
  :after (rust-mode)
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  :config
  (setq cargo-process--command-bench "cargo bench")
  (setq cargo-process--command-build "cargo build")
  (setq cargo-process--command-clean "cargo clean")
  (setq cargo-process--command-doc "cargo doc")
  (setq cargo-process--command-doc-open "cargo doc --open")
  (setq cargo-process--command-new "cargo new")
  (setq cargo-process--command-init "cargo init")
  (setq cargo-process--command-run "cargo run")
  (setq cargo-process--command-run-bin "cargo run --bin")
  (setq cargo-process--command-run-example "cargo run --example")
  (setq cargo-process--command-search "cargo search")
  (setq cargo-process--command-test "cargo test")
  (setq cargo-process--command-current-test "cargo test")
  (setq cargo-process--command-current-file-tests "cargo test")
  (setq cargo-process--command-update "cargo update")
  (if (and (executable-find "cargo-fmt") (executable-find "rustfmt"))
      ;; cargo install rustfmt
      (setq cargo-process--command-fmt "cargo fmt"))
  (if (and (executable-find "cargo-check"))
      ;; cargo install cargo-check
      (setq cargo-process--command-check "cargo check"))
  (if (and (executable-find "cargo-clippy") (executable-find "clippy-driver"))
      ;; cargo install clippy
      (setq cargo-process--command-clippy "cargo clippy"))
  )


(provide '60-rust)
