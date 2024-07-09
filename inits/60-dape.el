(use-package dape
    :straight (dape :host github
                            :repo "svaante/dape"
                            :local-repo "svaante/dape"
                            :branch "master"
                            )
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
  ((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
   (after-init . dape-breakpoint-load))

  :init
  ;; To use window configuration like gud (gdb-mi)
  (setq dape-buffer-window-arrangement 'gud)

  :config
  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-start-hook 'dape-info)
  ;; (remove-hook 'dape-start-hook 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-stopped-hook 'dape-info)
  ;; (add-hook 'dape-stopped-hook 'dape-repl)

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root)
  )

(provide '60-dape)
