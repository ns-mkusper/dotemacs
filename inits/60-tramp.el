(use-package tramp
  :straight nil
  :custom
  (tramp-default-method "ssh")
  ;; cache file names for 10 seconds
  (remote-file-name-inhibit-cache 10)

  ;; do not store remote command history over tramp
  (tramp-histfile-override t)

  :config
  (defun my-turn-off-project-detection ()
    ;; projectile causes slowness over remote connection
    ;; see: https://www.reddit.com/r/emacs/comments/xul3qm/comment/iqy0gct/?utm_source=reddit&utm_medium=web2x&context=3
    (setq-local projectile-auto-update-cache nil)
    (setq-local projectile-dynamic-mode-line nil)
    (setq-local doom-modeline-project-detection nil))
  (add-hook 'tramp-mode-hook #'my-turn-off-project-detection))

(use-package tramp-sh
  :straight nil
  :custom
  ;; Use out-of-band method for big files
  (tramp-copy-size-limit (* 0.5 1024 1024))
  :config
  ;; Use the PATH from the remote
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
