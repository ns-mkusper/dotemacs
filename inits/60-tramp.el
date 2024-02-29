(use-package tramp
  :straight nil
  :custom
  (tramp-default-method "ssh")
  ;; projectile causes slowness over remote connection
  ;; see: https://www.reddit.com/r/emacs/comments/xul3qm/comment/iqy0gct/?utm_source=reddit&utm_medium=web2x&context=3
  (doom-modeline-project-detection nil)
  (projectile-auto-update-cache nil)
  (projectile-dynamic-mode-line nil)
  :config
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq tramp-verbose 1))

(use-package tramp-sh
  :straight nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
