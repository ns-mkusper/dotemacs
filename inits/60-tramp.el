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

;; force psuedoterminal on windows as tramp workaround
;; see: https://emacs.stackexchange.com/questions/76120/how-can-i-use-tramps-ssh-on-windows-10-with-the-native-ssh-exe
(when (eq system-type 'windows-nt)
  (require 'cl-lib)
  (with-eval-after-load 'tramp
    (cl-pushnew '("-tt")
                (car (alist-get 'tramp-login-args
                                (cdr (assoc "ssh" tramp-methods))))
                :test #'equal)))
