(use-package tramp
  :straight (tramp :type git
                   :repo "https://git.savannah.gnu.org/git/tramp.git"
                   :host nil
                   :pre-build
                   (("autoconf")
                    ("./configure")
                    ("make")))
  :demand t
  :init
  (autoload #'tramp-register-crypt-file-name-handler "tramp-crypt")
  :config
  ;; (setq explicit-shell-file-name "/bin/zsh") TODO: How can we set this for just tramp connections?
  ;; Avoid “ControlPath too long” with Tramp on OSX
  (when (file-directory-p "/tmp/")
    (put 'temporary-file-directory 'standard-value (list "/tmp/")))


  (defun my-turn-off-project-detection ()
    ;; projectile causes slowness over remote connection
    ;; see: https://www.reddit.com/r/emacs/comments/xul3qm/comment/iqy0gct/?utm_source=reddit&utm_medium=web2x&context=3
    (setq-local projectile-auto-update-cache nil)
    (setq-local projectile-dynamic-mode-line nil)
    (setq-local doom-modeline-project-detection nil))
  (add-hook 'tramp-mode-hook #'my-turn-off-project-detection)

  ;; disable company in remote shell since it's slow
  ;; SEE: https://emacs.stackexchange.com/questions/55028/how-can-i-disable-company-mode-in-a-shell-when-it-is-remote
  (defun my-shell-mode-setup-function ()
    (when (and (fboundp 'company-mode)
               (file-remote-p default-directory))
      (company-mode -1)))

  (add-hook 'shell-mode-hook 'my-shell-mode-setup-function)


  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))
  (setq tramp-auto-save-directory temporary-file-directory)
  (setq remote-file-name-inhibit-locks t)

  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  ;; Only check for Git files remotely
  (setq vc-handled-backends '(Git))

  ;; Honor remote PATH.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; Allow ssh connections to persist.
  ;;
  ;; This seems to maybe cause tramp to hang a lot.
  (customize-set-variable 'tramp-use-ssh-controlmaster-options nil)
  ;; (customize-set-variable 'tramp-use-ssh-controlmaster-options t)

  (if (eq window-system 'w32)
                                        ;(setq tramp-default-method "ssh")
      (setq tramp-default-method "plink")
                                        ;(setq tramp-default-method "scpx")
    (setq tramp-default-method "ssh"))

  ;;(setq tramp-default-method "ssh")
  ;;(tramp-change-syntax 'simplified)
  ;; (setq tramp-verbose 6)

  ;; constant probing for VC files can slow tramp down. disabling is a significant speed-up
  (defun my-project-no-remote (orig dir)
    "Advice to avoid waking up tramp connections when probing for VC roots"
    (unless (file-remote-p dir)
      (funcall orig dir)))

  (advice-add 'project-try-vc :around #'my-project-no-remote)

  )

(use-package tramp-sh
  :straight nil
  :custom
  ;; Use out-of-band method for big files
  (tramp-copy-size-limit (* 0.5 1024 1024))
  :config
  ;; Use the PATH from the remote
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
