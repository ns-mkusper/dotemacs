(use-package tramp
  :straight nil
  :demand t
  :init
  (autoload #'tramp-register-crypt-file-name-handler "tramp-crypt")
  :config
  ;; Keep TRAMP on the built-in version bundled with Emacs for best compatibility.
  ;; Mixing a git snapshot with the rest of the editor can cause subtle hangs.
  (setq tramp-default-method "ssh")
  (setq tramp-verbose 1)

  (defvar my/tramp-default-endpoints nil
    "Optional endpoint defaults for TRAMP.
Each entry is (HOST-REGEXP USER PORT). HOST-REGEXP is required.
Set this from private machine-local config to avoid committing LAN details.")

  (defun my/tramp--env-nonempty (name)
    "Return non-empty environment variable NAME, else nil."
    (let ((value (getenv name)))
      (unless (or (null value) (= (length value) 0))
        value)))

  (defun my/tramp--apply-default-endpoint (host user port)
    "Add TRAMP defaults for HOST regexp with optional USER and PORT."
    (when (and host (> (length host) 0))
      (when (and user (> (length user) 0))
        (add-to-list 'tramp-default-user-alist (list "ssh" host user)))
      (when (and port (> (length port) 0))
        (add-to-list 'tramp-default-port-alist (list "ssh" host port)))))

  (defun my/tramp-configure-default-endpoints ()
    "Apply endpoint defaults from env vars and `my/tramp-default-endpoints'."
    (my/tramp--apply-default-endpoint
     (my/tramp--env-nonempty "TRAMP_DEFAULT_HOST_REGEXP")
     (my/tramp--env-nonempty "TRAMP_DEFAULT_USER")
     (my/tramp--env-nonempty "TRAMP_DEFAULT_PORT"))
    (dolist (endpoint my/tramp-default-endpoints)
      (pcase-let ((`(,host ,user ,port) endpoint))
        (my/tramp--apply-default-endpoint host user port))))

  ;; Performance defaults that are stable across Linux/macOS/Windows.
  (setq remote-file-name-inhibit-locks t)
  (setq remote-file-name-inhibit-auto-save-visited t)
  (setq tramp-use-scp-direct-remote-copying t)
  (when (boundp 'tramp-use-connection-share)
    (setq tramp-use-connection-share t))
  (when (boundp 'tramp-direct-async-process)
    (setq tramp-direct-async-process nil))
  (when (boundp 'tramp-completion-reread-directory-timeout)
    (setq tramp-completion-reread-directory-timeout nil))
  (when (boundp 'tramp-ssh-controlmaster-options)
    (setq tramp-ssh-controlmaster-options
          "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=600"))
  (my/tramp-configure-default-endpoints)

  ;; Keep backups/autosaves local and avoid extra remote churn.
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
  (setq tramp-auto-save-directory temporary-file-directory)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))

  ;; Re-enable SSH multiplexing for compile buffers.
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook
                 #'tramp-compile-disable-ssh-controlmaster-options))

  ;; Remote buffers should avoid heavyweight features.
  (defun my/tramp-buffer-optimizations ()
    (when (file-remote-p default-directory)
      (when (boundp 'projectile-auto-update-cache)
        (setq-local projectile-auto-update-cache nil))
      (when (boundp 'projectile-dynamic-mode-line)
        (setq-local projectile-dynamic-mode-line nil))
      (when (boundp 'doom-modeline-project-detection)
        (setq-local doom-modeline-project-detection nil))
      (when (fboundp 'flycheck-mode) (flycheck-mode -1))
      (when (fboundp 'lsp-mode) (lsp-mode -1))
      (when (fboundp 'eglot-managed-mode) (eglot-managed-mode -1))
      (when (fboundp 'git-gutter-mode) (git-gutter-mode -1))
      (when (fboundp 'company-mode) (company-mode -1))))
  (add-hook 'find-file-hook #'my/tramp-buffer-optimizations)

  ;; Windows-specific safety: do not leak local shell overrides into TRAMP sessions.
  (when (eq system-type 'windows-nt)
    (connection-local-set-profile-variables
     'my/tramp-windows-shell-profile
     '((explicit-shell-file-name . nil)
       (shell-file-name . nil)
       (explicit-bash.exe-args . nil)))
    (connection-local-set-profiles
     '(:application tramp :protocol "ssh")
     'my/tramp-windows-shell-profile)))

(use-package tramp-sh
  :straight nil
  :custom
  (tramp-copy-size-limit (* 1024 1024)))
