;;; smoke-platform.el --- Cross-platform TRAMP smoke test  -*- lexical-binding: t; -*-

(require 'tramp)

(setq tramp-default-method "ssh")
(setq tramp-verbose 1)
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

(let ((vec (tramp-dissect-file-name "/ssh:example-host:/tmp/")))
  (unless (string= (tramp-file-name-method vec) "ssh")
    (error "Unexpected method parse: %S" (tramp-file-name-method vec)))
  (unless (string= (tramp-file-name-host vec) "example-host")
    (error "Unexpected host parse: %S" (tramp-file-name-host vec))))

(princ (format "OK: Emacs=%s System=%s TRAMP=%s\n"
               emacs-version
               system-type
               tramp-version))
