;; -----------------------------------------------------------------------------
;; 1. CACHING UTILITIES (From "Core Dumped" Blog)
;; -----------------------------------------------------------------------------
(defun memoize-remote (key cache orig-fn &rest args)
  "Memoize a value if the key is a remote path."
  (if (and key (file-remote-p key))
      (if-let ((current (assoc key (symbol-value cache))))
          (cdr current)
        (let ((current (apply orig-fn args)))
          (set cache (cons (cons key current) (symbol-value cache)))
          current))
    (apply orig-fn args)))

;; -----------------------------------------------------------------------------
;; 2. MAIN TRAMP CONFIG
;; -----------------------------------------------------------------------------
(use-package tramp
  :straight (tramp :type git
                   :repo "https://git.savannah.gnu.org/git/tramp.git"
                   :host nil
                   :pre-build (("autoconf") ("./configure") ("make")))
  :demand t
  :init
  (autoload #'tramp-register-crypt-file-name-handler "tramp-crypt")
  :config
  ;; --- SPEED SETTINGS (From Blog) ---
  (setq remote-file-name-inhibit-locks t)
  (setq tramp-use-scp-direct-remote-copying t)
  (setq remote-file-name-inhibit-auto-save-visited t)
  (setq tramp-verbose 1)

  ;; --- WINDOWS + DIRECT ASYNC PROFILE ---
  ;; This combines your Windows Shell fix AND the "Direct Async" speed boost
  (connection-local-set-profile-variables
   'my-tramp-profile
   '((tramp-direct-async-process . t)  ;; ENABLE DIRECT ASYNC (The "Go Brrr" setting)
     (explicit-shell-file-name . nil)  ;; FIX: Use remote shell, not Windows f_zsh.exe
     (explicit-bash.exe-args . nil)))  ;; FIX: Clear Windows args

  (connection-local-set-profiles
   '(:application tramp)
   'my-tramp-profile)

  ;; --- COMPILE FIX ---
  ;; Re-enable SSH ControlMaster for compilation (faster remote compiles)
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options))

  ;; --- DOOM MODELINE OPTIMIZATIONS ---
  ;; Prevent modeline from checking file stats constantly
  (with-eval-after-load 'doom-modeline
    (remove-hook 'evil-insert-state-exit-hook #'doom-modeline-update-buffer-file-name)
    (remove-hook 'find-file-hook #'doom-modeline-update-buffer-file-name))

  (remove-hook 'find-file-hook 'forge-bug-reference-setup)

  ;; --- PROJECTILE OPTIMIZATIONS ---
  (defun my-turn-off-project-detection ()
    (setq-local projectile-auto-update-cache nil)
    (setq-local projectile-dynamic-mode-line nil)
    (setq-local doom-modeline-project-detection nil))
  (add-hook 'tramp-mode-hook #'my-turn-off-project-detection)

  ;; --- STANDARD SETTINGS ---
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
  (setq tramp-auto-save-directory temporary-file-directory)

  ;; Use standard OpenSSH (ssh.exe) from MSYS2/Git.
  ;; Plink is SLOW because it lacks ControlMaster support.
  (setq tramp-default-method "ssh")

  ;; Honor remote PATH.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; -----------------------------------------------------------------------------
;; 3. TRAMP-SH TWEAKS (Chunk sizes)
;; -----------------------------------------------------------------------------
(use-package tramp-sh
  :straight nil
  :custom
  ;; "Core Dumped" recommends 1MB chunk size
  (tramp-copy-size-limit (* 1024 1024))
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; -----------------------------------------------------------------------------
;; 5. HEAVY MODE DISABLE (LSP/Flycheck)
;; -----------------------------------------------------------------------------
(defun my-disable-remote-heavy-stuff ()
  (when (file-remote-p default-directory)
    (when (fboundp 'flycheck-mode) (flycheck-mode -1))
    (when (fboundp 'lsp-mode) (lsp-mode -1))
    (when (fboundp 'git-gutter-mode) (git-gutter-mode -1))
    (when (fboundp 'company-mode) (company-mode -1))))

(add-hook 'find-file-hook #'my-disable-remote-heavy-stuff)
