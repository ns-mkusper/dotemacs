;;;; --- VTERM CONFIGURATION ---

(if (eq system-type 'windows-nt)
    ;; ---------------------------------------------------------
    ;; WINDOWS (MSYS2/MINGW) CONFIGURATION
    ;; ---------------------------------------------------------
    (progn
      ;; 1. ENVIRONMENT SETUP
      ;; Tell bash it is a login shell to initialize MSYS2 environment.
      (setq explicit-bash-args '("--login" "--noediting" "-i"))

      ;; Set MSYS2 environment (e.g., UCRT64).
      (setq process-environment (nconc (list "MSYSTEM=UCRT64") process-environment))

      ;; Ensure MSYS2 binaries are in PATH.
      (add-to-list 'exec-path "C:/msys64/usr/bin")
      (add-to-list 'exec-path "C:/msys64/usr/local/bin")
      (add-to-list 'exec-path "C:/msys64/mingw64/bin")
      (add-to-list 'exec-path "C:/msys64/ucrt64/bin")

      ;; 2. FAKECYGPTY SETUP
      (use-package fakecygpty
        :straight (fakecygpty :host github
                              :repo "vleonbonnet/fakecygpty"
                              :branch "master")
        :init
        (fakecygpty-activate))

      ;; 3. VTERM PACKAGE (WINDOWS FORK)
      (use-package vterm
        :straight (vterm :host github :repo "vleonbonnet/emacs-libvterm")
        :config
        ;; Path to pre-compiled DLL
        (setq vterm-module-path "C:/Users/Mark/AppData/Roaming/git/emacs-libvterm/vterm-module.dll")

        ;; Specific shell executable
        (setq vterm-shell "C:/msys64/usr/bin/f_zsh.exe")
        (setq vterm-max-scrollback 100000)

        :bind (:map vterm-mode-map
                    ("C-y" . #'vterm-yank))))

  ;; ---------------------------------------------------------
  ;; LINUX / MACOS CONFIGURATION
  ;; ---------------------------------------------------------
  (progn
    (use-package vterm
      ;; Standard upstream repo
      :straight t
      :config
      (setq vterm-max-scrollback 100000)
      :bind (:map vterm-mode-map
                  ("C-y" . #'vterm-yank)))))

(provide '60-vterm)
