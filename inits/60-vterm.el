;;;; --- VTERM CONFIGURATION FOR WINDOWS (MSYS2/MINGW) ---

;; 1. WINDOWS/MSYS2 ENVIRONMENT SETUP
;; This block sets up the process environment needed for Emacs to correctly
;; spawn MSYS2/MinGW processes (recommended by the community discussion).
(if (eq system-type 'windows-nt)
    (progn
      ;; Tell bash it is a login shell to initialize MSYS2 environment.
      (setq explicit-bash-args '("--login" "--noediting" "-i"))

      ;; Set MSYS2 environment (e.g., UCRT64) for all processes spun out of Emacs.
      ;; Change UCRT64 if your MSYS2 environment is different (e.g., MINGW64).
      (setq process-environment (nconc (list "MSYSTEM=UCRT64") process-environment))

      ;; Ensure MSYS2 binaries are in PATH. Order is important.
      ;; Adjust "C:/msys64" to your actual MSYS2 installation path.
      (add-to-list 'exec-path "C:/msys64/usr/bin")
      (add-to-list 'exec-path "C:/msys64/usr/local/bin")
      (add-to-list 'exec-path "C:/msys64/mingw64/bin")
      (add-to-list 'exec-path "C:/msys64/ucrt64/bin")))


;; 2. FAKECYGPTY SETUP (CRITICAL for POSIX shells on Windows)
(use-package fakecygpty
  ;; Use the patched fork with MSYS2 support.
  :straight (fakecygpty :host github
                        :repo "vleonbonnet/fakecygpty"
                        :branch "master")
  :if (eq system-type 'windows-nt)
  :init
  ;; Activate the wrapper to handle POSIX shell spawning and signals.
  (fakecygpty-activate))

;; 3. VTERM PACKAGE SETUP
(use-package vterm
  ;; Use the patched fork for Windows compatibility.
  :straight (vterm :host github :repo "vleonbonnet/emacs-libvterm")
  :if (eq system-type 'windows-nt)
  :config
  ;; Tell vterm where to find the pre-compiled DLL.
  ;; REPLACE with your actual path.
  (setq vterm-module-path "C:/Users/Mark/AppData/Roaming/git/emacs-libvterm/vterm-module.dll")

  ;; Set the specific MSYS2 shell executable.
  ;; REPLACE with your actual bash.exe path (e.g., C:/msys64/mingw64/bin/bash.exe)
  (setq vterm-shell "C:/msys64/usr/bin/f_zsh.exe")

  ;; Optional settings
  (setq vterm-max-scrollback 100000)

  :bind (:map vterm-mode-map
              ("C-y" . #'vterm-yank)))

;; 4. FALLBACK/NON-WINDOWS CONFIG
;; This is the original config for Linux/macOS, included for completeness
(use-package vterm
  :straight t
  :if (or (string-equal system-type "gnu/linux") (string-equal system-type "darwin"))
  :config (setq vterm-max-scrollback 100000)
  :bind (:map vterm-mode-map
              ("C-y" . #'vterm-yank)))


(provide '60-vterm)
