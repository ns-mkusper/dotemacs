;; R

(use-package ess
  :straight t
  :config
  ;; Optionally, ensure ess-view is available
  (use-package ess-view :straight t)

  ;; 1. Define the R Language Server command
  (let ((r-server-command
         (if (eq system-type 'windows-nt)
             ;; Use your specific R.exe path for Windows
             '("C:/Program Files/R/R-4.5.2/bin/x64/R.exe" "--slave" "-e" "languageserver::run()")
           ;; Use the standard executable name for Linux/macOS
           '("R" "--slave" "-e" "languageserver::run()"))))

    ;; 2. Add the configuration to Eglot's server list
    ;;    Eglot is smart enough to handle the 'R' executable if it's in your PATH
    ;;    on non-Windows systems, or the specific path on Windows.
    (add-to-list 'eglot-server-programs
                 `(ess-r-mode . ,r-server-command)))

  ;; 3. Hook Eglot to automatically start in R buffers
  (add-hook 'ess-r-mode-hook 'eglot-ensure))

(use-package ess-view
  :straight t
  :ensure t
  :after ess)

(use-package ess-view-data
  :after ess
  :ensure t
  :config
  (setq ess-view-data-read-string 'completing-read))

(use-package polymode
  :straight t)

(use-package poly-R
  :straight t
  :after polymode ; Must load after the core framework
  :config
  ;; Correctly map the .Rmd extension to the composite mode
  (add-to-list 'auto-mode-alist '("\\.[rR]md\\'" . poly-markdown+R-mode)))

;; (general-define-key
;;  :states '(normal visual insert emacs)
;;  :keymaps 'ess-mode-map
;;  :prefix "SPC"
;;  :non-normal-prefix "M-SPC"
;;  "l" '(:ignore t :which-key "layer")
;;  "ll" '(ess-eval-region-or-function-or-paragraph :which-key "eval region")
;;  "le" '(ess-eval-buffer :which-key "eval buffer")
;;  "lR" '(R :which-key "R"))




(provide '60-r)
