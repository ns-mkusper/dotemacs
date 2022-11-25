(use-package powershell
  ;; https://learn.microsoft.com/en-us/archive/blogs/dotnetinterop/run-powershell-as-a-shell-within-emacs
  :if (eq system-type 'windows-nt)
  :ensure t
  :init
  (setq explicit-shell-file-name "powershell"
        explicit-powershell-args '("-NoLogo" "-NonInteractive"))
  :config
  ;; Change default compile command for powershell
  (add-hook 'powershell-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command)
                   (format "powershell.exe -NoLogo -NonInteractive -Command \"& '%s'\""             (buffer-file-name)))))
  )

(provide '60-powershell)
