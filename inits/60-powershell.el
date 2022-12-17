(use-package powershell
  ;; https://learn.microsoft.com/en-us/archive/blogs/dotnetinterop/run-powershell-as-a-shell-within-emacs
  :if (eq system-type 'windows-nt)
  :ensure t
  :bind (("C-c RET" . open-powershell-with-project-buffer-name))
  :config
  (defun open-powershell-with-project-buffer-name ()
      (interactive)
    (setq shell-buffer-name (my-get-project-or-filename))
    (powershell shell-buffer-name nil))
  ;; Change default compile command for powershell
  ;; (add-hook 'powershell-mode-hook
  ;;           (lambda ()
  ;;             (set (make-local-variable 'compile-command)
  ;;                  (format "powershell.exe -NoLogo -NonInteractive -Command \"& '%s'\""             ((my-get-project-or-filename) )))))
  )

(provide '60-powershell)
