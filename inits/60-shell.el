;; bash
;; format bash
(use-package shfmt
  :straight (shfmt :host github
                   :repo "amake/shfmt.el"
                   :local-repo "amake/shfmt.el"
                   :branch "master"
                   )
  :hook (sh-mode . shfmt-on-save-mode)
  )
(use-package flycheck-shfmt
  :straight (flycheck-shfmt :host github
                            :repo "amake/shfmt.el"
                            :local-repo "amake/shfmt.el"
                            :files ("flycheck-shfmt.el")
                            :branch "master"
                            )
  :config
  (flycheck-shfmt-setup)
  )

;; probably better to use powershell on windows
(use-package powershell
  ;; https://learn.microsoft.com/en-us/archive/blogs/dotnetinterop/run-powershell-as-a-shell-within-emacs
  :if (eq system-type 'windows-nt)
  :straight t
  :bind (("C-c RET" . open-powershell-with-project-buffer-name))
  :config
  (defun open-powershell-with-project-buffer-name ()
    (interactive)
    (setq shell-buffer-name (my-get-shell-buffer-name))
    (powershell shell-buffer-name nil))
  ;; Change default compile command for powershell
  ;; (add-hook 'powershell-mode-hook
  ;;           (lambda ()
  ;;             (set (make-local-variable 'compile-command)
  ;;                  (format "powershell.exe -NoLogo -NonInteractive -Command \"& '%s'\""             ((my-get-project-or-filename) )))))
  )

(provide '60-shell)
