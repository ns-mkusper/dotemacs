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



;;; WINDOWS
;; (when-on-windows
;;   (let* ((combine-path (lambda (dir dir-or-file)
;;                          (concat (file-name-as-directory dir) dir-or-file)))
;;          (base-dir "C:/tools/msys64")
;;          (mingw64-bin-dir (funcall combine-path base-dir "mingw64/bin"))
;;          (msys2-bin-dir (funcall combine-path base-dir "usr/bin"))
;;          (bash-path (funcall combine-path msys2-bin-dir "bash.exe")))
;;     (add-to-list 'exec-path msys2-bin-dir)
;;     (add-to-list 'exec-path mingw64-bin-dir)
;;     (setq explicit-shell-file-name bash-path)
;;     (setq shell-file-name bash-path)
;;     (setenv "SHELL" bash-path)
;;     (setq explicit-bash.exe-args (list "--noediting" "--login" "-i"))
;;     (setenv "PATH" (concat mingw64-bin-dir path-separator
;;                            (concat msys2-bin-dir path-separator
;;                                    (getenv "PATH"))))))


;; probably better to use powershell on windows
;; (use-package powershell
;;   ;; https://learn.microsoft.com/en-us/archive/blogs/dotnetinterop/run-powershell-as-a-shell-within-emacs
;;   :if (eq system-type 'windows-nt)
;;   :straight t
;;   :bind (("C-c RET" . open-powershell-with-project-buffer-name))
;;   :config
;;   (defun open-powershell-with-project-buffer-name ()
;;     (interactive)
;;     (setq shell-buffer-name (my-get-shell-buffer-name))
;;     (powershell shell-buffer-name nil))
;;   ;; Change default compile command for powershell
;;   ;; (add-hook 'powershell-mode-hook
;;   ;;           (lambda ()
;;   ;;             (set (make-local-variable 'compile-command)
;;   ;;                  (format "powershell.exe -NoLogo -NonInteractive -Command \"& '%s'\""             ((my-get-project-or-filename) )))))
;;   )

(provide '60-shell)
