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
(use-package fakecygpty
  ;; when using POSIX shells  on NT emacs we need to spawn these processes with fakecygpty.exe to ensure proper signal handling
  :straight (fakecygpty :host github
                        :repo "vleonbonnet/fakecygpty"
                        :branch "master")
  :if (eq system-type 'windows-nt)
  :init
  (fakecygpty-activate))

(when-on-windows
 (let* ((combine-path (lambda (dir dir-or-file)
                        (concat (file-name-as-directory dir) dir-or-file)))
        (base-dir "C:/msys64")
        (mingw64-bin-dir (funcall combine-path base-dir "ucrt64/bin"))
        (msys2-bin-dir (funcall combine-path base-dir "usr/bin"))
        (bash-path (funcall combine-path msys2-bin-dir "f_zsh.exe")))

   ;; Prepend the directories to exec-path
   (setq exec-path (append (list mingw64-bin-dir msys2-bin-dir) exec-path))

   (setq explicit-shell-file-name bash-path)
   (setenv "SHELL" bash-path)
   ;; Hint for MSYS2 bash to stay in the invoking directory.
   (setenv "CHERE_INVOKING" "1")
   ;; Avoid overriding the initial working directory by removing STARTDIR.
   (setenv "STARTDIR" nil)
   (setq explicit-bash.exe-args (list "--noediting" "--login" "-i"))
   (setenv "PATH" (concat mingw64-bin-dir path-separator
                          (concat msys2-bin-dir path-separator
                                  (getenv "PATH"))))))



;; probably better to use powershell on windows
(use-package powershell
  ;; https://learn.microsoft.com/en-us/archive/blogs/dotnetinterop/run-powershell-as-a-shell-within-emacs
  :if (eq system-type 'windows-nt)
  ;; :straight t
  ;; :bind (("C-c RET" . open-powershell-with-project-buffer-name))
  ;; :config
  ;; (defun open-powershell-with-project-buffer-name ()
  ;;   (interactive)
  ;;   (setq shell-buffer-name (my-get-shell-buffer-name))
  ;;   (powershell shell-buffer-name nil))
  ;; Change default compile command for powershell
  ;; (add-hook 'powershell-mode-hook
  ;;           (lambda ()
  ;;             (set (make-local-variable 'compile-command)
  ;;                  (format "powershell.exe -NoLogo -NonInteractive -Command \"& '%s'\""             ((my-get-project-or-filename) )))))
  )

(provide '60-shell)
