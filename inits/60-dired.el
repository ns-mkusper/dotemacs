(use-package dired
  :init
  (load "dired-x")
  :config
  ;; Don't create new buffer with RET key
  ;; http://www.pshared.net/diary/20071207.html#p02
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map "a" 'dired-advertised-find-file)

  ;; macOS `ls' does not have `--dired' , so use coreutils `ls'
  (if (file-executable-p "/usr/local/bin/gls")
    (setq insert-directory-program "/usr/local/bin/gls"))

  ;; recursive copy and recursive delete
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)

  ;; use dired-x instead of dired
  ;; (add-hook 'dired-load-hook
  ;; 		  (function (lambda ()
  ;; 					  (load "dired-x")
  ;; 					  ;; set global variables here.
  ;; 					  ;; ex)
  ;; 					  ;; (setq dired-guess-shell-gnutar "gtar")
  ;; 					  )))
  ;; (add-hook 'dired-mode-hook
  ;; 		  (function (lambda ()
  ;; 					  ;; set buffer-local variables here.
  ;; 					  ;; ex)
  ;; 					  ;; (dired-omit-mode 1)
  ;; 					  )))
  )
