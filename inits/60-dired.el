(use-package dired
  :straight (:type built-in)
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

  ;; Case insentitive search by default
  (setq find-grep-options "-q -i")

  ;; Recursive, interractive find and replace with dired:
  ;;   M-x find-name-dired: you will be prompted for a root directory and a filename pattern.
  ;;   Press t to "toggle mark" for all files found.
  ;;   Press Q for "Query-Replace in Files...": you will be prompted for query/substitution regexps.
  ;;   Proceed as with query-replace-regexp: SPACE to replace and move to next match, n to skip a match, etc.
  ;;   Press C-x s to save buffers. (You can then press y for yes, n for no, or ! for yes for all)


  ;; use dired-x instead of dired
  ;; (add-hook 'dired-load-hook
  ;;              (function (lambda ()
  ;;                                      (load "dired-x")
  ;;                                      ;; set global variables here.
  ;;                                      ;; ex)
  ;;                                      ;; (setq dired-guess-shell-gnutar "gtar")
  ;;                                      )))
  ;; (add-hook 'dired-mode-hook
  ;;              (function (lambda ()
  ;;                                      ;; set buffer-local variables here.
  ;;                                      ;; ex)
  ;;                                      ;; (dired-omit-mode 1)
  ;;                                      )))

  (use-package nerd-icons-dired
    :straight t
    :if (display-graphic-p)
    :hook (dired-mode . nerd-icons-dired-mode))
  )
