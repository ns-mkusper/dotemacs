(use-package magit
  :straight t
  :bind
  (("C-x g" . magit-status))
  :config
  )
(use-package magit-find-file
  :straight t
  )

(use-package magit-todos
  :after magit
  :straight t
  :config (magit-todos-mode)
  ;; fix  rg scan limitation on windows
  ;; see: https://github.com/alphapapa/magit-todos/issues/156#issuecomment-1908717384
  (if (eq system-type 'windows-nt) (progn
                                     (setq magit-todos-scanners nil)
                                     (magit-todos-defscanner "rg"
                                       :test (executable-find "rg")
                                       :directory-form (f-relative directory default-directory) ;; revert
                                       :allow-exit-codes (0 1)
                                       :command (list "rg" "--no-heading" "--line-number"
                                                      (when depth
                                                        (list "--maxdepth" (1+ depth)))
                                                      (when magit-todos-ignore-case
                                                        "--ignore-case")
                                                      (when magit-todos-exclude-globs
                                                        (--map (list "--glob" (concat "!" it))
                                                               magit-todos-exclude-globs))
                                                      (unless magit-todos-submodule-list
                                                        (--map (list "--glob" (concat "!" it))
                                                               (magit-list-module-paths)))
                                                      extra-args search-regexp-pcre directory))))
  )


;; Migrated to diff-hl
;; see: https://github.com/dgutov/diff-hl

;; (use-package git-gutter
;;   :hook (prog-mode . git-gutter-mode)
;;   :config
;;   (setq git-gutter:update-interval 0.02))

;; (use-package git-gutter-fringe
;;   :config
;;   (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; (use-package git-gutter+
;;   :straight t
;;   :bind
;;   ((:map git-gutter+-mode-map
;;          ;; Jump between hunks
;;          ("C-x n" . git-gutter+-next-hunk)
;;          ("C-x p" . git-gutter+-previous-hunk)
;;          ;; Act on hunks
;;          ("C-x v =" . git-gutter+-show-hunk)
;;          ("C-x r" . git-gutter+-revert-hunks)
;;          ;; Stage hunk at point.
;;          ;; If region is active, stage all hunk lines within the region.
;;          ("C-x t" . git-gutter+-stage-hunks)
;;          ("C-x c" . git-gutter+-commit)
;;          ("C-x C" . git-gutter+-stage-and-commit)
;;          ("C-x C-y" . git-gutter+-stage-and-commit-whole-buffer)
;;          ("C-x U" . git-gutter+-unstage-whole-buffer)
;;          ))
;;   :init
;;   (add-hook 'prog-mode-hook 'git-gutter+-mode)
;;   (add-hook 'yaml-mode-hook 'git-gutter+-mode)
;;   (add-hook 'emacs-lisp-mode-hook 'git-gutter+-mode)
;;   (add-hook 'go-mode-hook 'git-gutter+-mode)
;;   (add-hook 'rust-mode-hook 'git-gutter+-mode)
;;   (add-hook 'python-mode-hook 'git-gutter+-mode)
;;   (add-hook 'c++-mode-hook 'git-gutter+-mode)
;;   (add-hook 'java-mode-hook 'git-gutter+-mode)
;;   :config
;;   )

;; (use-package git-link
;;   :straight t
;;   :bind
;;   ("C-c g l" . git-link)
;;   :init
;;   :config
;;   (if (file-exists-p "~/.emacs.d/secret/git-link-ghe.el")
;;       (load "~/.emacs.d/secret/git-link-ghe.el"))
;;   (setq git-link-use-commit t)
;;   )

(provide '60-git)
