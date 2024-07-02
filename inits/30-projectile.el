;; TODO: look into project, a built-in replacement for projectile
(use-package projectile
  :straight t
  ;; :diminish projectile-mode
  ;; :ensure-system-package (ripgrep)
  :bind-keymap
  ("C-C p" . projectile-command-map)
  :bind
  ;; (:map projectile-command-map
  ;;             ("s" . my-scratch-buffer))
  ("C-M-g" . 'projectile-ripgrep)

  :init
  (setq projectile-globally-ignored-file-suffixes '("pyc" "~" "#" "o" "obj" "map" "elf" "scl"))

  (setq projectile-globally-ignored-files
        '("TAGS" "GPATH" "GRTAGS" "GSYMS" "GTAGS"))

  ;; We'd like projects contained within other projects, e.g. packages pulled
  ;; into the .emacs.d/.straight dir via straight.el, to be recognized as
  ;; Projectile packages.
  (setq projectile-project-root-functions
        '(projectile-root-local
          projectile-root-bottom-up
          projectile-root-top-down
          projectile-root-top-down-recurring))

  (setq projectile-git-command "git ls-files -zc --exclude-standard")   ;; '**/*.py' remove -o

  :custom
  (projectile-enable-caching t)
  (projectile-sort-order 'recently-active)
  (projectile-buffers-filter-function 'projectile-buffers-with-file)
  (projectile-use-git-grep t)
  (projectile-completion-system 'ivy)
  (projectile-current-project-on-switch 'keep)

  :config
  (projectile-mode +1)
  (add-to-list 'projectile-globally-ignored-modes "dired-mode")
  )

;; TODO: figure out why this breaks ripgrep on windows
;; (use-package projectile-ripgrep
;;   :straight t)

(provide '30-projectile)
