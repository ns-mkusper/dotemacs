(use-package neotree
  :ensure t
  :bind
  ("<f5>" . 'neotree-toggle)
  :init
  ;; slow rendering
  (setq inhibit-compacting-font-caches t)
  ;; set icons theme
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  ;; Every time when the neotree window is opened, let it find current file and jump to node
  (setq neo-smart-open t)
  ;; When running ‘projectile-switch-project’ (C-c p p), ‘neotree’ will change root automatically
  (setq projectile-switch-project-action 'neotree-projectile-action)
  ;; show hidden files
  (setq-default neo-show-hidden-files t))

(provide '60-neotree)
