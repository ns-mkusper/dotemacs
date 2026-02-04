;;; Header line context of symbol/heading (breadcrumb.el)
(use-package breadcrumb
  :straight t
  :hook (after-init . breadcrumb-mode)
  :config
  (setq breadcrumb-project-max-length 0.5)
  (setq breadcrumb-project-crumb-separator "/")
  (setq breadcrumb-imenu-max-length 1.0)
  (setq breadcrumb-imenu-crumb-separator " » ")

  ;; Only update breadcrumbs when idle for 0.5s, not instantly.
  ;; This prevents it from allocating strings during rapid undo/navigation.
  (setq breadcrumb-idle-delay 0.5)
  )


(provide '60-breadcrumb)
