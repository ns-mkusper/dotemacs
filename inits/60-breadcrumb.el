;;; Header line context of symbol/heading (breadcrumb.el)
(use-package breadcrumb
  :straight t
  :hook (after-init . breadcrumb-mode)
  :config
  (setq breadcrumb-project-max-length 0.5)
  (setq breadcrumb-project-crumb-separator "/")
  (setq breadcrumb-imenu-max-length 1.0)
  (setq breadcrumb-imenu-crumb-separator " » "))


(provides '60-breadcrumb)
