(use-package cfn-mode
  :config
  (add-to-list 'magic-mode-alist
    '("\\(.\\|\n\\)*Type: AWS::" . cfn-mode))
  ;; hack to stop this file from having cfn-mode enabled based on the
  ;; presence of the cfn-mode magic-mode-alist set up
  (add-to-list 'magic-mode-alist
    '("\\(.\\|\n\\)*;;; Commentary" . emacs-lisp-mode)))

(use-package flycheck-cfn
  :after (flycheck cfn-mode)
  :config
  (flycheck-cfn-setup))

(provide '60-aws)
