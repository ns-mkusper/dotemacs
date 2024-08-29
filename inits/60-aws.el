(use-package cfn-mode
  :straight (cfn-mode
             :repo "worr/cfn-mode"
             :type git
             :host gitlab)
  :config
  ;; (flycheck-cfn-setup)
  (add-to-list 'magic-mode-alist
    '("\\(.\\|\n\\)*Type: AWS::" . cfn-mode))
  ;; hack to stop this file from having cfn-mode enabled based on the
  ;; presence of the cfn-mode magic-mode-alist set up
  (add-to-list 'magic-mode-alist
    '("\\(.\\|\n\\)*;;; Commentary" . emacs-lisp-mode)))

(provide '60-aws)
