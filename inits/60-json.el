(use-package json-mode
  :ensure t
  :init
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

(provide '60-custom-keys)
