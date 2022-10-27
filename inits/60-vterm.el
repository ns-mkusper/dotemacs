(use-package vterm
  ;; TODO: test on cygwin
  :if (or (string-equal system-type "gnu/linux") (string-equal system-type "darwin"))
  :ensure t
  :bind (:map vterm-mode-map
	      ("C-y" . #'vterm-yank)))

(provide '60-vterm)
