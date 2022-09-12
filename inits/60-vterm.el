(use-package vterm
    :if (or (string-equal system-type "gnu/linux") (string-equal system-type "darwin"))
    :ensure t)

(provide '60-vterm)
