;; Mac OS needed workarounds
(if (eq system-type 'darwin)
    ;; fix bad ssl
    (use-package gnutls
      :ensure t
      :config
      (add-to-list 'gnutls-trustfiles
		   (expand-file-name
		    "~/etc/tls/certificates/comodo.rsa.ca.intermediate.crt")))
  )

(provide '05-ugly-workarounds)
