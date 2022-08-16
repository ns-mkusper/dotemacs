;; Mac OS needed workarounds
(if (eq system-type 'darwin)
    ;; fix bad ssl
    (use-package gnutls
      :ensure t
      :config
      (add-to-list 'gnutls-trustfiles
		   (expand-file-name
		    "~/etc/tls/certificates/comodo.rsa.ca.intermediate.crt")))
;; get meta key working on Mac
(setq-default mac-option-modifier 'meta)
  )

(provide '05-ugly-workarounds)
