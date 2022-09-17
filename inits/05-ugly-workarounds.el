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
;; Windows needed workarounds
(if (eq system-type 'windows-nt)
    ;; Windows API is built on UTF-16
    (set-selection-coding-system 'utf-16-le)          ; correct
)

(provide '05-ugly-workarounds)
