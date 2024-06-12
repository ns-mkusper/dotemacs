;; TODO: Cannot open org-mode files in remote drives with EAF enabled. Test before re-enabling.
;;
;; (straight-use-package-lazy
;;  `(eaf :type git
;;        :host github
;;        :repo "emacs-eaf/emacs-application-framework"
;;        :files ("*.el" "*.py" "*.js" "*.html" "*.json" "core" "app" "node_modules")
;;        :build t
;;        :pre-build ,(pcase system-type
;;                        ('gnu/linux '(("./install-eaf.py" "--ignore-core-deps" "--ignore-sys-deps" "--ignore-py-deps" "--install-all-apps")))
;;                        (_          '(;; ("python" "./install-eaf.py" "--ignore-core-deps" "--ignore-sys-deps" "--ignore-py-deps" "--install-all-apps")
;;                                      )))))


;; (use-package eaf
;;     :straight t
;;     :after org
;;     :defines (eaf-browser-enable-adblocker
;;               eaf-open-browser)
;;     :functions (eaf-setq
;;                 eaf-bind-key)
;;     :init
;;     (use-package epc
;;         :straight t
;;         :defer t)
;;     (use-package ctable
;;         :straight t
;;         :defer t )
;;     (use-package deferred
;;         :straight t
;;         :defer t)
;;     (use-package s
;;         :straight t
;;         :defer t)

;;     :custom
;;     (eaf-browser-continue-where-left-off t)

;;     :init

;;     :config
;;     (when (eq system-type 'gnu/linux)
;;         (require 'eaf-browser)
;;         (require 'eaf-org-previewer)
;;         (require 'eaf-markdown-previewer)
;;         (eaf-setq eaf-browser-enable-adblocker "true")))

(provide '40-eaf)
