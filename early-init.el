;; prevent package.el loading packages prior to their init-file loading
(setq package-enable-at-startup nil)
;; workaround for straight bug
;; see: https://jeffkreeftmeijer.com/emacs-native-comp-log/
(defvar native-comp-deferred-compilation-deny-list nil)
;; lsp-mode optimization
;; see: https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
;; (setenv "LSP_USE_PLISTS" "true")
