;; prevent package.el loading packages prior to their init-file loading
(setq package-enable-at-startup nil)
;; workaround for straight bug
;; see: https://jeffkreeftmeijer.com/emacs-native-comp-log/
(defvar native-comp-deferred-compilation-deny-list nil)
;; No titlebar on emacs 29 and 30
;; see: https://emacs.stackexchange.com/questions/62258/undecorated-frame-with-header-line-as-titlebar
