;; AUTO COMPLETE
;; TODO: evaluate vs company
;; (use-package corfu
;;   :straight t
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)       ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)        ;; Enable auto completion
;;   (corfu-separator ?\s) ;; Orderless field separator
;;   (corfu-quit-at-boundary nil) ;; Never quit at completion boundary
;;   (corfu-quit-no-match nil)   ;; Never quit, even if there is no match
;;   (corfu-preview-current nil) ;; Disable current candidate preview
;;   (corfu-preselect-first nil) ;; Disable candidate preselection
;;   (corfu-on-exact-match nil)  ;; Configure handling of exact matches
;;   (corfu-echo-documentation nil) ;; Disable documentation in the echo area
;;   (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; Enable Corfu only for certain modes.
;;   :hook (;; (prog-mode . corfu-mode)
;;          (shell-mode . corfu-mode)
;;          (eshell-mode . corfu-mode))

;;   ;; Recommended: Enable Corfu globally.
;;   ;; This is recommended since Dabbrev can be used globally (M-/).
;;   ;; See also `corfu-excluded-modes'.
;;   ;; :init
;;   ;; (global-corfu-mode)
;;   ;; Not enabling since lsp completion is preferred but this might work better in some languages
;;   )

;; ;; Use Dabbrev with Corfu!
;; (use-package dabbrev
;;   :straight t
;;   ;; Swap M-/ and C-M-/
;;   :bind (("M-/" . dabbrev-completion)
;;          ("C-M-/" . dabbrev-expand))
;;   ;; Other useful Dabbrev configurations.
;;   :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(provide '40-corfu)
