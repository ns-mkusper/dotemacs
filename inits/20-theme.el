;;  Colors
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-ayu-dark t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  :custom-face
  (org-ellipsis ((t (:height 0.8 :inherit 'shadow))))
  (org-scheduled-previously ((t (:background "red")))))

;; highlight current lines
(global-hl-line-mode 1)
(set-face-background hl-line-face "grey10")

;; powerline and doom require full icon pack
;; run M-x all-the-icons-install-fonts to enable (and then install the downloaded font files if on windows)
;; TODO: automate ^
(use-package all-the-icons
  :ensure t)

(provide '20-theme)
