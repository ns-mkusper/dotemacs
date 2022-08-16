;;  Colors
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; fonts
;; windows font naming is dumb
(if (eq system-type 'windows-nt)
    (setq default-monospaced-emacs-font "Inconsolata NF")
  (setq default-monospaced-emacs-font "Inconsolata")
  )

(custom-set-faces
 '(default ((t (:family "Fira Code" :foundry "outline" :slant normal :weight normal :height 113 :width normal)))))

;; highlight current lines
(global-hl-line-mode 1)
(set-face-background hl-line-face "grey20")

;; powerline and doom require full icon pack
(use-package all-the-icons
  :ensure t)

(provide '20-theme)
