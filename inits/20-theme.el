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
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  :custom-face
  (org-ellipsis ((t (:height 0.8 :inherit 'shadow))))
  (org-scheduled-previously ((t (:background "red")))))

;; cursor color
(set-cursor-color 'IndianRed1)

;; highlight current lines
(global-hl-line-mode 1)
(set-face-background hl-line-face "grey10")

;; ayu mode doesn't colorize variable names
(custom-theme-set-faces 'user
                        `(font-lock-variable-name-face ((t (:foreground "#29AAA1"))))
                        )

;; set org level colors
(custom-theme-set-faces 'user
                        `(org-level-1 ((t (:foreground "#bfe438"))))
                        `(org-level-2 ((t (:foreground "#29AAA1"))))
                        `(org-level-3 ((t (:foreground "#eec900"))))
                        `(org-level-4 ((t (:foreground "#01fcff"))))
                        `(org-level-5 ((t (:foreground "#00bbea"))))
                        `(org-level-6 ((t (:foreground "#c66f19"))))
                        `(org-level-7 ((t (:foreground "#11bfaa"))))
                        `(org-level-8 ((t (:foreground "#735875"))))
                        `(org-level-9 ((t (:foreground "#9d3d4d"))))
                        `(org-level-10 ((t (:foreground "#ff6503"))))
                        `(org-scheduled-today ((t (:foreground "#c3c0bb"))))
                        )

;; powerline and doom require full icon pack
(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))


(provide '20-theme)
