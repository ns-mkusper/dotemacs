;;  Colors
;; (use-package doom-themes
;;   :straight t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-peacock t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config)
;;   :custom-face
;;   (org-ellipsis ((t (:height 0.8 :inherit 'shadow))))
;;   (org-scheduled-previously ((t (:background "red")))))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

;; https://gitlab.com/protesilaos/modus-themes
(use-package modus-themes
  :straight (modus-themes :type git
                          :host gitlab
                          :repo "protesilaos/modus-themes")
    :custom
    ;; Add all your customizations prior to loading the themes
    (modus-themes-italic-constructs t)
    (modus-themes-bold-constructs t)
    (modus-themes-region '(bg-only no-extend))
    (modus-themes-lang-checkers '(text-also background straight-underline))
    (modus-themes-paren-match '(bold))
    (modus-themes-mode-line '(moody accented))

    :init
    (modus-themes-load-themes)
    :config
    (modus-themes-load-vivendi)
  )

;; cursor color
;; (set-cursor-color "IndianRed1")

;; highlight current lines
(global-hl-line-mode 1)
;; (set-face-background hl-line-face "grey9")

;; ayu mode doesn't colorize variable names
;; (custom-theme-set-faces 'user
;;                         `(font-lock-variable-name-face ((t (:foreground "#29AAA1"))))
;;                         )

;; set org level colors
;; (custom-theme-set-faces 'user
;;                         `(org-level-1 ((t (:foreground "#bfe438"))))
;;                         `(org-level-2 ((t (:foreground "#29AAA1"))))
;;                         `(org-level-3 ((t (:foreground "#eec900"))))
;;                         `(org-level-4 ((t (:foreground "#01fcff"))))
;;                         `(org-level-5 ((t (:foreground "#00bbea"))))
;;                         `(org-level-6 ((t (:foreground "#c66f19"))))
;;                         `(org-level-7 ((t (:foreground "#11bfaa"))))
;;                         `(org-level-8 ((t (:foreground "#735875"))))
;;                         `(org-level-9 ((t (:foreground "#9d3d4d"))))
;;                         `(org-level-10 ((t (:foreground "#ff6503"))))
;;                         `(org-scheduled-today ((t (:foreground "#c3c0bb"))))
;;                         )

;; customize company (auto-complete) colors
(set-face-attribute
 'company-tooltip-selection nil :background "maroon" :foreground "#0d1017")

;; powerline and doom require full icon pack
(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; (use-package all-the-icons
;;   :if (display-graphic-p)
;;   :straight t
;;   :commands all-the-icons-install-fonts
;;   :init
;;   (unless (find-font (font-spec :name "all-the-icons"))
;;     (all-the-icons-install-fonts t))
  ;; )

(use-package emojify
  :straight t
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(provide '70-theme)
