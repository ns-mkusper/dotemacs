(use-package ivy
  :straight t
  :bind
  (:map ivy-minibuffer-map
        ;; close the minibuffer with ESC
        ("<escape>" . 'minibuffer-keyboard-quit))
  :custom
  (ivy-use-virutal-buffers t)
  (ivy-count-format "(%d/%d) ")
  :config
  ;; ability to select what's in the prompt when there's a partial match below it in the ivy results
  (setq ivy-use-selectable-prompt t)
  ;; Allow command issue in minibuffer
  (when (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode 1))

  ;; more
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))
  )

(use-package counsel
  :straight t
  :diminish ivy-mode counsel-mode
  :defines
  (projectile-completion-system magit-completing-read-function)
  :hook
  (after-init . ivy-mode)
  (ivy-mode . counsel-mode)
  :bind
  ("M-x" . 'counsel-M-x)
  ("M-y" . 'counsel-yank-pop)
  ("C-M-z" . 'counsel-fzf)
  ("C-M-r" . 'counsel-recentf)
  ("C-x C-b" . 'counsel-ibuffer)
  ("C-x C-i" . 'counsel-semantic-or-imenu)
  :custom
  ;; Include recent files and bookmarks in `ivy-switch-buffer' (Cx b) list
  (ivy-use-virtual-buffers t)

  ;; Wrap prompt
  (ivy-truncate-lines nil)

  ;; If `Cp' at the beginning of the list, move to the end of the list
  (ivy-wrap t)

  ;; Do not show unnecessary files in `counsel-find-file'
  (counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions))
  :config
  (use-package ivy-hydra
    :straight t
    :custom
    ;; Assign Mo to ivy-hydra-read-action
    (ivy-read-action-function #'ivy-hydra-read-action)
    )
  )

(use-package all-the-icons-ivy-rich
  :straight t
  :after (counsel-projectile)
  :config
  ;; counsel-projectile-find-file
  (plist-put all-the-icons-ivy-rich-display-transformers-list
             'counsel-projectile-find-file
             '(:columns
               (
                (ivy-read-file-transformer)
                (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face)))))
  (all-the-icons-ivy-rich-mode 1)
  )

(use-package ivy-rich
  :straight t
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  ;; counsel-projectile-find-file
  (plist-put ivy-rich-display-transformers-list
             'counsel-projectile-find-file
             '(:columns
               (
                (ivy-read-file-transformer)
                (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face)))))
  (ivy-rich-mode 1))

(use-package swiper
  :straight t
  :bind
  ("C-s" . 'swiper)
  :custom
  (swiper-include-line-number-in-search t)
  :config
  ;; Enable preview in counsel-imenu
  (ivy-configure 'counsel-imenu
    :update-fn 'auto)
  )

(use-package avy
  :straight t )

(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode 1)
  )

(use-package ivy-yasnippet
  :straight t )

(use-package ivy-pass
  :straight t )

(provide '40-ivy)
