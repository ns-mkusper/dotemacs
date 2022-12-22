(use-package perspective
  :straight t
  :bind-keymap
  ("C-z" . perspective-map)
  :bind
  ("C-x b" . 'persp-counsel-switch-buffer)
  ("C-x C-M-b" . 'persp-bs-show)
  (:map perspective-map
        ("SPC" . persp-switch)
        ("k" . persp-kill)
        ("C-k" . persp-kill)
        ("M-k" . persp-kill)
        ("C-p" . persp-prev)
        ("C-n" . persp-next)
        ("a" . persp-switch-last)
        ("C-a" . persp-switch-last)
        ("c" . persp-switch)
        )
  :custom
  (persp-state-default-file "~/.emacs.d/data/persp-state-file")
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :hook
  (kill-emacs . persp-state-save)
  :config
  (persp-mode)
  )

(provide '40-perspective)
