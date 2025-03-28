;; CUSTOM KEYS
(bind-keys ([f6] . visual-line-mode)
           ([f7] . display-line-numbers-mode)
           ([f10] . menu-bar-mode)
           ([f2] . breadcrumb-mode)
           ("C-c m" . my/switch-to-minibuffer-window)
           ;; ("C-c r" . my/revert-buffer-no-confirm)
           ("C-c s" . my/open-scratch-buffer)
           ;; TODO: ensure canvas-mode works on mac & windows
           ;; see: https://www.reddit.com/r/emacs/comments/pk5ad6/draw_and_scribble_in_gnu_emacs/
           ;; ("C-c c" . my/open-canvas-buffer)
           ("C-c t" . my/open-text-buffer)
           ("C-c RET" . my-proj/shell)
           ("C-x C-b" . ibuffer)
           ("C-M-n" . forward-paragraph)
           ("C-M-p" . backward-paragraph)
           ("C-x a r". align-regexp)
           ("C-x a c" . center-line)
           ("M-C-h" . backward-kill-word)
           ("M-C-r" . query-replace)
           ("M-o" . replace-string)
           ("M-O" . replace-regexp)
           ("M-G" . goto-line)
           ;; ("M-h" . help-command)
           ("C-;" . comment-or-uncomment-region)
           ("C-x C-e" . my/eval)
           ("C-c o" . org-store-link)
           ("C-c a" . org-agenda)
           ("C-M-n" . my/forward-down-list)
           ("C-c i" . kprompt-bard)
           )

(use-package general
  :straight t)

(provide '01-custom-keys)
