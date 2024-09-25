;; copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :bind (:map copilot-mode-map
              ("<tab>" . my/copilot-tab)
              ("M-C-<" . copilot-previous-completion)
              ("M-C->" . copilot-next-completion)
              ("M-C-<tab>" . copilot-accept-completion-by-line))
  :config

  (defun my/copilot-tab ()
    "Call copilot if you can, else company else yasnippet, else indent."
    (interactive)
    (or (copilot-accept-completion)
        (company-indent-or-complete-common nil)
        ;; (company-yasnippet-or-completion)
        (indent-for-tab-command nil))))

;; disabled for being buggy
;; (use-package copilot-chat
;;   :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
;;   :after (request shell-maker)
;;   :custom
;;   (copilot-chat-frontend 'shell-maker)
;;   :config
;;   (require 'copilot-chat-shell-maker)
;;   (push '(shell-maker . copilot-chat-shell-maker-init) copilot-chat-frontend-list)
;;   (copilot-chat-shell-maker-init))

(provide '60-copilot)
