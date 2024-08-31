(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :general
  ('insert copilot-mode-map
           "C-f" 'copilot-accept-completion
           "M-f" 'copilot-accept-completion-by-word
           "C-e" 'copilot-accept-completion-by-line
           "M-p" 'copilot-previous-completion
           "M-n" 'copilot-next-completion))

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request shell-maker)
  :custom
  (copilot-chat-frontend 'shell-maker)
  :config
  (require 'copilot-chat-shell-maker)
  (push '(shell-maker . copilot-chat-shell-maker-init) copilot-chat-frontend-list)
  (copilot-chat-shell-maker-init))

(provide '60-copilot)
