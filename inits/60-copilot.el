;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :hook (prog-mode . copilot-mode)
;;   :general
;;   ('insert copilot-mode-map
;;            "C-f" 'copilot-accept-completion
;;            "M-f" 'copilot-accept-completion-by-word
;;            "C-e" 'copilot-accept-completion-by-line
;;            "M-p" 'copilot-previous-completion
;;            "M-n" 'copilot-next-completion))


(provide '60-copilot)
