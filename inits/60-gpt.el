;; in-emacs gpt prompt
(use-package gpt
  :straight (gpt :type git :host github :repo "stuhlmueller/gpt.el"))


;; allows code generation from gpt
(use-package codegpt
  :straight (codegpt :type git :host github :repo "emacs-openai/codegpt"))
