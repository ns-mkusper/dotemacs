;; in-emacs gpt prompt
(use-package gptel
  :straight (gpt :type git :host github :repo "karthink/gptel"))

;; allows code generation from gpt
(use-package codegpt
  :straight (codegpt :type git :host github :repo "emacs-openai/codegpt"))

;; TODO: add support for gemini?

(provide '60-gpt)
