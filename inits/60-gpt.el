(defun my/gptel-api-key (host)
  (let ((secret (auth-source-pick-first-password
                 :host host)))
    secret))
;; in-emacs gpt prompt
(use-package gptel
  :straight (gpt :type git :host github :repo "karthink/gptel")
  :config
  ;; Setup claude
  (setq
   gptel-model "claude-3-sonnet-20240229" ;  "claude-3-opus-20240229" also available
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t :key (my/gptel-api-key "api.claude.com")))

  )

;; allows code generation from gpt
(use-package codegpt
  :straight (codegpt :type git :host github :repo "emacs-openai/codegpt"))

;; TODO: add support for gemini?

(provide '60-gpt)
