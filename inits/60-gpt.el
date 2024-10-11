(defun my/gptel-api-key (host)
  (let ((secret (auth-source-pick-first-password
                 :host host)))
    secret))
;; in-emacs gpt prompt
(use-package gptel
  :straight (gpt :type git :host github :repo "karthink/gptel")
  :bind
  ("C-c C-a" . 'gptel)


  :custom
  ;; default curl on windows doesn't work with gptel
  ;; see: https://github.com/karthink/gptel/issues/90
  ;; TODO: get working with msys2 curl on windows
  (gptel-use-curl nil)
  :config
  ;; Setup claude
  (setq
   gptel-model "claude-3-sonnet-20240229" ;  "claude-3-opus-20240229" also available
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t :key (my/gptel-api-key "api.claude.com")))
  (setq
   gptel-model "gemini-pro"
   gptel-backend (gptel-make-gemini "Gemini"
                   :stream t :key (my/gptel-api-key "generativelanguage.googleapis.com")))
  )

;; allows code generation from gpt
(use-package codegpt
  :straight (codegpt :type git :host github :repo "emacs-openai/codegpt"))

;; TODO: add support for gemini?

(provide '60-gpt)
