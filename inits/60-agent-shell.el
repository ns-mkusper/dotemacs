(use-package agent-shell
  :straight
  :ensure t
  :init
  ;; Load secrets.el (don't error if missing)
  (load (expand-file-name "secrets.el" user-emacs-directory) t)

  :config
  ;; Check for the variable YOU confirmed you have in secrets.el
  (if (and (boundp 'my-gemini-api-key)
           (stringp my-gemini-api-key)
           (not (string-empty-p my-gemini-api-key)))
      (progn
        ;; DEBUG: Print the last 4 chars of the key to the Messages buffer to prove which one is loaded
        (message "✅ Agent-Shell: Configuring with key ending in ...%s"
                 (substring my-gemini-api-key -4))

        ;; 1. Configure Authentication using your secret variable
        (setq agent-shell-google-authentication
              (agent-shell-google-make-authentication :api-key my-gemini-api-key))

        ;; 2. Configure Environment (passes key to the underlying CLI tool)
        (setq agent-shell-google-gemini-environment
              (agent-shell-make-environment-variables
               "GOOGLE_API_KEY" my-gemini-api-key
               ;; Explicitly DISABLE Vertex AI to force API Key usage
               "GOOGLE_GENAI_USE_VERTEXAI" nil
               :inherit-env t)))

    ;; Fallback: Warn if the variable is missing
    (warn "❌ Agent-Shell Error: 'my-gemini-api-key' is empty or missing in secrets.el!")))
