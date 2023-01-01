(use-package flycheck
  :straight t
  :commands (global-flycheck-mode)
  :bind
              ("C-c C-c l" . flycheck-list-errors)
  
  :hook
  (after-init . global-flycheck-mode)
  :init
  (push '("*Flycheck errors*") popwin:special-display-config)
  :config
  ;; https://www.emacswiki.org/emacs/CfnLint
  (flycheck-define-checker cfn-lint
                           "AWS CloudFormation linter using cfn-lint.

Install cfn-lint first: pip install cfn-lint

See `https://github.com/aws-cloudformation/cfn-python-lint'."
                           :command ("cfn-lint" "-f" "parseable" source)
                           :error-patterns ((warning line-start (file-name) ":" line ":" column
                                                     ":" (one-or-more digit) ":" (one-or-more digit) ":"
                                                     (id "W" (one-or-more digit)) ":" (message) line-end)
                                            (error line-start (file-name) ":" line ":" column
                                                   ":" (one-or-more digit) ":" (one-or-more digit) ":"
                                                   (id "E" (one-or-more digit)) ":" (message) line-end))
                           :modes (cfn-json-mode cfn-yaml-mode))
  (add-to-list 'flycheck-checkers 'cfn-lint)
  )

(provide '60-flycheck)
