(use-package apheleia
  :defines (apheleia-formatters apheleia-mode-alist)
  :hook (after-init . apheleia-global-mode)
  :config
  ;; Use 'ruff' instead of 'black'. Remove 'ruff-isort' when 'ruff format'
  ;; supports it.
  ;; Check - https://docs.astral.sh/ruff/formatter/#sorting-imports
  ;; https://github.com/astral-sh/ruff/issues/8232
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

(provide '60-apheleia)
