(use-package apheleia
  :straight t
  :config
  (apheleia-global-mode +1)

  ;; Explicitly tell Apheleia to use Prettier for TS/JS
  ;; it respects .editorconfig
  (setf (alist-get 'typescript-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  )

(provide '60-apheleia)
