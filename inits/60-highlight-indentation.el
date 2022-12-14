;; make indentation more visible in files where it dictates scope
(use-package highlight-indentation
  :straight t
  :hook
  (prog-mode . highlight-indentation-mode)
  (prog-mode . highlight-indentation-current-column-mode)
  (python-mode . highlight-indentation-mode)
  (python-mode . highlight-indentation-current-column-mode)
  (yaml-mode . highlight-indentation-mode)
  (yaml-mode . highlight-indentation-current-column-mode)
  :config
  (set-face-background 'highlight-indentation-face "grey15")
  (set-face-background 'highlight-indentation-current-column-face "grey25"))

(provide '60-highlight-indentation)
