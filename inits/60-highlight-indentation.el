(use-package highlight-indent-guides
  :straight t
  :diminish highlight-indent-guides-mode
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'toml-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-auto-odd-face-perc 75)
  (setq highlight-indent-guides-auto-even-face-perc 75)
  (setq highlight-indent-guides-auto-character-face-perc 90)
  ;; (defun indent-guides-init-faces ()
  ;;   "Set indent-guides faces"
  ;;   (interactive)
  ;; ;;   (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  ;; ;;   (set-face-background 'highlight-indent-guides-even-face "dimgray")
  ;; ;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  ;;   )
  )

;; make indentation more visible in files where it dictates scope
;; (use-package highlight-indentation
;;   :straight t
;;   :hook
;;   (prog-mode . highlight-indentation-mode)
;;   (prog-mode . highlight-indentation-current-column-mode)
;;   (python-mode . highlight-indentation-mode)
;;   (python-mode . highlight-indentation-current-column-mode)
;;   (yaml-mode . highlight-indentation-mode)
;;   (yaml-mode . highlight-indentation-current-column-mode)
;;   :config
;;   (set-face-background 'highlight-indentation-face "grey15")
;;   (set-face-background 'highlight-indentation-current-column-face "grey25"))

(provide '60-highlight-indentation)
