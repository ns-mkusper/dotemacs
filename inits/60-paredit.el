(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
	 (lisp-mode . enable-paredit-mode)
	 (clojure-mode . enable-paredit-mode)
	 (cider-repl-mode . enable-paredit-mode)
	 (ielm-mode . enable-paredit-mode)
	 (scheme-mode . enable-paredit-mode)
	 (geiser-repl-mode . enable-paredit-mode)
	 (slime-repl-mode . enable-paredit-mode)))

(use-package paredit-everywhere
  :ensure t
  :disabled t
  :init (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

(provide '60-paredit)
