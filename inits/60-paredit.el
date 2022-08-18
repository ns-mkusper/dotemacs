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

(provide '60-paredit)
