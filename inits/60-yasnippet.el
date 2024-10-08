(use-package yasnippet
  :straight t
  :diminish (yas-global-mode yas-minor-mode)
  :commands (yas-minor-mode)
  ;; :commands (yas-reload-all yas-minor-mode)
  :bind
  ((:map yas-minor-mode-map
         ("C-x i i" . yas-insert-snippet)
         ("C-x i n" . yas-new-snippet)
         ("C-x i v" . yas-visit-snippet-file)))
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :hook (prog-mode . yas-minor-mode)
  :config
  (setq yas-snippet-dirs '("~/drive/emacs/snippets"))
  (yas-initialize)
  (yas-reload-all)
  )

(use-package yasnippet-snippets
  :straight t
  :after (yasnippet)
  :config
  (add-to-list 'yas-snippet-dirs yasnippet-snippets-dir)
  )

(provide '60-yasnippet)
