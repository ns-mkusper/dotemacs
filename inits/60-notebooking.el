;; Use pandoc to convert files
(use-package pandoc-mode
  :defer 10
  :straight t
  :bind (:map pandoc-mode-map (("C-c /" . nil)))
  :hook (text-mode . pandoc-mode))

(use-package ox-ipynb
  :straight (:host github :repo "jkitchin/ox-ipynb"))
