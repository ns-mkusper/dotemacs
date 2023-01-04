(use-package zeal-at-point
  :straight (zeal-at-point :host github
                           :repo "ns-mkusper/zeal-at-point"
                           :local-repo "zeal-at-point"
                           :branch "master"
                           )
  :bind
  ("\C-cz" . 'zeal-at-point)
  :config
  (add-to-list 'zeal-at-point-mode-alist '(rustic-mode . "rust"))
  )

(provide '60-zeal)
