(use-package zeal-at-point
  :straight t
  :straight (shfmt :host github
                   :repo "ns-mkusper/zeal-at-point"
                   :local-repo "ns-mkusper/zeal-at-point"
                   :branch "master"
                   )
  :bind
  ("\C-cz" . 'zeal-at-point)
  )

(provide '60-zeal)
