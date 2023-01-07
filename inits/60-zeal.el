(use-package zeal-at-point
  :straight (zeal-at-point :host github
                           :repo "ns-mkusper/zeal-at-point"
                           :local-repo "ns-mkusper/zeal-at-point"
                           :branch "master"
                           )
  :bind
  ("\C-cz" . 'zeal-at-point)
  :config
  (add-to-list 'zeal-at-point-mode-alist '(rustic-mode . "rust"))
  (add-to-list 'zeal-at-point-mode-alist '(gitlab-ci-mode . "gitlab-ci"))
  )

(provide '60-zeal)
