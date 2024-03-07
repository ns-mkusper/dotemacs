;; PROTOBUF

(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))
(use-package protobuf-mode
  :straight t
  :init
  :mode
  ("\\.proto$" . protobuf-mode)
  :config
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t)))
  )

(provide '60-proto)
