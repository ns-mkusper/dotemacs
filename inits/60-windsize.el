;; resize windows with arrow keys
; https://www.emacswiki.org/emacs/download/lazy-set-key.el
(require 'lazy-set-key)
(use-package windsize
  :straight t
  :config
  (windsize-default-keybindings))

(provide '60-windsize)
