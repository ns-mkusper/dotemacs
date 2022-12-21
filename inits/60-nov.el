;; Read epubs in emacs
(use-package nov
  :straight t
  :preface
  (defun my/nov-delayed-render-setup ()
    (run-with-idle-timer 0.2 nil 'nov-render-document))
  (defun my/nov-fringes-setup ()
    "Hide the fringes for `nov-mode'."
    (set-window-fringes (get-buffer-window) 0 0 nil))
  :mode
  ("\\.epub$" . nov-mode)
  :hook
  (nov-mode . my/nov-delayed-render-setup)
  (nov-mode . my/nov-fringes-setup))

(provide '60-nov)
