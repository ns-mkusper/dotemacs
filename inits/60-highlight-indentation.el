(use-package highlight-indent-guides
  :straight t
  :delight
  (highlight-indent-guides-mode)
  :commands
  (highlight-indent-guides-auto-set-faces highlight-indent-guides-mode)
  :preface
  (defun highlight-indent-guides-auto-set-faces-with-frame (frame)
    (with-selected-frame frame
      (highlight-indent-guides-auto-set-faces)))
  :hook
  (highlight-indent-guides-mode-hook . highlight-indent-guides-auto-set-faces)
  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-responsive 'top)
  ;; (highlight-indent-guides-character ?|)
  (highlight-indent-guides-delay 0.05)
  (highlight-indent-guides-auto-odd-face-perc 5)
  (highlight-indent-guides-auto-even-face-perc 5)
  (highlight-indent-guides-auto-character-face-perc 10))

;; make indentation more visible in files where it dictates scope
;; (use-package highlight-indentation
;;   :straight t
;;   :hook
;;   (prog-mode . highlight-indentation-mode)
;;   (prog-mode . highlight-indentation-current-column-mode)
;;   (python-mode . highlight-indentation-mode)
;;   (python-mode . highlight-indentation-current-column-mode)
;;   (yaml-mode . highlight-indentation-mode)
;;   (yaml-mode . highlight-indentation-current-column-mode)
;;   :config
;;   (set-face-background 'highlight-indentation-face "grey15")
;;   (set-face-background 'highlight-indentation-current-column-face "grey25")),


(provide '60-highlight-indentation)
