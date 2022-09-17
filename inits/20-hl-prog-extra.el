(use-package hl-prog-extra
  :ensure t
  :commands (hl-prog-extra-mode)
  :config
  (setq hl-prog-extra-list
		(list
		 ;; Match TKs in quotation marks (hl-prog-extra sees them as strings)
		 '("\\(TK\\)+" 0 string '(:weight bold :inherit font-lock-warning-face))
		 ;; Match TKs not in quotation marks
		 '("\\(TK\\)+" 0 nil '(:weight bold :inherit font-lock-warning-face)))))
