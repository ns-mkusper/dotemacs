(use-package sql-indent
  :straight t
  :hook (sql-mode . sqlind-minor-mode))

(use-package sqlformat
  :straight t
  ;; TODO: get working on mac/windows
  ;; :ensure-system-package (pg_format . "sudo apt install pgformatter")
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-s2" "-g"))
  :hook (sql-mode . sqlformat-on-save-mode)
  :bind (:map sql-mode-map ("C-c C-f" . sqlformat)))

(use-package flymake-sqlfluff
  :straight (flymake-sqlfluff
             :type git
             :host github
             :repo "erickgnavar/flymake-sqlfluff"))


(provide '60-sql)
