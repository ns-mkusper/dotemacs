;; CUSTOM KEYS
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-x a r") 'center-line)
(global-set-key "\M-\C-h" 'backward-kill-word)
(global-set-key "\M-\C-r" 'query-replace)
(global-set-key "\M-r" 'replace-string)
(global-set-key "\M-R" 'replace-regexp)
(global-set-key "\M-G" 'goto-line)
(global-set-key "\M-h" 'help-command)
(global-set-key "C-;" 'comment-or-uncomment-region)

(provide '01-custom-keys)
