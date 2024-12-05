(use-package python
  :straight t
  :init
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq-default python-indent-guess-indent-offset nil)
  (setq python-indent-offset 4)
  (setq highlight-indentation-offset 4)
  (setq fill-column 100)
  (fci-mode)
  ;; TODO: explore eglot as an alternative
  ;; :hook ((python-mode . eglot-ensure))
  :bind (:map python-mode-map
              ;; Force the execution of the main block
              ("C-c C-b" .  (lambda () (interactive) (python-shell-send-buffer t)))))

(use-package poetry
  :defer t
  :straight (:build t)
  :commands (poetry-venv-toggle
             poetry-tracking-mode)
  :config
  (setq poetry-tracking-strategy 'project)
  (add-hook 'python-mode-hook #'poetry-tracking-mode))

(use-package python-pytest
  :hook
  (python-mode . python-black-on-save-mode)
  (python-ts-mode . python-black-on-save-mode)
  )

(use-package python-isort
  :hook
  (python-mode . python-isort-on-save-mode)
  (python-ts-mode . python-isort-on-save-mode)
  )

(use-package python-black)

(use-package ruff-format
  ;; :hook
  ;; (python-mode . ruff-format-on-save-mode)
  ;; (python-ts-mode . ruff-format-on-save-mode)
  )

;; Virtual Environment Management
(use-package pet
  :ensure t
  :ensure-system-package (sqlite3 dasel)
  :commands (pet-mode)
  :init (add-hook 'python-base-mode-hook 'pet-mode -10)
  :config
  ;; Pyright requires specific config to find our virtual environment
  ;; see: https://robbmann.io/posts/emacs-eglot-pyrightconfig/
  (defun my/pyrightconfig-write (virtualenv)
    (interactive "DEnv: ")

    (let* (;; file-truename and tramp-file-local-name ensure that neither `~' nor
           ;; the Tramp prefix (e.g. "/ssh:my-host:") wind up in the final
           ;; absolute directory path.
           (venv-dir (tramp-file-local-name (file-truename virtualenv)))

           ;; Given something like /path/to/.venv/, this strips off the trailing `/'.
           (venv-file-name (directory-file-name venv-dir))

           ;; Naming convention for venvPath matches the field for
           ;; pyrightconfig.json.  `file-name-directory' gets us the parent path
           ;; (one above .venv).
           (venvPath (file-name-directory venv-file-name))

           ;; Grabs just the `.venv' off the end of the venv-file-name.
           (venv (file-name-base venv-file-name))

           ;; Eglot demands that `pyrightconfig.json' is in the project root
           ;; folder.
           (base-dir (vc-git-root default-directory))
           (out-file (expand-file-name "pyrightconfig.json" base-dir))

           ;; Finally, get a string with the JSON payload.
           (out-contents (json-encode (list :venvPath venvPath :venv venv))))

      ;; Emacs uses buffers for everything.  This creates a temp buffer, inserts
      ;; the JSON payload, then flushes that content to final `pyrightconfig.json'
      ;; location
      (with-temp-file out-file (insert out-contents))))

  )


;; flymake-ruff integration
(use-package flymake-ruff
  :straight (flymake-ruff
             :type git
             :host github
             :repo "erickgnavar/flymake-ruff"))

(provide '60-python)
