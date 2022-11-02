;; Modular in-buffer completion framework for Emacs
;; http://company-mode.github.io/
(use-package company
  :diminish company-mode
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq
   company-echo-delay 0
   company-idle-delay 0.2
   company-minimum-prefix-length 1
   company-tooltip-align-annotations t
   company-tooltip-limit 20)
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))

;; Ansible keywords completion for Emacs
;; https://github.com/krzysztof-magosa/company-ansible
(use-package company-ansible :ensure t)

;; Auto-completion for C/C++ headers using Company
;; https://github.com/randomphrase/company-c-headers
(use-package company-c-headers
  :ensure t
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                (add-to-list 'company-backends 'company-c-headers)))))

;; company-mode completion back-end for irony-mode
;; https://github.com/Sarcasm/company-irony
(use-package company-irony
  :ensure t
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                (progn
                  (add-to-list 'company-backends 'company-irony)
                  (irony-mode))))))

;; Python auto-completion for Emacs
;; http://tkf.github.io/emacs-jedi/latest/
;; Requires: `pip install jedi`
;; Company backend for Python jedi
;; https://github.com/syohex/emacs-company-jedi
(use-package company-jedi
  :ensure t
  :init
  (setq-default
   jedi:complete-on-dot t
   jedi:get-in-function-call-delay 0.2))

(use-package company-lua :ensure t)

;; https://github.com/rafalcieslak/emacs-company-terraform
(use-package company-terraform :ensure t)
