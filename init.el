;; customize file for cocoa emacs

(global-unset-key (kbd "C-z"))

;; user-emacs-directory
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(setq package-user-dir (locate-user-emacs-file (concat "elpa/" emacs-version)))

;; Don't warn `Package cl is deprecated' when using (require 'cl)
(setq byte-compile-warnings '(not cl-functions obsolete))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(setq package-archives
      '(("celpa" . "https://celpa.conao3.com/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))


(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(package-refresh-contents)
;; install packages and suppress output
(setq custom-file (expand-file-name
                   (concat user-emacs-directory "my-custom-vars.el")))
(load custom-file)
(package-initialize)

(package-refresh-contents)

(use-package s
  :ensure  t )
(use-package use-package)
(use-package init-loader
  :ensure  t
  :config
  (setq init-loader-show-log-after-init nil)
  (init-loader-load "~/.emacs.d/inits"))

(global-font-lock-mode  t)
