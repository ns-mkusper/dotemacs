;; No garbage collection at startup
(setq gc-cons-threshold-default gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

;; Custom init.el emacs config
(global-unset-key (kbd "C-z"))

;; user-emacs-directory
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(if (file-directory-p (locate-user-emacs-file (concat "elpa/" emacs-version)))
    (setq package-user-dir (locate-user-emacs-file (concat "elpa/" emacs-version))))

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


;; include all installed packages so far to load-path
(let ((base package-user-dir))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

;; use-package setup
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(package-refresh-contents) ;; can be disabled and ran manually to speed up boot
(package-initialize)
(setq custom-file (expand-file-name
                   (concat user-emacs-directory "my-custom-vars.el")))
(load custom-file) ;; install packages and suppress output

(use-package s
  :ensure  t )
(use-package use-package)
;; load all our sub-config packages
(use-package init-loader
  :ensure  t
  :config
  (setq init-loader-show-log-after-init nil)
  (init-loader-load "~/.emacs.d/inits"))

(global-font-lock-mode  t)

;; GARBAGE COLLECTING
;; reset GC back to default to reverse startup workaround
(setq gc-cons-threshold gc-cons-threshold-default)
;; larger mark-ring ceiling since we have typically have more than
;; enough memory
(setq global-mark-ring-max 256
      mark-ring-max 256
      kill-ring-max 256)
;; enable gcmh which is a gc hack that sensibly adjusts the gc size
;; widnow and defers gc'n to idle time
(use-package gcmh
  :ensure t
  :demand t
  :config (progn
            (setq gcmh-high-cons-threshold (* 256 1024 1024)
                  gcmh-low-cons-threshold (* 1 1024 1024))
            (defun my-enable-gcmh ()
              (setq gc-cons-threshold (* 256 1024 1024))
              (gcmh-mode 1))
            (add-hook 'emacs-startup-hook #'my-enable-gcmh)
            )
  )

(provide 'init)
