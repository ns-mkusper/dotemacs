;; No garbage collection at startup
(setq gc-cons-threshold-default gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

;; Custom init.el emacs config
(global-unset-key (kbd "C-z"))

;; user-emacs-directory
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; (if (file-directory-p (locate-user-emacs-file (concat "elpa/" emacs-version)))
;;     (setq package-user-dir (locate-user-emacs-file (concat "elpa/" emacs-version))))

;; Don't warn `Package cl is deprecated' when using (require 'cl)
(setq byte-compile-warnings '(not cl-functions obsolete))


;; Add additional repos
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("elpa" .  "https://elpa.gnu.org/packages/" ) t)

;; include all installed packages so far to load-path
;; (let ((base package-user-dir))
;;   (add-to-list 'load-path base)
;;   (dolist (f (directory-files base))
;;     (let ((name (concat base "/" f)))
;;       (when (and (file-directory-p name)
;;                  (not (equal f ".."))
;;                  (not (equal f ".")))
;;         (add-to-list 'load-path name)))))

;; set gpg home dir when on Windows to avoid strange path loop issue
;; see: https://www.reddit.com/r/emacs/comments/ymzw78/windows_elpa_gnupg_and_keys_problems/
(when-let (cygpath (executable-find "cygpath.exe"))
  (setopt package-gnupghome-dir
          (with-temp-buffer
            (call-process cygpath nil t nil
                          "-u" (default-value 'package-gnupghome-dir))
            (string-trim (buffer-string)))))

;; setup straight
(defvar straight-bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (straight-bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; TODO: run only when stale
;; (package-refresh-contents) ;; can be disabled and ran manually to speed up boot

;; (package-initialize) ;; no longer needed after package was removed?

(use-package straight
  :custom (straight-use-package-by-default t))

(use-package use-package-ensure-system-package
  :straight t)


;; custom file
(setq custom-file (expand-file-name
                   (concat user-emacs-directory "my-custom-vars.el")))
(load custom-file) ;; install packages and suppress output

(use-package s
  :straight t )
(use-package use-package)
;; load all our sub-config packages
(use-package init-loader
  :straight t
  :config
  (setq init-loader-show-log-after-init nil)
  (init-loader-load "~/.emacs.d/inits"))

;; fontify all buffers
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
  :straight t
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
