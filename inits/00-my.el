;; safe loading of configs
(defmacro exec-if-bound (sexplist)
  "Only run the function if it exists (just check fboundp of car)."
  `(if (fboundp (car ',sexplist))
       ,sexplist))
(defmacro defun-add-hook (hookname &rest sexplist)
  "Alias for add-hook. Adds arguments back to a function to hook."
  `(add-hook  ,hookname
              (function (lambda () ,@sexplist))))
(defun load-safe (loadlib)
  "Safe load. Don't stop if load fails."
  (let ((load-status (load loadlib t)))
    (or load-status
        (message (format "[load-safe] failed %s" loadlib)))
    load-status))
(defun my-load-path (path)
  "Function for adding load-path."
  (let ((epath (expand-file-name path)))
    (unless (member epath load-path)
      (setq load-path (cons epath load-path)))))
;; modeline fix
(defun my-which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))
;; eval-last-sexp if no region selected, eval-region if selected
(defun my-eval ()
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning) (region-end) t)
    (call-interactively #'eval-last-sexp)))

(defun my-switch-to-minibuffer-window ()
  "Switch to the minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun my-revert-buffer-no-confirm ()
  "Revert the current buffer without confirmation."
  (interactive)
  (if (not (buffer-modified-p))
      (revert-buffer :ignore-auto :noconfirm)
    (when (yes-or-no-p "The contents of this buffer have been modified.  Really revert? ")
      (revert-buffer :ignore-auto :noconfirm))))

(defun my-open-scratch-buffer ()
  "Open the scratch buffer, (re)creating it if not present."
  (interactive)
  (if (get-buffer "*scratch*")
      (switch-to-buffer "*scratch*")
    (progn
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (lisp-interaction-mode))))

(defun my-get-shell-buffer-name ()
  "Return either either the project name or filename if outside of a project."
  (if  (member "git" (split-string buffer-file-name "/"))
      (setq shell-buffer-name-local-segment (nth 1 (member "git" (split-string buffer-file-name "/") )))
    (setq shell-buffer-name-local-segment (nth 0 (last (split-string buffer-file-name "/") )))
    )

  (setq shell-buffer-name (concat "*shell*<" shell-buffer-name-local-segment ">"))
  )

(defun my-open-default-shell ()
  "Opens or switches to a shell dedicated to the current project or file (if outside of a project)."
  (interactive)
  (setq shell-buffer-name (my-get-shell-buffer-name))
  (if (get-buffer shell-buffer-name)
      (switch-to-buffer-other-window shell-buffer-name)
    (progn
    (call-interactively 'shell)
    (rename-buffer shell-buffer-name)))
  )

(my-load-path "~/.emacs.d/lisp")

;; setup environment variables
(defvar my-env-shell-path
      (concat
         (getenv "PATH")
         "/usr/local/bin/:"
         "/usr/local/opt/coreutils/bin/:"
         "/opt/homebrew/bin/:"
         "/usr/local/opt/texinfo/bin/:"
         (concat (getenv "HOME") "/bin/:")
         (concat (getenv "HOME") "/go/bin:")
         (concat (getenv "HOME") "/.local/bin:")
         (concat (getenv "HOME") "/.cargo/bin:")
         ))
(setenv "PATH" my-env-shell-path)
(setq exec-path (split-string my-env-shell-path path-separator))

;;; General Settings
(setq inhibit-startup-message t)
(transient-mark-mode t)
(menu-bar-mode -1)
(savehist-mode 1) ;; save shell history
(save-place-mode 1)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on) ;; colorize shell
(setq compilation-window-height 45)
(setq debug-on-error nil) ;; ignore errors in emacs
(mouse-avoidance-mode 'jump)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq-default indicate-empty-lines t)
(setq-default display-fill-column-indicator-column 100)
;; show cursor position
(setq column-number-mode 1)
(blink-cursor-mode 1) ;; Make Cursor more visible
;; show matching braces
(show-paren-mode t)
(setq show-paren-style 'expression)
(auto-image-file-mode  t) ;; auto show images
(auto-compression-mode t) ;; edit compressed files w/o prompt
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(setq undo-limit 100000)
(setq fill-column 80)
(setq case-fold-search t)
(setq show-trailing-whitespace t)
(setq c-default-style "bsd")
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil) ;; only use space indenting
(setq completion-cycle-threshold 3) ;; TAB cycle if there are only few candidates
;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)
(setq font-lock-maximum-decoration t) ;; enable bold-italic styles
(setq select-enable-clipboard t) ;; combine clipboards
;; copy to x clipboard
(setq interprogram-paste-function 'x-selection-value)
(setq truncate-partial-width-windows nil)
(setq initial-scratch-message nil) ;; Eliminate *scratch* strings
;;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq display-time-24hr-format t)
(display-time)
;; avoid encoding issues (so long as we always use utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq transient-mark-mode t)
(setq frame-title-format
      (format "emacs@%s: %%f" (system-name))) ;; Show file name in title bar
;; ;; use window decoration
;; (set-frame-parameter nil 'undecorated nil)
;; ;; resize window to full screen dimensions
;; (setq initial-frame-alist '( (fullscreen . maximized)))
;; (setq default-frame-alist '( (fullscreen . fullheight)))
(setq backup-directory-alist '(("." . "~/.emacs_backups")))
