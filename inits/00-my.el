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

(my-load-path "~/.emacs.d/lisp")
;;(my-load-path "/usr/share/emacs/site-lisp/")

;;; General Settings
(setq inhibit-startup-message t)
(transient-mark-mode t)
(menu-bar-mode -1)
(savehist-mode 1) ;; save shell history
(setq-default save-place t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on) ;; colorize shell
(setq compilation-window-height 45)
(setq debug-on-error nil) ;; ignore errors in emacs
(mouse-avoidance-mode 'jump)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(line-number-mode 1)
(setq-default indicate-empty-lines t)
;; show cursor position
(setq column-number-mode 1)
(setq line-number-mode t)
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

;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;; Corfu commands are hidden, since they are not supposed to be used via M-x.
;; (setq read-extended-command-predicate
;;       #'command-completion-default-include-p)

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
;; remove window decoration
(set-frame-parameter nil 'undecorated t)
;; resize window to full screen dimensions
(setq initial-frame-alist '( (fullscreen . maximized)))
(setq default-frame-alist '( (fullscreen . fullheight)))
;; delete trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq backup-directory-alist '(("." . "~/.emacs_backups")))
