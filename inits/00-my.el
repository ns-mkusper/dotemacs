(defun my/number-lines-region (start end)
  "Number a selected region of lines."
  (interactive "r")
  (save-excursion
    (let ((line-num 1))
      (goto-char start)
      (while (re-search-forward "^" end t)
        (replace-match (format "%d. " line-num) t t)
        (setq line-num (1+ line-num))))))

(defun my/generate-weekly-todos (start-date start-value num-weeks message mutator mutator-arg)
  "Generates TODO org-mode agenda items for a number of weeks.
    (my-generate-weekly-todos (current-time) 82 10 \"reach weight (kg)\" '- 1.6)

  Args:
    start-date: The date to start generating agendas from. (e.g., (date 2024 04 28))
    start-value: value at current date
    num-weeks: The number of weeks to generate agendas for.
    message: The message for each agenda item.
    mutator: The function used to mutate the value week-by-week.
    mutator-arg: The argument to pass to the mutator function.

  Returns:
    A string containing the org-mode entries for the agenda items."
  (let ((org-string "") (current-date start-date))
    (dotimes (i num-weeks)
      (let ((value (funcall mutator start-value (* mutator-arg i))))

        (setq org-string (concat org-string
                                 (format "* TODO %s: %s \n DEADLINE: <%s>\n"
                                         message value (format-time-string "%Y-%m-%d" current-date)))))
      (setq current-date (time-add current-date (days-to-time 7)))
      )
    org-string))

;; easily set keymaps
(defmacro my/defkeymap (name prefix &rest bindings)
  "Create a new NAME-keymap bound to PREFIX, with BINDINGS.

Usage:
  (my/defkeymap \"spook-git\" \"C-c g\"
    '(\"s\" . magit-status))
"
  (let* ((keymap-name (intern (concat name "-keymap")))
         (keymap-alias (intern name))
         (keymap-bindings (mapcar
                           (lambda (binding)
                             (let ((binding (eval binding)))
                               `(define-key ,keymap-name (kbd ,(car binding)) #',(cdr binding))))
                           bindings)))
    `(progn
       (defvar ,keymap-name (make-sparse-keymap))
       (defalias ',keymap-alias ,keymap-name)
       (global-set-key (kbd ,prefix) ',keymap-alias)
       ,@keymap-bindings)))

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
(defun my-active-modes ()
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

(defun my-open-text-buffer ()
  "Open the *text*  buffer, (re)creating it if not present."
  (interactive)
  (if (get-buffer "*text*")
      (switch-to-buffer "*text*")
    (progn
      (switch-to-buffer (get-buffer-create "*text*"))
      (fundamental-mode))))

(defun my-open-canvas-buffer ()
  "Open the *canvas*  (doodling) buffer, (re)creating it if not present."
  ;; TODO: merge these my-open... functions into one?
  (interactive)
  (if (get-buffer "*canvas*")
      (switch-to-buffer "*canvas*")
    (progn
      (switch-to-buffer (get-buffer-create "*canvas*"))
      (canvas-mode))))

(defun my-get-shell-buffer-name ()
  "Return either either the project name or filename if outside of a project."
  ;; if a fileless buffer don't even consult projectile
  (if (not (buffer-file-name))
      (setq shell-buffer-name-local-segment (buffer-name))
    (progn
      (if (> (length (projectile-project-name)) 1)
          (setq shell-buffer-name-local-segment (projectile-project-name))
        ;; if projectile doesn't find a project we go by our project dir structure
        (progn
          (if (member "git" (split-string buffer-file-name "/"))
              (setq shell-buffer-name-local-segment (nth 1 (member "git" (split-string buffer-file-name "/") )))
            (setq shell-buffer-name-local-segment (first (last (split-string buffer-file-name "/") 2))))))))
  (setq shell-buffer-name (concat "*shell*<" shell-buffer-name-local-segment ">"))
  )

(defun my-open-default-shell ()
  "Open or switch to a shell dedicated to the current project or file (if outside of a project)."
  (interactive)
  ;; TODO: only use default-directory if we're in a buffer visiting a file?
  (setq current-buffer-directory default-directory)
  (setq current-open-and-visible-frames (length (cl-delete-duplicates (mapcar #'window-buffer (window-list)))))
  (setq shell-buffer-name (my-get-shell-buffer-name))
  ;; does a shell for this project already exist?
  (if-let (shell-buffer (get-buffer shell-buffer-name))
      ;; is its frame visible?
      (if (eq shell-buffer (buffer-name (window-buffer (selected-window))))
          (select-window (get-buffer-window shell-buffer))
        (progn
          ;; if only one window is open on-screen then split vertically
          (if (<= current-open-and-visible-frames 1) (split-window-right))
          (other-window 1)
          (switch-to-buffer shell-buffer-name)))
    (progn
      ;; if only one window is open on-screen then split vertically and move focus to it
      (if (<= current-open-and-visible-frames 1)
          (select-window  (split-window-right))
        (other-window 1))
      (setq default-directory current-buffer-directory)
      (shell shell-buffer-name)
      (rename-buffer shell-buffer-name)))
  )

(defun my-forward-down-list ()
  "Jumps to the end delimeter of the actibe block."
  (interactive)
  (backward-up-list)
  (forward-sexp)
  )

(defun my-stop-debugging-mode ()
  (interactive)
  (dap-delete-all-sessions)
  (dap-mode 0)
  (dap-ui-mode 0)
  (dap-ui-controls-mode 0)
  (delete-other-windows) ;; hide all the dap UI. I might want to delete the buffers as well.
  )

(defun my-get-ppi ()
  "Get display PPI.  Do not run this function in non-graphic mode."
  (setq my-disp-attrs (car (display-monitor-attributes-list)))

  (setq my-mm-size (assq 'mm-size my-disp-attrs))
  (setq my-mm-width (nth 1 my-mm-size))
  (setq my-mm-height (nth 2 my-mm-size))

  (setq my-diag-mm (sqrt (+
                          (expt my-mm-width 2)
                          (expt my-mm-height 2))))

  (setq my-diag-inches (* my-diag-mm 0.0393701))

  (setq my-geom (assq 'geometry my-disp-attrs))
  (setq my-pixel-width (nth 3 my-geom))
  (setq my-pixel-height (nth 4 my-geom))

  (setq my-diag-pixels (sqrt (+
                              (expt my-pixel-width 2)
                              (expt my-pixel-height 2))))

  (/ my-diag-pixels my-diag-inches))

(defmacro when-on-windows (&rest body)
  `(when (equal system-type 'windows-nt)
     ,@body))

(defmacro unless-on-windows (&rest body)
  `(unless (equal system-type 'windows-nt)
     ,@body))

(defun append-env (env new-item)
  (setenv env (concat (getenv env) ":"
                      new-item)))

(defun path-as-list ()
  (split-string (getenv "PATH") ":"))

(defun apply-path-list (list)
  (setenv "PATH" (string-join list ":")))


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
(fset 'yes-or-no-p 'y-or-n-p)
 ;; silence bell, see: https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Beep-sound.html
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
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq confirm-nonexistent-file-or-buffer nil)
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
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq transient-mark-mode t)
(setq frame-title-format
      (format "emacs@%s: %%f" (system-name))) ;; Show file name in title bar
(setq eval-expression-print-length 999) ;; do not truncate longer eval results in messages buffer
;; ;; use window decoration
;; (set-frame-parameter nil 'undecorated nil)
;; ;; resize window to full screen dimensions
;; (setq initial-frame-alist '( (fullscreen . maximized)))
;; (setq default-frame-alist '( (fullscreen . fullheight)))
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))
