(use-package project
  :straight t
  :defer t
  :preface
  (defvar-local my-proj/cache nil
    "Project cache.")

  (defun my-proj/locate-root (&optional buffer)
    "Locate root of project for BUFFER.
Optional BUFFER is a buffer or string, respectively the buffer or
name of the buffer to locate the root for and defaults to the
current buffer.

This function is expensive.  For a potentially cheap alternative,
see `my-proj/get-root'."
    (let ((file-name
           (buffer-local-value
            'default-directory (get-buffer (or buffer (current-buffer))))))
      (when-let ((project (project-current nil file-name)))
        (project-root project))))

  (defun my-proj/determine-name (file-name)
    "Determine name of project for project FILE-NAME."
    (file-name-nondirectory (directory-file-name file-name)))

  (defun my-proj/init ()
    "Initialize project cache of current."
    (unless my-proj/cache
      (setq my-proj/cache
            (if-let ((file-name (my-proj/locate-root)))
                (list :root file-name
                      :name (my-proj/determine-name file-name))
              'no-project))))

  (defun my-proj/get-root ()
    "Return cached root of project for current.
The first call in a buffer is expensive.  Subsequent calls in the
same buffer are cheap but its results incorrect when the buffer
is no longer part of the initial project.  For a correct
alternative, see `my-proj/locate-root'."
    (my-proj/init)
    (plist-get my-proj/cache :root))

  (defun my-proj/get-name ()
    "Return cached name of project for current.
The first call in a buffer is expensive.  Subsequent calls in the
same buffer are cheap but its results incorrect when the buffer
is no longer part of the initial project."
    (my-proj/init)
    (plist-get my-proj/cache :name))

  (defun my-proj/locate-configs (name &optional buffer prompt)
    "Locate configuration files of project for BUFFER.
Some projects contain configuration files (e.g. Makefile,
project.clj and package.json), one in the root directory, the
primary configuration file, and/or one or more in the child
directories of the root directory, the secondary configuration
files.

NAME is a string, the name of the configuration file.  Optional
BUFFER is a buffer or string, respectively the buffer or name of
the buffer to locate the configuration files for and defaults to
the current buffer.

When optional PROMPT is truthy and the project only contains
secondary configuration files, prompt for one.  The selected
configuration file is sorted before the other ones.

When the project contains both a primary configuration file and
secondary configuration files, the primary one is sorted before
the secondary ones."
    (let* ((root (or (my-proj/locate-root buffer) (error "Not in a project")))
           (primary (let ((file-name (expand-file-name name root)))
                      (when (file-exists-p file-name)
                        (list file-name))))
           (secondary (file-expand-wildcards (thread-last root
                                               (expand-file-name "*/")
                                               (expand-file-name name)))))
      (if (and (null primary) (> (length secondary) 1) prompt)
          (let* ((candidates (mapcar (lambda (file-name)
                                       (cons (file-relative-name file-name root)
                                             file-name))
                                     secondary))
                 (file-name (thread-first
                                (completing-read "Project configuration: "
                                                 candidates nil t)
                              (assoc candidates)
                              cdr)))
            (cons file-name (remove file-name secondary)))
        (append primary secondary))))

  (define-advice project-remember-project
      (:before-while (project) my-proj/exclude-uninteresting)
    (if (bound-and-true-p recentf-mode)
        (recentf-include-p (project-root project))
      t))

  (defun my-proj/magit-status ()
    "Launch magit in current project.
When not in a project, prompt for one."
    (interactive)
    (let ((default-directory            ; Dynamic variable
            (project-root (project-current 'maybe-prompt))))
      (magit-status-setup-buffer)))

  (defun my-proj/vterm (&optional arg)
    "Launch or switch to a vterm session in current project.
With numeric prefix argument ARG, launch or switch to a numbered
vterm session.  With `\\[universal-argument]' prefix argument
ARG, launch a new vterm session.  When not in a project, prompt
for one."
    (interactive "P")
    ;; Dynamic variables
    (defvar vterm-buffer-name)
    (let* ((default-directory (project-root (project-current 'maybe-prompt)))
           (vterm-buffer-name (project-prefixed-buffer-name "vterm")))
      (vterm arg)))

  ;; `project-eshell' doesn't launch or switch to numbered eshell
  ;; sessions
  (defun my-proj/eshell (&optional arg)
    "Launch or switch to an eshell session in current project.
With numeric prefix argument ARG, launch or switch to a numbered
eshell session.  With `\\[universal-argument]' prefix argument
ARG, launch a new eshell session.  When not in a project, prompt
for one."
    (interactive "P")
    ;; Dynamic variables
    (defvar eshell-buffer-name)
    (let* ((default-directory (project-root (project-current 'maybe-prompt)))
           (eshell-buffer-name (project-prefixed-buffer-name "eshell")))
      (eshell arg)))

  (defun my-proj/shell ()
    "Open or switch to a shell dedicated to the current project or file (if outside of a project)."
    (interactive)
    ;; Dynamic variables
    (defvar shell-buffer-name)
    (let* ((default-directory (project-root (project-current 'maybe-prompt)))
           (shell-buffer-name (project-prefixed-buffer-name "shell")))
      (if-let (shell-buffer (get-buffer shell-buffer-name))
          ;; is its frame visible?
          (if (eq shell-buffer-name (buffer-name (window-buffer (selected-window))))
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
      ))
  :config
  (bind-keys
   :map project-prefix-map
   ("m" . my-proj/magit-status)
   ("t" . my-proj/vterm)
   ("e" . my-proj/eshell))

  (setq project-compilation-buffer-name-function #'project-prefixed-buffer-name)
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-dired "Dired")
          (project-find-dir "Find directory")
          (project-switch-to-buffer "Buffer")
          (my-proj/magit-status "Magit")
          (my-proj/vterm "Vterm")
          (my-proj/eshell "Eshell"))))

(provides '30-project)
