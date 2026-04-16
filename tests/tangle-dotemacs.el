;;; tangle-dotemacs.el --- Tangle literate config and fail on drift -*- lexical-binding: t; -*-

(require 'org)
(require 'ob-tangle)
(require 'subr-x)

(let* ((repo-root (file-name-directory (directory-file-name default-directory)))
       (org-file (expand-file-name "dotemacs.org" repo-root)))
  (unless (file-exists-p org-file)
    (error "Missing literate config: %s" org-file))
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle-file org-file))
  (let* ((default-directory repo-root)
         (status (shell-command-to-string
                  "git status --porcelain -- early-init.el init.el my-custom-vars.el emacs.bat inits/*.el")))
    (unless (string-empty-p status)
      (princ status)
      (error "Tangling changed generated files. Run ./tests/tangle-config.sh and commit results"))))
