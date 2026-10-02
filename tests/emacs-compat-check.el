;;; emacs-compat-check.el --- Check tangled config against the running Emacs  -*- lexical-binding: t; -*-

;; Usage:
;;   emacs --batch -Q -l tests/emacs-compat-check.el -- FILE...
;;
;; Reads every top-level form in FILE... and reports, for the Emacs that is
;; running the check:
;;
;; 1. Functions and variables the config references that this Emacs marks
;;    obsolete (`make-obsolete' / `make-obsolete-variable').
;; 2. `use-package' blocks declared as built-in (`:straight nil` or
;;    `:straight (:type built-in)`) whose library this Emacs does not ship.
;;
;; No third-party packages are loaded, so the check runs offline in a bare
;; Emacs.  Built-in libraries the config touches are loaded first so their
;; obsolescence markers are visible.

(require 'seq)

(defvar compat-check-preload-libraries
  '(cl-lib subr-x treesit eglot project flymake which-key editorconfig
    hideshow flyspell comint dired dired-x tramp vc xref eldoc recentf
    savehist saveplace uniquify display-line-numbers whitespace winner
    windmove tab-bar pixel-scroll repeat time ansi-color shell eshell org
    ob-tangle package use-package compile ediff erc ibuffer imenu outline
    simple files minibuffer completion-preview)
  "Built-in libraries to load so their obsolete markers are available.")

(defvar compat-check-ignored-symbols nil
  "Symbols never reported.  Add an entry only with a comment saying why.")

(defvar compat-check--fn-syms nil
  "Symbols seen in function position while walking one file.")
(defvar compat-check--other-syms nil
  "Symbols seen anywhere else while walking one file.")
(defvar compat-check--built-ins nil
  "use-package names declared built-in while walking one file.")

(defun compat-check--use-package-active-p (plist)
  "Return non-nil unless a `:if', `:when' or `:unless' keyword in PLIST
rules the block out for this Emacs.  Guards are evaluated; one that
signals an error counts as active so the block is still checked."
  (let ((active t))
    (while plist
      (let ((key (car plist)) (guard (cadr plist)))
        (when (memq key '(:if :when :unless))
          (let ((value (condition-case nil (eval guard t) (error 'compat-error))))
            (unless (eq value 'compat-error)
              (when (if (eq key :unless) value (not value))
                (setq active nil))))))
      (setq plist (cdr plist)))
    active))

(defun compat-check--note-built-in (form)
  "If FORM is a use-package declared built-in, remember its name.
Blocks whose `:if'/`:when'/`:unless' guard excludes this Emacs are skipped."
  (when (and (eq (car form) 'use-package)
             (symbolp (cadr form))
             (compat-check--use-package-active-p (cddr form)))
    (let ((plist (cddr form)))
      (while plist
        (when (and (eq (car plist) :straight)
                   (or (null (cadr plist))
                       (and (consp (cadr plist))
                            (eq (plist-get (cadr plist) :type) 'built-in))))
          (puthash (cadr form) t compat-check--built-ins))
        (setq plist (cdr plist))))))

(defun compat-check--walk (form &optional fn-pos)
  "Record every symbol in FORM.  FN-POS means FORM is in call position."
  (cond
   ((symbolp form)
    (when form
      (puthash form t (if fn-pos compat-check--fn-syms compat-check--other-syms))))
   ((consp form)
    (compat-check--note-built-in form)
    (compat-check--walk (car form) (not (memq (car form) '(quote function))))
    (let ((rest (cdr form)))
      (while (consp rest)
        (compat-check--walk (car rest))
        (setq rest (cdr rest)))
      (when rest (compat-check--walk rest))))
   ((vectorp form)
    (mapc #'compat-check--walk form))))

(defun compat-check--read-forms (file)
  "Return all top-level forms in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((forms nil))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun compat-check--replacement (info)
  "Format the replacement hint from obsolescence INFO."
  (let ((hint (car info)))
    (cond ((null hint) "")
          ((stringp hint) (format "; %s" hint))
          (t (format "; use `%s'" hint)))))

(defun compat-check-file (file)
  "Return a list of problem strings for FILE."
  (let ((compat-check--fn-syms (make-hash-table :test #'eq))
        (compat-check--other-syms (make-hash-table :test #'eq))
        (compat-check--built-ins (make-hash-table :test #'eq))
        (problems nil))
    (dolist (form (compat-check--read-forms file))
      (compat-check--walk form))
    ;; Obsolete functions: any occurrence counts, since hooks and
    ;; `add-hook'/`define-key' take quoted function symbols.
    (dolist (table (list compat-check--fn-syms compat-check--other-syms))
      (maphash
       (lambda (sym _)
         (let ((info (get sym 'byte-obsolete-info)))
           (when (and info (not (memq sym compat-check-ignored-symbols)))
             (push (format "obsolete function `%s' (since %s%s)"
                           sym (or (nth 2 info) "?") (compat-check--replacement info))
                   problems))))
       table))
    ;; Obsolete variables: only when the symbol appears outside call
    ;; position, and is not also a face (Emacs 31 obsoletes the
    ;; `font-lock-*-face' variables but the faces of the same name stay).
    (maphash
     (lambda (sym _)
       (let ((info (get sym 'byte-obsolete-variable)))
         (when (and info
                    (not (facep sym))
                    (not (memq sym compat-check-ignored-symbols)))
           (push (format "obsolete variable `%s' (since %s%s)"
                         sym (or (nth 2 info) "?") (compat-check--replacement info))
                 problems))))
     compat-check--other-syms)
    (maphash
     (lambda (pkg _)
       (unless (or (featurep pkg) (locate-library (symbol-name pkg)))
         (push (format "use-package `%s' declared built-in but Emacs %s does not ship it"
                       pkg emacs-version)
               problems)))
     compat-check--built-ins)
    (seq-uniq (sort problems #'string<))))

(defun compat-check-main ()
  "Entry point for batch use."
  (dolist (lib compat-check-preload-libraries)
    (require lib nil t))
  (let ((files (seq-remove (lambda (f) (string= f "--")) command-line-args-left))
        (total 0))
    (princ (format "Emacs %s (%s)\n" emacs-version system-configuration))
    (dolist (file files)
      (let ((problems (compat-check-file file)))
        (if (null problems)
            (princ (format "OK   %s\n" file))
          (princ (format "FAIL %s\n" file))
          (dolist (p problems)
            (princ (format "       %s\n" p)))
          (setq total (+ total (length problems))))))
    (if (zerop total)
        (princ "Compatibility check passed.\n")
      (princ (format "%d compatibility problem(s) found.\n" total))
      (kill-emacs 1))))

(compat-check-main)

;;; emacs-compat-check.el ends here
