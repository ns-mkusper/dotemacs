;;; elisp-syntax-check.el --- Parse Emacs Lisp files for syntax errors  -*- lexical-binding: t; -*-

(require 'seq)

(let ((failed nil)
      (files (seq-remove (lambda (f) (string= f "--")) command-line-args-left)))
  (dolist (file files)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while t
            (read (current-buffer))))
      (end-of-file
       (princ (format "OK %s\n" file)))
      (error
       (setq failed t)
       (princ (format "FAIL %s :: %S\n" file err)))))
  (when failed
    (kill-emacs 1)))
