;;; elisp-syntax-check.el --- Parse Emacs Lisp files for syntax errors  -*- lexical-binding: t; -*-

(let ((failed nil))
  (dolist (file command-line-args-left)
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
       (princ (format "FAIL %s :: %S\n" file err))))))
  (when failed
    (kill-emacs 1)))
