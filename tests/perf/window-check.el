;;; window-check.el --- Compilation pop-up sizing in a real frame  -*- lexical-binding: t; -*-

;; Usage: run inside a fixed-size terminal (run-perf.sh uses `script' with
;; 60 rows), after the full config:
;;
;;   emacs -nw -q -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el \
;;         -l tests/perf/window-check.el
;;
;; Batch Emacs has no usable frame, so window layout needs a terminal (or GUI)
;; frame.  Results go to WINDOW_CHECK_OUT; the exit code is 0 when all pass.
;;
;; Guards against compilation output (builds, tests, C-c d runs) taking most
;; of the frame: it should open at about 30%, keep focus in the source, and
;; shrink to short output once finished without growing past its cap.

(defvar window-check-out (or (getenv "WINDOW_CHECK_OUT") "/dev/stdout"))
(defvar window-check-failed nil)

(defun window-check-say (fmt &rest args)
  (append-to-file (concat (apply #'format fmt args) "\n") nil window-check-out))

(defun window-check (name ok detail)
  (unless ok (setq window-check-failed t))
  (window-check-say "%-4s %-42s %s" (if ok "ok" "FAIL") name detail))

(defun window-check--height ()
  (when-let* ((w (get-buffer-window "*compilation*")))
    (window-total-height w)))

(defun window-check--wait ()
  (while (get-buffer-process "*compilation*")
    (accept-process-output nil 0.1))
  (sit-for 0.5))

(defun window-check--run (label command)
  "Compile COMMAND from a lone source window; return (OPEN-HEIGHT . DONE-HEIGHT)."
  (delete-other-windows)
  (let ((source (selected-window))
        (default-directory temporary-file-directory)
        (compilation-ask-about-save nil))
    (compile command)
    (redisplay t)
    (let ((open (window-check--height)))
      (window-check (format "%s: focus stays in source" label)
                    (eq (selected-window) source)
                    (buffer-name (window-buffer (selected-window))))
      (window-check--wait)
      (prog1 (cons open (window-check--height))
        (when (fboundp 'popwin:close-popup-window) (popwin:close-popup-window))
        (delete-other-windows)))))

(defun window-check-main ()
  (condition-case err
      (let* ((frame-lines (frame-height))
             (cap (ceiling (* 0.35 frame-lines))))
        (window-check-say "frame %dx%d, cap %d lines" (frame-width) frame-lines cap)
        (find-file (expand-file-name "window-check-source.txt" temporary-file-directory))
        (let ((short (window-check--run "short output" "echo one line")))
          (window-check "short output: opens at <= 35% of frame"
                        (and (car short) (<= (car short) cap)) (format "%S lines" (car short)))
          (window-check "short output: shrinks when finished"
                        (and (cdr short) (<= (cdr short) 12)) (format "%S lines" (cdr short))))
        (let ((long (window-check--run "long output" "seq 1 400")))
          (window-check "long output: opens at <= 35% of frame"
                        (and (car long) (<= (car long) cap)) (format "%S lines" (car long)))
          (window-check "long output: stays capped when finished"
                        (and (cdr long) (<= (cdr long) cap)) (format "%S lines" (cdr long)))))
    (error (window-check "window check ran" nil (format "%S" err))))
  (kill-emacs (if window-check-failed 1 0)))

(add-hook 'emacs-startup-hook (lambda () (run-at-time 1 nil #'window-check-main)))

;;; window-check.el ends here
