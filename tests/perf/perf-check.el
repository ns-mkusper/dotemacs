;;; perf-check.el --- Timing budgets for the fully loaded config  -*- lexical-binding: t; -*-

;; Usage, after the config is installed in `user-emacs-directory':
;;
;;   emacs --batch -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el \
;;         -l tests/perf/perf-check.el </dev/null
;;
;; Each check times one everyday operation against a budget in seconds.
;; Budgets sit a few times above current timings on a CI runner, so noise
;; passes but the 10x-and-worse regressions these guard against fail:
;; init scanning a network-mounted org-roam directory, pet listing every
;; file of a project on each Python buffer, and slow JSON buffers.
;;
;; PERF_SCALE multiplies every budget (e.g. 2 on a slow machine).
;; PERF_OUT, if set, receives the results as JSON.

(require 'json)
(require 'seq)

(defvar perf-startup-seconds (float-time (time-subtract nil before-init-time))
  "Seconds from Emacs start until this file loaded, i.e. the whole init.")

(defvar perf-scale (string-to-number (or (getenv "PERF_SCALE") "1")))
(defvar perf-results nil)
(defvar perf-failed nil)

(defun perf-record (name seconds budget)
  "Record check NAME that took SECONDS against BUDGET (scaled)."
  (let* ((limit (* budget perf-scale))
         (ok (<= seconds limit)))
    (unless ok (setq perf-failed t))
    (push `((name . ,name) (seconds . ,seconds) (budget . ,limit) (ok . ,(if ok t :json-false)))
          perf-results)
    (princ (format "%-4s %-42s %7.3fs  (budget %.1fs)\n" (if ok "ok" "FAIL") name seconds limit))))

(defun perf-assert (name ok &optional detail)
  "Record a pass/fail check NAME that does not measure time."
  (unless ok (setq perf-failed t))
  (push `((name . ,name) (ok . ,(if ok t :json-false))) perf-results)
  (princ (format "%-4s %-42s %s\n" (if ok "ok" "FAIL") name (or detail ""))))

(defmacro perf-time (&rest body)
  "Seconds taken by BODY."
  `(let ((t0 (float-time))) ,@body (- (float-time) t0)))

(defun perf-open (file)
  "Visit FILE as a user would, run one command cycle, then kill the buffer.
Return seconds taken.  The command cycle matters: Eglot and Flycheck
defer work to the first `post-command-hook' in a new buffer."
  (perf-time
   (let ((buf (find-file-noselect file)))
     (with-current-buffer buf
       (condition-case err (run-hooks 'post-command-hook)
         (error (message "post-command-hook error in %s: %S" file err))))
     (let ((kill-buffer-query-functions nil))
       (kill-buffer buf)))))

;;;; Fixtures

(defvar perf-dir (file-name-as-directory (make-temp-file "dotemacs-perf" t)))

(defun perf-write (file content)
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert content)))

;; JSON: an empty file and a ~1 MB one.
(perf-write (expand-file-name "json/empty.json" perf-dir) "")
(perf-write (expand-file-name "json/large.json" perf-dir)
            (concat "[\n"
                    (mapconcat (lambda (i) (format "  {\"id\": %d, \"name\": \"item-%d\", \"tags\": [\"a\", \"b\"], \"value\": %d.5}" i i i))
                               (number-sequence 1 15000) ",\n")
                    "\n]\n"))

;; A project that holds its own venv: 15,000 library files under lib/,
;; a pyrightconfig.json marking the root, and a few source files.
(defvar perf-venv (expand-file-name "venv-project/" perf-dir))
(perf-write (expand-file-name "pyvenv.cfg" perf-venv) "home = /usr/bin\nversion_info = 3.12.0\n")
(perf-write (expand-file-name "pyrightconfig.json" perf-venv)
            "{\"venvPath\": \".\", \"venv\": \".\", \"exclude\": [\"bin\", \"lib\"]}\n")
(perf-write (expand-file-name "src/app/main.py" perf-venv)
            "import json\n\n\ndef main():\n    return json.dumps({\"ok\": True})\n")
(dotimes (p 150)
  (dotimes (m 100)
    (perf-write (expand-file-name (format "lib/python3.12/site-packages/pkg%03d/mod%03d.py" p m) perf-venv)
                (format "def f%d():\n    return %d\n" m m))))

;;;; Checks

(princ (format "perf-check on %s, budget scale %s\n" (emacs-version) perf-scale))

(perf-record "startup (full init)" perf-startup-seconds 5)
(perf-assert "org-roam not loaded at startup" (not (featurep 'org-roam)))

(perf-record "open empty .json" (perf-open (expand-file-name "json/empty.json" perf-dir)) 1.5)
(perf-record "open 1 MB .json" (perf-open (expand-file-name "json/large.json" perf-dir)) 3)
(perf-record "reopen empty .json" (perf-open (expand-file-name "json/empty.json" perf-dir)) 1)

(perf-record "open .py in project with 15000-file venv"
             (perf-open (expand-file-name "src/app/main.py" perf-venv)) 1)
(perf-record "open .py inside that venv (M-. target)"
             (perf-open (expand-file-name "lib/python3.12/site-packages/pkg007/mod042.py" perf-venv)) 1)

(when-let* ((out (getenv "PERF_OUT")))
  (with-temp-file out (insert (json-encode (nreverse perf-results)))))

(delete-directory perf-dir t)
(princ (if perf-failed "perf-check: FAILED\n" "perf-check: all within budget\n"))
(kill-emacs (if perf-failed 1 0))

;;; perf-check.el ends here
