;;; tramp-benchmark.el --- Repeatable TRAMP timing test  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'tramp)

(defun tramp-test-default-target ()
  "Build a default TRAMP target from env vars.
Defaults to an SSH host alias so no local IP is committed."
  (let* ((method (or (getenv "TRAMP_TEST_METHOD") "ssh"))
         (user (getenv "TRAMP_TEST_USER"))
         (host (or (getenv "TRAMP_TEST_HOST") "tramp-test"))
         (port (getenv "TRAMP_TEST_PORT"))
         (path (or (getenv "TRAMP_TEST_PATH") "/tmp/"))
         (user-part (if (and user (not (string-empty-p user)))
                        (concat user "@")
                      ""))
         (port-part (if (and port (not (string-empty-p port)))
                        (concat "#" port)
                      "")))
    (format "/%s:%s%s%s:%s" method user-part host port-part path)))

(defvar tramp-test-target
  (or (getenv "TRAMP_TEST_TARGET") (tramp-test-default-target)))
(defvar tramp-test-cycles
  (string-to-number (or (getenv "TRAMP_TEST_CYCLES") "4")))
(defvar tramp-test-profile 'optimized)
(defvar tramp-test-verbose-level
  (string-to-number (or (getenv "TRAMP_TEST_VERBOSE") "10")))
(defvar tramp-test-output-dir (or (getenv "TRAMP_TEST_OUTPUT_DIR") "/bench/out"))
(defvar tramp-test-debug-dir nil)
(defun tramp-test-remote-file (remote-dir local-file)
  "Build a TRAMP path on REMOTE-DIR host that points to LOCAL-FILE."
  (with-parsed-tramp-file-name remote-dir v
    (tramp-make-tramp-file-name
     v-method v-user v-domain v-host v-port local-file v-hop)))

(defun tramp-test-setup-profile (profile)
  "Apply PROFILE settings for a benchmark run."
  (setq tramp-verbose tramp-test-verbose-level)
  (setq tramp-debug-to-file t)
  (setq tramp-debug-command-messages nil)
  (setq tramp-test-debug-dir
        (expand-file-name (format "%s/debug" profile) tramp-test-output-dir))
  (make-directory tramp-test-debug-dir t)
  ;; Place TRAMP's internal debug files under our exported artifact directory.
  (when (boundp 'tramp-compat-temporary-file-directory)
    (setq tramp-compat-temporary-file-directory tramp-test-debug-dir))
  (pcase profile
    ('current
     (setq remote-file-name-inhibit-locks t)
     (setq tramp-use-scp-direct-remote-copying t)
     (setq remote-file-name-inhibit-auto-save-visited t)
     ;; This setting can speed up some systems but causes hangs for others.
     (setq tramp-direct-async-process t))
    ('optimized
     (setq remote-file-name-inhibit-locks t)
     (setq tramp-use-scp-direct-remote-copying t)
     (setq remote-file-name-inhibit-auto-save-visited t)
     (setq tramp-direct-async-process nil)
     (setq tramp-use-connection-share t)
     (setq tramp-ssh-controlmaster-options
           "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=600")
     (setq tramp-completion-reread-directory-timeout nil))))

(defun tramp-test-time-op (fn)
  "Run FN and return elapsed seconds as float."
  (let ((start (float-time)))
    (funcall fn)
    (- (float-time) start)))

(defun tramp-test-run-once (remote-dir)
  "Run one full benchmark round on REMOTE-DIR."
  (tramp-cleanup-all-connections)
  (tramp-cleanup-all-buffers)
  (let* ((remote-file (concat remote-dir "hostname-copy"))
         (remote-hostname-file (tramp-test-remote-file remote-dir "/etc/hostname"))
         (t1 (tramp-test-time-op
              (lambda ()
                (file-directory-p remote-dir))))
         (t2 (tramp-test-time-op
              (lambda ()
                (directory-files remote-dir nil "^[^.].*"))))
         (t3 (tramp-test-time-op
              (lambda ()
                (with-temp-buffer
                  (insert-file-contents remote-hostname-file)
                  (write-region (point-min) (point-max) remote-file nil 'silent)))))
         (t4 (tramp-test-time-op
              (lambda ()
                (file-attributes remote-file)))))
    (list t1 t2 t3 t4)))

(defun tramp-test--remote-log-buffers ()
  "Return current TRAMP log buffers."
  (cl-remove-if-not
   (lambda (buf)
     (string-prefix-p "*tramp/" (buffer-name buf)))
   (buffer-list)))

(defun tramp-test-write-cycle-logs (profile cycle)
  "Write all current TRAMP logs for PROFILE and CYCLE."
  (let* ((dir (expand-file-name (format "%s/logs" profile) tramp-test-output-dir))
         (out (expand-file-name (format "cycle-%02d.log" cycle) dir)))
    (make-directory dir t)
    (with-temp-file out
      (insert (format "profile=%s cycle=%d verbose=%d\n\n"
                      profile cycle tramp-test-verbose-level))
      (dolist (buf (tramp-test--remote-log-buffers))
        (insert (format "=== %s ===\n" (buffer-name buf)))
        (insert (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max))))
        (unless (bolp) (insert "\n"))
        (insert "\n")))
    ))

(defun tramp-test-average (rows)
  "Return average vector for ROWS."
  (cl-loop for i below 4
           collect (/ (cl-loop for row in rows sum (nth i row))
                      (float (length rows)))))

(defun tramp-test-write-summary (profile rows avg)
  "Write machine-readable and human-readable summaries for PROFILE."
  (let* ((dir (expand-file-name tramp-test-output-dir))
         (txt (expand-file-name (format "%s-summary.txt" profile) dir))
         (sexp-file (expand-file-name (format "%s-summary.sexp" profile) dir))
         (payload `(:profile ,(symbol-name profile)
                   :target ,tramp-test-target
                   :cycles ,tramp-test-cycles
                   :verbose ,tramp-test-verbose-level
                   :avg-connect ,(nth 0 avg)
                   :avg-list ,(nth 1 avg)
                   :avg-copy ,(nth 2 avg)
                   :avg-stat ,(nth 3 avg)
                   :raw ,rows)))
    (make-directory dir t)
    (with-temp-file txt
      (insert (format "PROFILE=%s\n" profile))
      (insert (format "TARGET=%s\n" tramp-test-target))
      (insert (format "CYCLES=%d\n" tramp-test-cycles))
      (insert (format "VERBOSE=%d\n" tramp-test-verbose-level))
      (insert (format "AVG_CONNECT=%.4fs\n" (nth 0 avg)))
      (insert (format "AVG_LIST=%.4fs\n" (nth 1 avg)))
      (insert (format "AVG_COPY=%.4fs\n" (nth 2 avg)))
      (insert (format "AVG_STAT=%.4fs\n" (nth 3 avg)))
      (insert (format "RAW=%S\n" rows)))
    (with-temp-file sexp-file
      (prin1 payload (current-buffer))
      (insert "\n"))))

(defun tramp-test-main ()
  (let ((profile-env (getenv "TRAMP_TEST_PROFILE")))
    (when (and profile-env (not (string-empty-p profile-env)))
      (setq tramp-test-profile (intern profile-env))))
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (tramp-test-setup-profile tramp-test-profile)
  (let ((rows nil))
    (dotimes (i tramp-test-cycles)
      (push (tramp-test-run-once tramp-test-target) rows)
      (tramp-test-write-cycle-logs tramp-test-profile (1+ i)))
    (setq rows (nreverse rows))
    (let ((avg (tramp-test-average rows)))
      (tramp-test-write-summary tramp-test-profile rows avg)
      (princ (format "PROFILE=%s\n" tramp-test-profile))
      (princ (format "TARGET=%s\n" tramp-test-target))
      (princ (format "CYCLES=%d\n" tramp-test-cycles))
      (princ (format "VERBOSE=%d\n" tramp-test-verbose-level))
      (princ (format "AVG_CONNECT=%.4fs\n" (nth 0 avg)))
      (princ (format "AVG_LIST=%.4fs\n" (nth 1 avg)))
      (princ (format "AVG_COPY=%.4fs\n" (nth 2 avg)))
      (princ (format "AVG_STAT=%.4fs\n" (nth 3 avg)))
      (princ (format "RAW=%S\n" rows)))))

(tramp-test-main)
