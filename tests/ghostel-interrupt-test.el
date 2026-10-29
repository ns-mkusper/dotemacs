;;; ghostel-interrupt-test.el --- C-c reaches programs in a ghostel shell -*- lexical-binding: t -*-

;; Loads the tangled inits/60-ghostel.el (so the shell and environment
;; are the ones the config ships), opens a ghostel terminal, starts a
;; long-running program, presses C-c C-c and checks that the program
;; stops while the shell keeps running.  On Windows this is the case
;; fakecygpty used to be needed for; it also interrupts a native console
;; program (ping.exe) there.
;;
;; Usage:
;;   GHOSTEL_DIR=/path/to/ghostel TANGLE_DIR=/path/to/tangled \
;;     emacs -Q --batch -l tests/ghostel-interrupt-test.el

(require 'cl-lib)
(require 'use-package)

(defvar my-test-ghostel-dir (or (getenv "GHOSTEL_DIR") (error "GHOSTEL_DIR not set")))
(defvar my-test-tangle-dir (or (getenv "TANGLE_DIR") (error "TANGLE_DIR not set")))
(defvar my-test-timeout 20 "Seconds to wait for each piece of expected output.")

;; The config installs ghostel with straight; here it is already on disk,
;; so accept `:straight' and ignore it.
(add-to-list 'use-package-keywords :straight)
(defun use-package-normalize/:straight (_name _keyword args) args)
(defun use-package-handler/:straight (name _keyword _args rest state)
  (use-package-process-keywords name rest state))

(setq user-emacs-directory (file-name-as-directory (make-temp-file "ghostel-ued-" t)))
(add-to-list 'load-path (expand-file-name "lisp" my-test-ghostel-dir))
(load (expand-file-name "inits/60-ghostel.el" my-test-tangle-dir) nil t)
(require 'ghostel)

(message "system-type: %s" system-type)
(message "ghostel-shell: %S" ghostel-shell)
(message "ghostel-environment: %S" ghostel-environment)

(defvar my-test-buffer nil)

(defun my-test-buffer-text ()
  (with-current-buffer my-test-buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun my-test-fail (fmt &rest args)
  (message "---- terminal contents ----\n%s\n---------------------------"
           (if (buffer-live-p my-test-buffer) (my-test-buffer-text) "<buffer killed>"))
  (when (buffer-live-p my-test-buffer)
    (let ((proc (buffer-local-value 'ghostel--process my-test-buffer)))
      (message "shell process: %S status: %S exit: %S"
               (and proc (process-command proc))
               (and proc (process-status proc))
               (and proc (process-exit-status proc)))))
  (unless (eq system-type 'windows-nt)
    (message "---- processes ----\n%s"
             (shell-command-to-string "ps -eo pid,ppid,stat,tty,args --forest | tail -20")))
  (message "FAIL: %s" (apply #'format fmt args))
  (kill-emacs 1))

(defun my-test-wait-for (regexp what)
  "Wait until REGEXP shows up in the terminal, or fail describing WHAT."
  (let ((deadline (+ (float-time) my-test-timeout)))
    (while (and (< (float-time) deadline)
                (buffer-live-p my-test-buffer)
                (not (string-match-p regexp (my-test-buffer-text))))
      (accept-process-output nil 0.2))
    (unless (and (buffer-live-p my-test-buffer)
                 (string-match-p regexp (my-test-buffer-text)))
      (my-test-fail "timed out waiting for %s" what))))

(defun my-test-pause (seconds)
  (let ((deadline (+ (float-time) seconds)))
    (while (< (float-time) deadline)
      (accept-process-output nil 0.2))))

(defun my-test-type (line)
  (with-current-buffer my-test-buffer
    (ghostel-send-string (concat line "\r"))))

(defun my-test-press-C-c ()
  "Press C-c C-c the way a user would, via `ghostel-mode-map'."
  (with-current-buffer my-test-buffer
    (call-interactively (keymap-lookup ghostel-mode-map "C-c C-c"))))

(defun my-test-interrupt (name command)
  "Run COMMAND, press C-c, and check the shell is back and still alive.
Each check echoes arithmetic so the typed command line itself can't match."
  (message "case %s: %s" name command)
  (my-test-type (format "%s && echo DONE-%s-$((40+2))" command name))
  ;; Let the program start before interrupting it.
  (my-test-pause 3)
  (my-test-press-C-c)
  (my-test-pause 1)
  (my-test-type (format "echo BACK-%s-$((6*7))" name))
  (my-test-wait-for (format "BACK-%s-42" name)
                    (format "the shell prompt after C-c (%s)" name))
  (when (string-match-p (format "DONE-%s-42" name) (my-test-buffer-text))
    (my-test-fail "%s ran to completion; C-c did not interrupt it" name))
  (unless (process-live-p (buffer-local-value 'ghostel--process my-test-buffer))
    (my-test-fail "the shell exited after C-c (%s)" name))
  (message "ok: %s interrupted, shell still running" name))

(let ((default-directory (file-name-as-directory (make-temp-file "ghostel-cwd-" t)))
      (ghostel-buffer-name "*ghostel-interrupt-test*")
      (ghostel-kill-buffer-on-exit nil))
  (setq my-test-buffer (ghostel)))

(my-test-type "echo READY-$((1+1))")
(my-test-wait-for "READY-2" "the shell to start")

;; An MSYS2 program on Windows; a plain POSIX one elsewhere.
(my-test-interrupt "sleep" "sleep 60")

;; A native Windows console program run from the MSYS2 shell.
(when (eq system-type 'windows-nt)
  (my-test-interrupt "ping" "ping -n 60 127.0.0.1 > /dev/null"))

(message "PASS")
(kill-emacs 0)

;;; ghostel-interrupt-test.el ends here
