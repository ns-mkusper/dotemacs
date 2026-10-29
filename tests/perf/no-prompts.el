;;; no-prompts.el --- Answer "no" to every prompt during unattended runs  -*- lexical-binding: t; -*-

;; Loaded before the config in tests/perf runs.  A fresh install asks
;; questions (compile vterm's module? install a tree-sitter grammar?).  In
;; batch those error out, but in the terminal frame used by window-check.el
;; they would wait forever.  Declining keeps both runs unattended; the
;; affected features simply stay off, as on a machine that said no.

(fset 'y-or-n-p (lambda (&rest _) nil))
(fset 'yes-or-no-p (lambda (&rest _) nil))

;;; no-prompts.el ends here
