;; see: https://github.com/protesilaos/modus-themes/issues/98
(defun my/set-highlight-indent-guides-faces-default ()
  "Set theme-agnostic `highlight-indent-guides' faces.

Not pretty or dynamic, but they should be serviceable."
  (when (not highlight-indent-guides-auto-enabled)
    (set-face-background 'highlight-indent-guides-odd-face "darkgray")
    (set-face-background 'highlight-indent-guides-even-face "dimgray")
    (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
    (set-face-background 'highlight-indent-guides-top-odd-face "darkgray")
    (set-face-background 'highlight-indent-guides-top-even-face "dimgray")
    (set-face-foreground 'highlight-indent-guides-top-character-face "dimgray")
    (set-face-background 'highlight-indent-guides-stack-odd-face "darkgray")
    (set-face-background 'highlight-indent-guides-stack-even-face "dimgray")
    (set-face-foreground 'highlight-indent-guides-stack-character-face "dimgray")))

(defun my/set-highlight-indent-guides-faces-for-modus ()
  "Customize `highlight-indent-guides' faces for Modus themes.

These faces rely on Modus themes' custom color variables."
  (modus-themes-with-colors
    ;; Customize faces for `highlight-indent-guides-mode' when using
    ;; `modus-themes' since `highlight-indent-guides-mode' cannot seem to
    ;; figure out its faces on its own.
    (when (not highlight-indent-guides-auto-enabled)
      (set-face-background 'highlight-indent-guides-odd-face bg-dim)
      (set-face-background 'highlight-indent-guides-even-face bg-dim)
      (set-face-foreground 'highlight-indent-guides-character-face bg-dim)
      (set-face-background 'highlight-indent-guides-top-odd-face magenta-faint)
      (set-face-background 'highlight-indent-guides-top-even-face magenta-faint)
      (set-face-foreground 'highlight-indent-guides-top-character-face magenta-faint)
      (set-face-background 'highlight-indent-guides-stack-odd-face bg-lavender)
      (set-face-background 'highlight-indent-guides-stack-even-face bg-lavender)
      (set-face-foreground 'highlight-indent-guides-stack-character-face bg-lavender))))

(use-package highlight-indent-guides
  :straight t
  :hook
  (prog-mode . highlight-indent-guides-mode) ;; automatically start mode
  (yaml-mode . highlight-indent-guides-mode)
  (json-mode . highlight-indent-guides-mode)
  :custom
  ;; Only highlight the first character of each indent level instead of the
  ;; entire column. This works better with minimap-mode.
  (highlight-indent-guides-method 'bitmap)
  ;; Whether to calculuate the faces for the indent guides automatically based
  ;; on the current theme. I have found that this method does not work well with
  ;; the Modus themes, so be warned. You may need to set the faces manually.
  (highlight-indent-guides-auto-enabled nil)
  ;; How to visualize your current position in the guide stack.
  ;; Must be one of:
  ;;   nil (default) - Disable responsive guides.
  ;;   top - Use a different color to highlight the "current" guide
  ;;         (the indentation block of the line that the cursor is on).
  ;;   stack - Like `top', but also use a third color for all "ancestor"
  ;;           guides of the current guide.
  (highlight-indent-guides-responsive 'stack)
  :init
  (add-hook 'highlight-indent-guides-mode-hook
            (lambda ()
              (if (seq-some (lambda (th) (string-match "^modus-" (symbol-name th)))
                            custom-enabled-themes)
                  (my/set-highlight-indent-guides-faces-for-modus)
                (my/set-highlight-indent-guides-faces-default)))))


(provide '60-highlight-indentation)
