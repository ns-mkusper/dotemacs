;; set system font

;; set font size based on monitor ppi
(if (display-graphic-p)
    (funcall
     (lambda()
       (setq my-ppi (floor (my-get-ppi)))

       ;; mac air has ppi of 126.x
       (if (<= my-ppi 92)
           (setq my-font-height 130)
         (if (<= my-ppi 108)
             (setq my-font-height 140)
           (if (<= my-ppi 126)
               (setq my-font-height 140)
             (setq my-font-height 240))))

       (setq my-font-face "Fira Code")
       (setq my-font-height 120)

       ;; set larger font size on mac: say, 160 size becomes 232
       (if (eq system-type 'darwin)
           (setq my-font-height (floor (* 1.4 my-font-height))))

       (set-face-attribute 'default nil
                           :height (symbol-value 'my-font-height)
                           :font (symbol-value 'my-font-face))
       )))

;; good for reading documentation or anything else in the EWW browser or anything in sans font
(custom-set-faces
 '(variable-pitch ((t (:family "OpenDyslexic3")))))

(provide '20-font)
