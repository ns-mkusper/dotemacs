;; set system font

;; set font size based on monitor ppi
(if (display-graphic-p)
    (funcall
     (lambda()
       (setq my-ppi (floor (my-get-ppi)))

       (setq my-font-face "Fira Code")
       (setq my-font-height 110)

       ;; mac air has ppi of 126.x
       (if (<= my-ppi 92)
           (setq my-font-height 120)
         (if (<= my-ppi 113)
             (setq my-font-height 110)
           (if (<= my-ppi 126)
               (setq my-font-height 120)
             (setq my-font-height 120))))



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

;; (defcustom my-fonts:han "Noto Sans CJK"
;;   "A font to render Han characters."
;;   :tag "Han Font"
;;   :group 'my-fonts
;;   :type 'string)

;; (defun my-fonts:set-fallbacks:han (&optional frame)
;;   (set-fontset-font "fontset-default" 'han
;;                     (font-spec :name my-fonts:han) frame))

;; (defcustom my-fonts:emoji "Noto Color Emoji"
;;   "A font to render emoji characters."
;;   :tag "Emoji Font"
;;   :group 'my-fonts
;;   :type 'string)

;; (defcustom my-fonts:emoji-charpoints
;;   '(#x203c #x2049 #x20e3 #x2139 (#x21a9 . #x21aa) (#x231a . #x231b) #x2328
;;            #x23cf (#x23e9 . #x23f3) (#x23f8 . #x23fa) #x24c2 (#x25fb . #x25fe)
;;            (#x2600 . #x2604) #x260e #x2611 (#x2614 . #x2615) #x2618 #x261d #x2620
;;            (#x2622 . #x2623) #x2626 #x262a (#x262e . #x262f) (#x2638 . #x263a) #x2640
;;            #x2642 (#x2648 . #x2653) (#x265f . #x2660) #x2663 (#x2665 . #x2666) #x2668
;;            #x267b (#x267e . #x267f) (#x2692 . #x2697) #x2699 (#x269b . #x269c) #x26a7
;;            (#x26aa . #x26ab) (#x26b0 . #x26b1) (#x26bd . #x26be) (#x26c4 . #x26c5)
;;            #x26c8 (#x26ce . #x26cf) #x26d1 (#x26d3 . #x26d4) (#x26e9 . #x26ea)
;;            (#x26f0 . #x26f5) (#x26f7 . #x26fa) #x26fd #x2702 #x2705 (#x2708 . #x270d)
;;            #x270f #x2712 #x2714 #x2716 #x271d #x2721 #x2728 (#x2733 . #x2734) #x2744
;;            #x2747 #x274c #x274e (#x2753 . #x2755) #x2757 (#x2763 . #x2764)
;;            (#x2795 . #x2797) #x27a1 #x27b0 #x27bf (#x2934 . #x2935) (#x2b05 . #x2b07)
;;            (#x2b1b . #x2b1c) #x2b50 #x2b55 #x3030 #x303d #x3297 #x3299
;;            (#x1f000 . #xff000))
;;   "A fontface to render Emoji characters."
;;   :tag "Emoji font charpoints"
;;   :group 'my-fonts
;;   :type '(repeat (radio (integer :tag "Codepoint")
;;                         (cons :tag "Range"
;;                               (integer :tag "First")
;;                               (integer :tag "Last")))))

;; (defun my-fonts:set-fallbacks:emoji (&optional frame)
;;   (let ((font (font-spec :name my-fonts:emoji)))
;;     (dolist (chars my-fonts:emoji-charpoints)
;;       (set-fontset-font "fontset-default" chars font frame))))

;; (dolist (fn '(my-fonts:set-fallbacks:han
;;               my-fonts:set-fallbacks:emoji))
;;   (add-hook 'after-make-frame-functions fn)
;;   (funcall fn))

(provide '20-font)
