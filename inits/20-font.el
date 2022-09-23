;; set system font
;; TODO: determine font size by monitor DPI
(cond
 ((eq system-type 'darwin)     (custom-set-faces
                                '(default ((t (:family "Fira Code" :slant normal :weight normal :height 150 :width normal))))))
 ((eq system-type 'windows-nt)     (custom-set-faces
                                    '(default ((t (:family "Fira Code" :slant normal :weight normal :height 113 :width normal))))))
 (t     (custom-set-faces
         '(default ((t (:family "Fira Code" :slant normal :weight normal :height 120 :width normal))))))
 )

;; good for reading documentation or anything else in the EWW browser
(custom-set-faces
 '(variable-pitch ((t (:family "OpenDyslexic3")))))

(provide '20-font)
