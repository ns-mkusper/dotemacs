;; in-emacs gpt prompt
(use-package gptel
  :straight (gpt :type git :host github :repo "karthink/gptel"))

;; allows code generation from gpt
(use-package codegpt
  :straight (codegpt :type git :host github :repo "emacs-openai/codegpt"))

;; BARD IN EMACS USING bard-rd (https://github.com/Alfex4936/Bard-rs.git)
;; Taken from https://www.birkey.co/2023-06-18-google-bard-and-emacs.html
(defun kcompilation-start (cmd name &optional mode)
  (let* ((compile-command nil)
         (compilation-save-buffers-predicate 'ignore)
         (compilation-buffer
          (compilation-start cmd
                             (if (equal mode 'read-only) nil t)
                             (lambda (m)
                               (or (when (boundp 'name)
                                     (format "*%s*" name))
                                   (buffer-name))))))
    (when current-prefix-arg
      (with-current-buffer compilation-buffer
        (switch-to-prev-buffer (get-buffer-window (current-buffer)))))
    (message (format "Running %s in %s ..." cmd name))))

(defun kprompt-bard (&optional p)
  "Prompts for input to send it to `bard` using `bard-rs` in
*bard-prompt* buffer. If mark-active, uses the text in the region
 as the prompt"
  (interactive "P")
  (let* ((bs "bard-prompt")
         (bname (format "*%s*" bs))
         (bname (if (get-buffer bname)
                    bname
                  (progn (kcompilation-start "bard-rs -e ~/.env" bs)
                         bname)))
         (prompt (if mark-active
                     (replace-regexp-in-string
                      "\n"
                      ""
                      (buffer-substring-no-properties (region-beginning) (region-end)))
                   (read-string "AI Chat Prompt: "))))
    (with-current-buffer (pop-to-buffer bname)
      (when p
        (end-of-buffer)
        (insert "!reset")
        (comint-send-input)
        (end-of-buffer)
        (insert prompt)
        (comint-send-input))
      (when (not p)
        (end-of-buffer)
        (insert prompt)
        (comint-send-input)))))

(provide '60-gpt)
