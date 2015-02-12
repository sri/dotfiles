(require 'advice)
(require 'smart-mode-line)

(sml/setup)
(sml/apply-theme 'dark)

(defun my-mode-line-copy-full-path ()
  "Copies the buffer name to the kill ring.
If that is nil, then it is mode specific as to what gets copied:
 - shell or magit modes, copies the default directory."
  (interactive)
  (let ((name (or (buffer-file-name)
                  (cond ((eq major-mode 'shell-mode)
                         default-directory)
                        ((string-prefix-p "Magit" mode-name)
                         default-directory)))))
    (when name
      (kill-new name)
      (message "Copied `%s'" name))))

(defadvice sml/generate-buffer-identification (after my-mode-line-mouse-click (&rest ignored))
  (add-text-properties 0 (length sml/buffer-identification)
                       '(help-echo "Click to copy buffer name to kill ring")
                       sml/buffer-identification)
  ad-return-value)
(ad-activate 'sml/generate-buffer-identification)

(define-key mode-line-buffer-identification-keymap
  [mode-line mouse-1] 'my-mode-line-copy-full-path)
