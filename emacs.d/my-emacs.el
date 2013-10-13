(defun my-load (filename-no-ext)
  "Compiles the file, if necessary, and then loads it.
Filename-no-ext should be the full name of the file with
the extension."
  (let ((source (concat filename-no-ext ".el"))
        (compiled (concat filename-no-ext ".elc")))
    (when (file-newer-than-file-p source compiled)
      (byte-compile-file source))
    (load compiled nil t t)))

(let ((my-files '("my-env" "my-fns" "my-keys" "my-dired"
                  "my-shell" "my-packages" "my-bm" "my-magit"
                  "my-sublime" "my-modeline"))
      (load-directory (file-name-directory load-file-name)))
  (dolist (f my-files)
    (my-load (expand-file-name f load-directory))))

(let ((private (expand-file-name "~/.emacs.private")))
  (when (file-exists-p (concat private ".el"))
    (my-load private)))

(message "")
