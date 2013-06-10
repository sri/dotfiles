(defun my-load (path)
  (let* ((path (expand-file-name path)) ;binds w/ default-directory
         (src (concat path ".el"))
         (cpl (concat path ".elc")))
    (if (file-newer-than-file-p src cpl)
        (byte-compile-file src))
    (load cpl nil nil t)))

(let ((my-files '("my-env" "my-keys" "my-dired" "my-shell"
                  "my-fns" "my-packages" "my-bm" "my-magit"
                  "my-sublime" "my-modeline"))
      (default-directory (file-name-directory load-file-name)))
  (dolist (f my-files)
    (my-load f)))

(let ((private (expand-file-name "~/.emacs.private")))
  (when (file-exists-p private)
    (my-load private)))

(message "")
