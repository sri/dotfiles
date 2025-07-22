;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'windmove)

(defun my/shell-in-same-repo-or-dir-p (buffer)
  (and (eq 'shell-mode (buffer-local-value 'major-mode buffer))
       (let ((current (expand-file-name default-directory))
             (other (expand-file-name
                     (buffer-local-value 'default-directory buffer))))
         (or (string= current other)
             (and (or (string-prefix-p current other)
                      (string-prefix-p other current))
                  (string= (my/git-root (current-buffer)) (my/git-root buffer)))))))

(defun my/shell-for-buffer ()
  "Open a shell for the current buffer."
  (interactive)
  (if (eq major-mode 'shell-mode)
      (if (window-in-direction 'above)
          (delete-window))
    (let ((win (window-in-direction 'below)))
      (if (and win (my/shell-in-same-repo-or-dir-p (window-buffer win)))
          (windmove-down)
        (split-window-below)
        (windmove-down)
        (switch-to-buffer
         (let ((existing (cl-find-if #'my/shell-in-same-repo-or-dir-p (buffer-list))))
           (if existing
               (if (get-buffer-process existing)
                   existing
                 (my/shell (buffer-name existing)))
             (my/shell))))))))
