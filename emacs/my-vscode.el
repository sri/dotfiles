;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'windmove)

(defun my/git-root (buffer)
  (with-current-buffer buffer
    (let ((command "git rev-parse --show-toplevel 2> /dev/null"))
      (string-trim (shell-command-to-string command)))))

(defun my/shell-in-same-repo-or-dir-p (buffer)
  (and (eq 'shell-mode (buffer-local-value 'major-mode buffer))
       (let ((current (expand-file-name default-directory))
             (other (expand-file-name
                     (buffer-local-value 'default-directory buffer))))
         (or (string= current other)
             (and (or (string-prefix-p current other)
                      (string-prefix-p other current))
                  (string= (my/git-root (current-buffer)) (my/git-root buffer)))))))

(defun my/shell-for-buffer (&optional create-new)
  "Open a shell for the current buffer."
  (interactive "P")
  (if (eq major-mode 'shell-mode)
      (cond ((window-in-direction 'above) (delete-window))
            (create-new (switch-to-buffer (my/shell))))
    (let ((win (window-in-direction 'below)))
      (if (and win (my/shell-in-same-repo-or-dir-p (window-buffer win)))
          (windmove-down)
        (split-window-below)
        (windmove-down)
        (switch-to-buffer
         (if create-new
             (my/shell)
           (let ((existing
                  (cl-find-if #'my/shell-in-same-repo-or-dir-p (buffer-list))))
             (if existing
                 (if (get-buffer-process existing) existing (my/shell (buffer-name existing)))
               (my/shell)))))))))
