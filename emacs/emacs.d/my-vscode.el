(require 'subr-x) ;if-let
(require 'cl)
(require 'windmove)

(cl-defun my/shell-for-buffer (&optional create-new)
  "Opens a shell for the current buffer in a window below it.
If a shell that belongs to the buffer already exists in a window
below, it switches to it. A shell is considered to belong to a
buffer if its in the same directory or in the same git repo as
the buffer.

Invoking this command from the shell will close the shell buffer.

With a prefix argument, it always generates a new shell for the
current buffer."
  (interactive "P")

  (when (eq major-mode 'shell-mode)
    (if (window-in-direction 'above) (delete-window))
    (return-from my/shell-for-buffer))

  (cl-flet* ((git-root (buffer)
               (with-current-buffer buffer
                 (shell-command-to-string "git rev-parse --show-toplevel")))
             (shell-in-same-repo-or-dir? (buffer)
               (and (eq 'shell-mode (buffer-local-value 'major-mode buffer))
                    (let ((current (expand-file-name default-directory))
                          (other (expand-file-name
                                  (buffer-local-value 'default-directory
                                                      buffer)))
                      (or (string= current other)
                          (and (or (string-prefix-p current other)
                                   (string-prefix-p other current))
                               (string= (git-root (current-buffer))
                                        (git-root buffer)))))))
             (new-shell (&optional name)
               (unless name
                 (let ((shortened (abbreviate-file-name default-directory)))
                   (setq name (format "*shell %s*"
                                      (generate-new-buffer shortened)))))
               (save-window-excursion
                 (shell name))
               name))

    (let ((win (window-in-direction 'below)))
      (if (and win (shell-in-same-repo-or-dir? (window-buffer win)))
          (windmove-down)
        (split-window-below)
        (windmove-down)
        (let ((new (if create-new
                       (new-shell)
                     (if-let (buffer (cl-find-if #'shell-in-same-repo-or-dir?
                                                 (buffer-list)))
                         (if (get-buffer-process buffer)
                             buffer
                           (new-shell (buffer-name buffer)))
                       (new-shell)))))
          (switch-to-buffer new))))))
