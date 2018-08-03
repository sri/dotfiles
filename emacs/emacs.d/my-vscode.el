(require 'subr-x) ;if-let
(require 'cl)
(require 'windmove)

(cl-defun my/shell-for-buffer (&optional create-new)
  (interactive "P")

  (when (eq major-mode 'shell-mode)
    (when (window-in-direction 'above) (delete-window))
    (return-from my/shell-for-buffer))

  (cl-flet* ((git-root (buffer)
               (with-current-buffer buffer
                 (shell-command-to-string "git rev-parse --show-toplevel")))
             (in-same-dir-or-repo? (buffer)
               (and (with-current-buffer buffer (eq major-mode 'shell-mode))
                    ;; TODO: slow
                    (string= (git-root (current-buffer))
                             (git-root buffer))))
             (new-shell (&optional name)
               (save-window-excursion
                 (shell (or name
                            (generate-new-buffer (abbreviate-file-name default-directory)))))
               name))

    (let ((win (window-in-direction 'below)))
      (if (and win (in-same-dir-or-repo? (window-buffer win)))
          (windmove-down)
        (split-window-below)
        (windmove-down)
        (let ((new (if create-new
                       (new-shell)
                     (if-let (buffer (cl-find-if #'in-same-dir-or-repo? (buffer-list)))
                         (if (get-buffer-process buffer) buffer (new-shell (buffer-name buffer)))
                       (new-shell)))))
          (switch-to-buffer new))))))
