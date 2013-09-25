(require 'magit)

(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)

(setq magit-status-buffer-switch-function 'switch-to-buffer)

(defun my-magit-click ()
  (cond ((memq major-mode '(magit-log-mode magit-branch-manager-mode))
         (magit-show-item-or-scroll-up))
        ((eq major-mode 'magit-status-mode)
         (let* ((current (magit-current-section))
                (current-title (format "%s" (magit-section-title current)))
                (parent (magit-section-parent current)))
           (if (and parent
                    (eq (magit-section-title parent) 'stashes))
               (magit-show-item-or-scroll-up)
             (unless (string-prefix-p "@@" current-title)
               (magit-toggle-section)))))
        ((memq major-mode '(magit-wazzup-mode
                            magit-commit-mode
                            magit-log-edit-mode
                            magit-stash-mode
                            magit-reflog-mode
                            magit-diff-mode))
         (magit-toggle-section))))
