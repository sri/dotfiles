;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'button)

(defun my/update-dot-emacs ()
  (lexical-let ((update-buffer-name
                 (generate-new-buffer "*Update-Dot-Emacs*")))
    (cl-labels
        ((run (program &rest args)
           (let ((running-msg (format "Running `%s %s'..."
                                      program
                                      (mapconcat 'identity args " "))))
             (insert running-msg "\n")
             (apply #'call-process program nil t nil args)))

         (do-update (button)
           (let ((default-directory "~/my/dotfiles")
                 (stash-first (button-get button 'stash-first)))
             (with-current-buffer update-buffer-name
               (goto-char (point-max))
               (insert "\n\nUpdating...\n")
               (when stash-first
                 (run "git" "stash" "save" "-u"
                      "\"DONT DELETE: stashed by my/update-dot-emacs.el\""))
               (run "git" "pull" "--rebase")
               (let ((status
                      (concat (if stash-first
                                  "Updated & stashed."
                                "Updated (nothing stashed).")
                              " Restart Emacs to include changes.")))
                 (message status)
                 (insert "\n\n" status)))))

         (show-maximum-output ()
           (switch-to-buffer (current-buffer) t)
           (delete-other-windows)
           ;; If there is a big diff -- I want to see the whole thing.
           (unless (pos-visible-in-window-p (point-min))
             (split-window-right)
             (goto-char (point-min))
             (follow-mode))))

      (with-current-buffer update-buffer-name
        (let ((default-directory "~/my/dotfiles")
              (master)
              (origin/master))

          (run "git" "fetch")

          ;; Both `master' and `origin/master' will end with newlines.
          (setq master (shell-command-to-string "git rev-parse master"))
          (setq origin/master
                (shell-command-to-string "git rev-parse origin/master"))
          (insert "Comparing master and origin/master:\n"
                  (format "         master: %s" master)
                  (format "  origin/master: %s" origin/master))

          (unless (string= master origin/master)
            (let* ((diff (shell-command-to-string "git diff"))
                   (stash-first (> (length diff) 0)))
              (if stash-first
                  (insert "Local changes:\n" diff)
                (insert "No local changes"))
              (insert "\n\n")
              (insert-button (if stash-first
                                 "Git Stash and Update Dot Emacs"
                               "Update Dot Emacs (no local changes)")
                             'action #'do-update
                             'stash-first stash-first)
              (show-maximum-output)
              (beginning-of-line))))))))
