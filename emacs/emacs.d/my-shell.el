(setenv "PAGER" "cat")

(defun my-shell-forward-char-or-previous-history (&optional arg)
  (interactive "p")
  (if (eobp)
      (comint-previous-input arg)
    (forward-char arg)))

(defun my-shell-next-line-or-next-history (&optional arg)
  (interactive "p")
  (if (eobp)
      (comint-next-input arg)
    (next-line arg)))

(defun my-shell-erase-buffer ()
  (interactive)
  (erase-buffer)
  (comint-send-input))

(defun my-shell-rename-buffer ()
  (interactive)
  (let ((new-name (read-string "New buffer name: ")))
    (rename-buffer (format "*%s*" new-name))))

(defun my-shell-bash-clear-screen ()
  (interactive)
  (recenter-top-bottom 0))

(defun my-shell (&optional arg)
  "Switch to the most recently active shell buffer.
With a prefix arg, create a new shell.
Also, creates a shell when there are no other shells."
  (interactive "P")
  (cond (arg
         (shell (generate-new-buffer-name "*shell*")))
        (t (let ((shells (-filter (lambda (buffer)
                                    (with-current-buffer buffer
                                      (eq major-mode 'shell-mode)))
                                  (buffer-list))))
             (setq shells
                   (sort shells
                         (lambda (x y)
                           (> (with-current-buffer x
                                my-shell-last-active-time)
                              (with-current-buffer y
                                my-shell-last-active-time)))))
             (cond ((null shells)
                    (shell))
                   ((eq major-mode 'shell-mode)
                    (switch-to-buffer (or (cadr (memq (current-buffer) shells))
                                          (car shells))))
                   (t
                    (switch-to-buffer (car shells))))))))

(defvar-local my-shell-last-active-time nil)

(defun my-shell-update-last-active-time (&optional string)
  (setq my-shell-last-active-time (float-time)))

(add-hook 'shell-mode-hook
          (lambda ()
            (my-shell-update-last-active-time)
            (add-hook 'comint-input-filter-functions
                      'my-shell-update-last-active-time)
            (linum-mode -1)
            (setq line-number-mode nil
                  column-number-mode nil)
            (setq comint-input-ignoredups t)
            (setq comint-scroll-to-bottom-on-input nil)
            (setq comint-scroll-show-maximum-output nil)
            (toggle-truncate-lines 1)
            (define-key shell-mode-map (kbd "C-<up>")
              'comint-previous-prompt)
            (define-key shell-mode-map (kbd "C-<down>")
              'comint-next-prompt)
            (define-key shell-mode-map (kbd "C-c e")
              'my-shell-erase-buffer)
            (define-key shell-mode-map (kbd "C-c n")
              'my-shell-rename-buffer)
            (define-key shell-mode-map (kbd "C-l")
              'my-shell-bash-clear-screen)
            (define-key shell-mode-map (kbd "<right>")
              'my-shell-forward-char-or-previous-history)
            (define-key shell-mode-map (kbd "<down>")
              'my-shell-next-line-or-next-history)
            (define-key shell-mode-map (kbd "M-.")
              'comint-insert-previous-argument)))
