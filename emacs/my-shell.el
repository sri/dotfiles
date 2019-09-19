(setenv "PAGER" "cat")

(require 'shell)
(setq shell-font-lock-keywords nil)
(add-to-list 'explicit-bash-args "--login")

;; This an be set using the SHELL env var, but on Mac bashrc doesn't
;; seem to get read when launched from Dock.
(let ((bash "/usr/local/bin/bash"))
  (when (file-exists-p bash)
    ;; M-x shell
    (setq explicit-shell-file-name bash)
    ;; shell-command & friends
    (setq shell-file-name bash)))

(add-to-list 'display-buffer-alist
             '("^\\*shell\\*" . (display-buffer-same-window)))

;; Disable "Pinging 4.to (Tonga)..." message
;; when you TAB complete
(setq ffap-machine-p-known nil)

(defun my/shell-forward-char-or-previous-history (&optional arg)
  (interactive "p")
  (if (eobp)
      (comint-previous-input arg)
    (forward-char arg)))

(defun my/shell-next-line-or-next-history (&optional arg)
  (interactive "p")
  (if (eobp)
      (comint-next-input arg)
    (next-line arg)))

(defun my/shell-erase-buffer ()
  (interactive)
  (erase-buffer)
  (comint-send-input))

(defun my/shell-rename-buffer ()
  (interactive)
  (let ((new-name (read-string "New buffer name: ")))
    (rename-buffer (format "*%s*" new-name))))

(defun my/shell-bash-clear-screen ()
  (interactive)
  (recenter-top-bottom 0))

(defun my/shell-rename-and-run-command ()
  "Rename buffer to the name of the command typed in.
And then run the command."
  (interactive)
  (when (comint-after-pmark-p) ; rename only when at command prompt
    (let* ((cmd (buffer-substring-no-properties (point-at-bol)
                                                (point-at-eol)))
           (bufname (format "*%s*" cmd)))
      (cond ((zerop (length cmd))
             ;; do nothing
             )
            ((get-buffer bufname)
             (message "`%s' already exists" bufname))
            (t
             (rename-buffer bufname)
             (comint-send-input))))))

(defun my/shell (&optional arg)
  "Switch to the most recently active shell buffer.
With a prefix arg, create a new shell.
Also, creates a shell when there are no other shells."
  (interactive "P")
  (cond (arg
         (shell (generate-new-buffer-name "*shell*")))
        (t (let (shells)

             (dolist (buf (buffer-list))
               (when (eq 'shell-mode (buffer-local-value 'major-mode buf))
                 (push buf shells)))

             (setq shells
                   (sort shells
                         (lambda (x y)
                           (> (buffer-local-value 'my/shell-last-active-time x)
                              (buffer-local-value 'my/shell-last-active-time y)))))

             (cond ((null shells)
                    (shell))
                   ((eq major-mode 'shell-mode)
                    (switch-to-buffer (or (cadr (memq (current-buffer) shells))
                                          (car shells))))
                   (t
                    (switch-to-buffer (car shells))))))))

(defvar-local my/shell-last-active-time nil)

(defun my/shell-update-last-active-time (&optional string)
  (setq my/shell-last-active-time (float-time)))

(defun my/shell-dont-scroll ()
  (interactive)
  (let ((point (point)))
    (comint-send-input)
    (goto-char point)
    (recenter 0)))

(add-hook 'shell-mode-hook
          (lambda ()
            (font-lock-mode t)
            (my/shell-update-last-active-time)
            (add-hook 'comint-input-filter-functions
                      'my/shell-update-last-active-time)
            (setq line-number-mode nil
                  column-number-mode nil)
            (setq comint-input-ignoredups t)
            (setq comint-scroll-to-bottom-on-input t)
            (setq comint-scroll-show-maximum-output nil)
            (toggle-truncate-lines 1)
            (local-unset-key (kbd "C-d"))
            (anzu-mode -1)

            (bind-keys :map comint-mode-map
                       ("C-c C-g" . my/shell-rename-and-run-command)
                       ("C-c d" . dirs)
                       ("C-c <return>" . my/shell-dont-scroll)
                       ("C-c RET" . my/shell-dont-scroll)
                       ("C-<up>" . comint-previous-prompt)
                       ("C-<down>" . comint-next-prompt)
                       ("C-c e" . my/shell-erase-buffer)
                       ("C-c n" . my/shell-rename-buffer)
                       ("C-l" . my/shell-bash-clear-screen)
                       ("<right>" . my/shell-forward-char-or-previous-history)
                       ("<down>" . my/shell-next-line-or-next-history)
                       ("M-." . comint-insert-previous-argument))

            ))
