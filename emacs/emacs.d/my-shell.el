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

(defvar my-shell-bash-esc-dot-counter 0)
(defvar my-shell-bash-esc-dot-last-insertion nil)

(defun my-shell-bash-esc-dot ()
  "Same as Esc-. in bash; insert previous command's last word."
  (interactive)
  (let* ((continue (eq last-command 'my-shell-bash-esc-dot))
         (count (if continue (1+ my-shell-bash-esc-dot-counter) 0))
         (cmd (comint-previous-input-string count))
         (last (if (string-match "\\([`'\"]\\)[^`'\"]+?\\1\\s-*$" cmd)
                   (match-string 0 cmd)
                 (car (last (split-string cmd " " t))))))
    (setq my-shell-bash-esc-dot-counter count)
    (when last
      (when continue
        (delete-region (point)
                       (save-excursion
                         (search-backward my-shell-bash-esc-dot-last-insertion
                                          (point-at-bol)))))
      (setq my-shell-bash-esc-dot-last-insertion last)
      (insert last))))

(defun my-shell-bash-clear-screen ()
  (interactive)
  (recenter-top-bottom 0))

(defun my-shell (&optional arg)
  "Create a new shell (with prefix arg) or switch to a shell buffer."
  (interactive "P")
  (if arg
      (shell (generate-new-buffer-name "*shell*"))
    (let (shells others)
      (dolist (buf (buffer-list))
        (when (eq (with-current-buffer buf major-mode) 'shell-mode)
          (if (string-match "^[*]shell[*]" (buffer-name buf))
              (push buf shells)
            (push buf others))))
      ;; Sort the shells named "*shell*", "*shell*<1>" by their names.
      (setq shells (sort shells (lambda (x y)
                                  (string-lessp (buffer-name x)
                                                (buffer-name y)))))
      ;; Sort the shells not named "*shell*" etc. by their
      ;; creation time.
      (setq others (sort others (lambda (x y)
                                  (< (with-current-buffer x
                                       my-shell-mode-created-at)
                                     (with-current-buffer y
                                       my-shell-mode-created-at)))))
      (cond ((and (null shells) (null others))
             (shell))
            ((eq major-mode 'shell-mode)
             (let ((cur (current-buffer)))
               (switch-to-buffer (if (string-match "^[*]shell[*]" (buffer-name))
                                     (or (cadr (memq cur shells))
                                         (car others)
                                         (car shells))
                                   (or (cadr (memq cur others))
                                       (car shells)
                                       (car others))))))
            (t (switch-to-buffer (or (car shells)
                                     (car others))))))))

(defvar my-shell-mode-created-at nil)
(make-variable-buffer-local 'my-shell-mode-created-at)

(add-hook 'shell-mode-hook
          (lambda ()
            (setq my-shell-mode-created-at (float-time))
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
            (define-key shell-mode-map (kbd "C-l")
              'my-shell-bash-clear-screen)
            (define-key shell-mode-map (kbd "<right>")
              'my-shell-forward-char-or-previous-history)
            (define-key shell-mode-map (kbd "<down>")
              'my-shell-next-line-or-next-history)
            (define-key shell-mode-map (kbd "M-.")
              'my-shell-bash-esc-dot)))
