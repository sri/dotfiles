(setenv "PAGER" "cat")

(require 'dirtrack)
(setq-default dirtrack-list (list "^\\([^(]+\\)\\( .*?\\)?[$] $" 1))
;;
;; (let ((possibles '("/$ " "~$ " "~/my/test (master)$ "))
;;       (results '()))
;;   (dolist (p possibles)
;;     (push (cons p
;;                 (and (string-match (car dirtrack-list) p)
;;                      (match-string (cadr dirtrack-list) p)))
;;           results))
;;   results)

(defun my-setup-shell-header-line ()
  (setq header-line-format
        (list (propertize " [↩]"
                          'mouse-face 'mode-line-highlight
                          'help-echo "erase buffer"
                          'local-map (my-make-header-line-mouse-map
                                      'down-mouse-1 #'ignore
                                      'mouse-1 #'my-header-line-shell-erase-buffer))
              (propertize " [↩!]"
                          'mouse-face 'mode-line-highlight
                          'help-echo "erase buffer & run prev cmd"
                          'local-map (my-make-header-line-mouse-map
                                      'down-mouse-1 #'ignore
                                      'mouse-1 #'my-header-line-shell-erase-buffer-and-run-prev-cmd)))))

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

;; If there is a visible "rspec" (*_spec.rb) buffer in the
;; current frame, running a "rspec" command is a shell process,
;; will run rspec against that buffer's file name (at the line
;; where the cursor is in that buffer). Add this to your shell-mode-hook
;; to enable this feature:
;;     (add-hook 'shell-mode-hook
;;               (lambda ()
;;                 (setq comint-input-sender 'my-shell-rspec-command)))
;;
(defun my-shell-rspec-command (proc string)
  (when (string-match "^rspec\n?$" string)
    (let ((buffers (mapcar #'window-buffer (window-list)))
          (spec-buffer nil))
      (dolist (buf buffers)
        (when (string-match "_(spec|test)[.]rb$" (or (buffer-file-name buf) ""))
          (setq spec-buffer buf)))
      (when spec-buffer
        (let ((n (with-current-buffer spec-buffer
                   (line-number-at-pos))))
          (setq string (format "rspec %s:%d"
                               (buffer-file-name spec-buffer)
                               n))
          (message "Running \"%s\"" string)))))
  (comint-simple-send proc string))

(defun my-shell-input-sender (proc string)
  (let ((curbuf (current-buffer)))
    (cond ((string-match "^eo \\(.*\\)$" string)
           (let ((path (match-string 1 string)))
             (when (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" path)
               (setq path (replace-match "" t t path)))
             (find-file-other-window path)
             (set-buffer curbuf)
             (setq string "")))
          ((string-match "^rb \\(.*\\)$" string)
           (let ((new-buffer-name (match-string 1 string)))
             (rename-buffer (format "*%s*" new-buffer-name))
             (setq string ""))))
    (comint-simple-send proc string)))

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
            ;; (my-setup-shell-header-line)
            (setq my-shell-mode-created-at (float-time))
            (linum-mode -1)
            (setq line-number-mode nil
                  column-number-mode nil)
            (setq comint-input-ignoredups t)
            (setq comint-input-sender
                  'my-shell-input-sender)
            ;; (setq comint-prompt-read-only t)
            (setq comint-scroll-to-bottom-on-input nil)
            (setq comint-scroll-show-maximum-output nil)
            (toggle-truncate-lines 1)
            (dirtrack-mode)
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
