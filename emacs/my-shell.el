;; -*- lexical-binding: t; -*-
(setenv "PAGER" "cat")

(require 'shell)
(setq shell-font-lock-keywords nil)
(add-to-list 'explicit-bash-args "--login")


(let ((shells '("/bin/zsh" "/bin/bash")))
  (when-let* ((sh (seq-find #'executable-find shells)))
    ;; M-x shell
    (setq explicit-shell-file-name sh)
    ;; shell-command & friends
    (setq shell-file-name sh)))

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
    (let* ((cmd (buffer-substring-no-properties (pos-bol)
                                                (pos-eol)))
           (bufname (format "*%s*" cmd)))
      (cond ((zerop (length cmd))
             ;; do nothing
             )
            ((get-buffer bufname)
             (message "`%s' already exists" bufname))
            (t
             (rename-buffer bufname)
             (comint-send-input))))))

(require 'dash)

(defun my/shell (&optional arg name)
  "Switch to the most recently active shell buffer.
With a prefix arg, create a new shell.
Also, creates a shell when there are no other shells."
  (interactive "P")
  (if (eq major-mode 'shell-mode)
      (tab-line-switch-to-next-tab)
    (if (project-current nil)
        (call-interactively 'project-shell)
      (shell (generate-new-buffer-name (or name "*shell*"))))))

(defun my/shell-dont-scroll ()
  (interactive)
  (let ((point (point)))
    (comint-send-input)
    (goto-char point)
    (recenter 0)))

(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(defun my/tab-line-tabs-project-shell-buffers ()
  (let* ((project-dir (project-current nil))
         (mode-buffers (tab-line-tabs-mode-buffers))
         (result
          (if project-dir
              (-filter (lambda (b)
                         (let ((buf-proj-root
                                (with-current-buffer b
                                  (when-let* ((proj (project-current nil)))
                                    (and proj (project-root proj))))))
                           (string= (project-root project-dir)
                                    buf-proj-root)))
                       mode-buffers)
            (-filter (lambda (b)
                       (string= default-directory
                                (with-current-buffer b
                                  default-directory)))
                     mode-buffers))))
    (set-window-parameter nil 'tab-line-buffers result)
    result))

(setq-default tab-line-close-button-show nil)

(add-hook 'shell-mode-hook
          (lambda ()
            (font-lock-mode -1)

            (setq comint-process-echoes t)

            (setq line-number-mode nil
                  column-number-mode nil)
            (setq comint-input-ignoredups t)
            (setq comint-scroll-to-bottom-on-input t)
            (setq comint-scroll-show-maximum-output nil)
            (toggle-truncate-lines 1)
            (local-unset-key (kbd "C-d"))

            (bind-keys :map comint-mode-map
                       ("C-c C-g" . my/shell-rename-and-run-command)
                       ("C-c d" . dirs)
                       ("C-c <return>" . my/shell-dont-scroll)
                       ("C-c RET" . my/shell-dont-scroll)
                       ("C-c <up>" . comint-previous-prompt)
                       ("C-c <down>" . comint-next-prompt)
                       ("C-c e" . my/shell-erase-buffer)
                       ("C-c n" . my/shell-rename-buffer)
                       ("C-l" . my/shell-bash-clear-screen)
                       ("<right>" . my/shell-forward-char-or-previous-history)
                       ("<down>" . my/shell-next-line-or-next-history)
                       ("M-." . comint-insert-previous-argument))

            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)

            (tab-line-mode 1)
            (setq tab-line-new-tab-choice
                  (lambda () (let ((current-prefix-arg 4)) (my/shell))))

            (setq tab-line-tabs-function 'my/tab-line-tabs-project-shell-buffers)))
