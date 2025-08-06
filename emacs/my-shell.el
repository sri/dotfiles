;; -*- lexical-binding: t; -*-
(require 'dash)
(require 'cl-lib)
(require 'windmove)
(require 'shell)

(setenv "PAGER" "cat")

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-' - open shell (or switches to, if it exists) window below
;; current buffer, in directory of current buffer. With prefix arg
;; (C-u), open/switch to shell in git repo (if one exists). Shells
;; that have the same root repo show up in the same tab-line-tabs.
;;
;; C-p - switches to other shells in order of most recently used

(defvar my/shell-modes '(shell-mode vterm-mode))

(defun my/shell-switch-to-next-most-recent (&optional buffer)
  "Switch to the most recently active shell buffer."
  (setq buffer (or buffer (current-buffer)))
  (let ((shells (->> (buffer-list)
                     (-filter (lambda (b)
                                (memq (buffer-local-value 'major-mode b) my/shell-modes)))
                     (-sort (lambda (x y)
                              (let ((my (buffer-local-value 'my/shell-last-active-time x))
                                    (other (buffer-local-value 'my/shell-last-active-time y)))
                                (cond ((null my) nil)
                                      ((null other) t)
                                      (t (> my other)))))))))
    (cond ((null shells)) ;; nothing to do
          ((memq major-mode shell-modes)
           (switch-to-buffer (or (cadr (memq buffer shells)) (car shells))))
          (t
           (switch-to-buffer (car shells))))))

(defvar-local my/shell-last-active-time nil)

(defun my/shell-update-last-active-time (&optional string)
  (setq my/shell-last-active-time (float-time)))

(defun my/shell-mode-p (buffer)
  (memq (buffer-local-value 'major-mode buffer) my/shell-modes))

(defun my/shell-in-same-repo-or-dir-p (buffer)
  (and (my/shell-mode-p buffer)
       (let ((current (expand-file-name default-directory))
             (other (expand-file-name (buffer-local-value 'default-directory buffer))))
         (or (string= current other)
             (and (or (string-prefix-p current other)
                      (string-prefix-p other current))
                  (string= (my/git-root (current-buffer))
                           (my/git-root buffer)))))))

(defun my/open-shell-window-for-buffer (&optional open-repo-root-p)
  "Open a shell for the current buffer."
  (interactive "P")
  (if (memq major-mode my/shell-modes)
      (if (window-in-direction 'above) (delete-window))
    (let ((current-dir (expand-file-name default-directory))
          (current-repo)
          (win (window-in-direction 'below)))
      (if (and win
               (if open-repo-root-p
                   (string= (setq current-repo (my/git-root (current-buffer)))
                            (my/git-root (window-buffer win)))
                 (string= current-dir
                          (expand-file-name (buffer-local-value 'default-directory (window-buffer win))))))
          (windmove-down)
        (split-window-below)
        (windmove-down)
        (cond (open-repo-root-p
               (unless current-repo (setq current-repo (my/git-root (current-buffer))))
               (let ((shell-in-current-repo (cl-find-if (lambda (buffer)
                                                          (and (my/shell-mode-p buffer)
                                                               (string= (buffer-local-value 'default-directory buffer)
                                                                        current-repo)))
                                                        (buffer-list))))

                 (if shell-in-current-repo
                     (switch-to-buffer shell-in-current-repo)
                   (let* ((default-directory current-repo)
                          (name (generate-new-buffer-name
                                 (format "*%s shell*"
                                         (file-name-nondirectory current-repo)))))
                     (switch-to-buffer (shell name))))))
              (t
               (let ((shell-in-buffer-dir
                      (cl-find-if (lambda (buffer)
                                    (and (my/shell-mode-p buffer)
                                         (string= current-dir (expand-file-name (buffer-local-value 'default-directory buffer)))))
                                  (buffer-list))))
                 (if shell-in-buffer-dir
                     (switch-to-buffer shell-in-buffer-dir)
                 (let ((name (generate-new-buffer-name
                              (format "*%s shell*"
                                      (file-name-nondirectory current-dir)))))
                   (switch-to-buffer (shell name)))))))))))


(defun my/shell (&optional create-new)
  "Switch to the most recently active shell buffer or create new."
  (interactive "P")
  (if (project-current nil)
      (call-interactively 'project-shell)
    (shell (generate-new-buffer-name "*shell*"))))

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
