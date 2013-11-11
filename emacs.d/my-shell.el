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
             (setq string ""))))
    (comint-simple-send proc string)))

(defvar my-shell-bash-esc-dot-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'my-shell-bash-esc-dot)
    map))

(defvar my-shell-bash-esc-dot-counter 0)
(make-variable-buffer-local 'my-shell-bash-esc-dot-counter)

(defun my-shell-bash-esc-dot ()
  "Output the last word for the previous command."
  (interactive)
  (let* ((continue (eq last-command 'my-shell-bash-esc-dot))
         (count (if continue (1+ my-shell-bash-esc-dot-counter) 0))
         (cmd (comint-previous-input-string count))
         ;; Fix me -- respect quotes, etc
         (last (car (last (split-string cmd " " t)))))
    (setq my-shell-bash-esc-dot-counter count)
    (when last
      ;; Delete last insertion.
      (when continue
        (save-excursion
          (end-of-line)
          (re-search-backward "[^ ]" (point-at-bol) nil)
          (if (search-backward " " (point-at-bol) t)
              (forward-char 1)
            (beginning-of-line))
          (delete-region (point) (point-at-eol))))
      (insert last))
    (when continue
      (set-temporary-overlay-map my-shell-bash-esc-dot-map t))))

(add-hook 'shell-mode-hook
          (lambda ()
            (my-setup-shell-header-line)
            (linum-mode -1)
            (setq line-number-mode nil
                  column-number-mode nil)
            (setq comint-input-sender
                  'my-shell-input-sender)
            (toggle-truncate-lines 1)
            ;(buffer-disable-undo)
            (dirtrack-mode)
            (define-key shell-mode-map (kbd "C-<up>")
              'comint-previous-prompt)
            (define-key shell-mode-map (kbd "C-<down>")
              'comint-next-prompt)
            (define-key shell-mode-map (kbd "C-c e")
              'my-shell-erase-buffer)
            (define-key shell-mode-map (kbd "<right>")
              'my-shell-forward-char-or-previous-history)
            (define-key shell-mode-map (kbd "<down>")
              'my-shell-next-line-or-next-history)
            (define-key shell-mode-map (kbd "M-.")
              'my-shell-bash-esc-dot)))
