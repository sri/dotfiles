(require 'dired-x)

(defun my-dired ()
  (interactive)
  (dired default-directory))

(defun my-dired-first-file ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line 4))

(defun my-dired-last-file ()
  (interactive)
  (goto-char (point-max))
  (dired-previous-line 1))

(add-hook 'dired-mode-hook
          (lambda ()
            (linum-mode -1)
            (setq dired-omit-files-p t)
            (define-key dired-mode-map [mouse-2] 'dired-find-file)
            (define-key dired-mode-map "a" 'my-dired-first-file)
            (define-key dired-mode-map "z" 'my-dired-last-file)
            (define-key dired-mode-map [left] 'dired-up-directory)
            (define-key dired-mode-map [right] 'dired-find-file)))
