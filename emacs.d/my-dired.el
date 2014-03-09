(require 'dired-x)

(defun my-dired ()
  (interactive)
  (let ((file-name buffer-file-name))
    (dired default-directory)
    (when file-name
      (dired-goto-file file-name))))

(defun my-dired-first-file ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line 2))

(defun my-dired-last-file ()
  (interactive)
  (goto-char (point-max))
  (dired-previous-line 1))

(my-overwrite-key-bindings-in-mode
 "C-t" 'ido-switch-buffer '(dired-mode))

(add-hook 'dired-mode-hook
          (lambda ()
            (linum-mode -1)
            (dired-omit-mode 1)
            (setq dired-dwim-target t)
            (setq dired-omit-size-limit nil)
            (my-define-key dired-mode-map [mouse-2] 'dired-find-file)
            (my-define-key dired-mode-map "a" 'my-dired-first-file)
            (my-define-key dired-mode-map "z" 'my-dired-last-file)
            (my-define-key dired-mode-map "f" 'my-isearch-forward)
            (my-define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
            (my-define-key dired-mode-map (kbd "SPC") 'scroll-up)
            (my-define-key dired-mode-map (kbd "S-SPC") 'scroll-down)
            (my-define-key dired-mode-map [left] 'dired-up-directory)
            (my-define-key dired-mode-map [right] 'dired-find-file)))
