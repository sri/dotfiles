(require 'dired-x)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "a"
              (lambda ()
                (interactive)
                (goto-char (point-min))
                (dired-next-line 4)))
            (define-key dired-mode-map "z"
              (lambda ()
                (interactive)
                (goto-char (point-max))
                (dired-previous-line 1)))
            (define-key dired-mode-map [left] 'dired-up-directory)
            (define-key dired-mode-map [right] 'dired-find-file)))
