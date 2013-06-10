(require 'dired-x)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map [left] 'dired-up-directory)
            (define-key dired-mode-map [right] 'dired-find-file)))
