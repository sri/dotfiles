(require 'dired-details)

(dired-details-install)
(setq dired-details-hidden-string "")

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "D" 'dired-details-toggle)))
