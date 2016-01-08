(require 'magit)

(add-hook 'magit-mode-hook
          (lambda ()
            (nlinum-mode -1)))

(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)
