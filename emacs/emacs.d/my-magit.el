(require 'magit)

(setq magit-save-repository-buffers 'dontask)

(add-hook 'magit-mode-hook
          (lambda ()
            (linum-mode -1)))

(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)
