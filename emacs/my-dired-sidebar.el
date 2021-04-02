(require 'dired-sidebar)

(add-hook 'dired-sidebar-mode-hook
          (lambda ()
            (define-key dired-sidebar-mode-map (kbd "<left>")
              'dired-sidebar-up-directory)
            (define-key dired-sidebar-mode-map (kbd "<right>")
              'dired-sidebar-subtree-toggle)))
