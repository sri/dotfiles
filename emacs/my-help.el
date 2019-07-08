(require 'help-mode)

(defun my/help-push-next-button ()
  (interactive)
  (forward-button 1 t)
  (push-button))

(bind-keys :map help-mode-map
           ("b" . help-go-back)
           ("f" . help-go-forward)
           ("n" . forward-button)
           ("p" . backward-button)
           ("x" . delete-window)
           ("g" . my/help-push-next-button))
