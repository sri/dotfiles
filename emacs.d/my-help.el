(require 'help-mode)

(defun my-help-push-next-button ()
  (interactive)
  (forward-button 1 t)
  (push-button))

(my-define-key help-mode-map (kbd "b") 'help-go-back)
(my-define-key help-mode-map (kbd "f") 'help-go-forward)
(my-define-key help-mode-map (kbd "n") 'forward-button)
(my-define-key help-mode-map (kbd "p") 'backward-button)
(my-define-key help-mode-map (kbd "x") 'delete-window)
(my-define-key help-mode-map (kbd "g") 'my-help-push-next-button)
