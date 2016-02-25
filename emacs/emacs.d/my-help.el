(require 'help-mode)

(defun my-help-push-next-button ()
  (interactive)
  (forward-button 1 t)
  (push-button))

(define-key help-mode-map (kbd "b") 'help-go-back)
(define-key help-mode-map (kbd "f") 'help-go-forward)
(define-key help-mode-map (kbd "n") 'forward-button)
(define-key help-mode-map (kbd "p") 'backward-button)
(define-key help-mode-map (kbd "x") 'delete-window)
(define-key help-mode-map (kbd "g") 'my-help-push-next-button)

(add-hook 'help-mode-hook
          (lambda ()
            (linum-mode -1)))
