(defun my/occur-mode-display-occurrence ()
  (interactive)
  (occur-next)
  (occur-mode-display-occurrence))

(bind-keys :map occur-mode-map
           ("n" . my/occur-mode-display-occurrence))

(add-hook 'occur-hook 'occur-rename-buffer)
