(defun my/occur-mode-display-occurrence ()
  (interactive)
  (occur-mode-display-occurrence)
  (occur-next))

(bind-keys :map occur-mode-map
           ("n" . my/occur-mode-display-occurrence))
