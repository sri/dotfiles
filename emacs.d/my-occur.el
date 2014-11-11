(require 'replace)

(defun my-occur-mode-display-occurrence ()
  (interactive)
  (occur-mode-display-occurrence)
  (occur-next))

(define-key occur-mode-map (kbd "n")
  'my-occur-mode-display-occurrence)
