(with-eval-after-load 'consult
  (define-key consult-narrow-map (kbd "C-<return>") #'vertico-exit-input))
