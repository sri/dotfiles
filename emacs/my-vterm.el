(use-package vterm
  :ensure t
  :hook (vterm-mode . (lambda () (hl-line-mode -1))))
