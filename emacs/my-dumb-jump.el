(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-force-searcher 'rg)
  ;; use completion-read instead of a separate buffer with candidates
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))
