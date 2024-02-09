(require 'ivy-avy)
(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
(setq ivy-wrap t)
(define-key ivy-minibuffer-map
            (kbd "S-<down>")
            'ivy-scroll-up-command)
(define-key ivy-minibuffer-map
            (kbd "S-<up>")
            'ivy-scroll-down-command)
(define-key ivy-minibuffer-map
            (kbd "C-i")
            'ivy-avy)
