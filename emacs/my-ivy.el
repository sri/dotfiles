(require 'ivy-avy)

(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
(setq ivy-wrap t)

(defun my/ivy-toggle-mark ()
  "Toggle mark for current candidate and move forwards."
  (interactive)
  (if (ivy--marked-p)
      (ivy-unmark)
    (ivy-mark)))
(define-key ivy-minibuffer-map
            (kbd "C-SPC")
            'my/ivy-toggle-mark)

(add-hook 'minibuffer-setup-hook (lambda () (override-global-mode -1)))
(add-hook 'minibuffer-exit-hook (lambda () (override-global-mode 1)))


(define-key ivy-minibuffer-map
            (kbd "S-<down>")
            'ivy-scroll-up-command)
(define-key ivy-minibuffer-map
            (kbd "S-<up>")
            'ivy-scroll-down-command)
(define-key ivy-minibuffer-map
            (kbd "C-i")
            'ivy-avy)
