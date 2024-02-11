(require 'ivy)
(require 'counsel)
(require 'ivy-avy)

(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
(setq ivy-wrap t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-use-virtual-buffers t)
(setq counsel-yank-pop-separator "\n----------------\n")

(defun my/ivy-toggle-mark ()
  "Toggle mark for current candidate and move forwards."
  (interactive)
  (if (ivy--marked-p)
      (ivy-unmark)
    (ivy-mark)))

(add-hook 'minibuffer-setup-hook (lambda () (override-global-mode -1)))
(add-hook 'minibuffer-exit-hook (lambda () (override-global-mode 1)))

(bind-keys :map ivy-minibuffer-map
           ("S-<down>" . ivy-scroll-up-command)
           ("S-<up>" . ivy-scroll-down-command)
           ("C-i" . ivy-avy)
           ("C-SPC" . my/ivy-toggle-mark))
