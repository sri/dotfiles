(require 'region-bindings-mode)
(require 'multiple-cursors)

(region-bindings-mode-enable)

(bind-keys :map region-bindings-mode-map
           (";" . my/comment-line-or-region)
           ("A" . beginning-of-buffer)
           ("C" . kill-region)
           ("E" . end-of-buffer)
           ("J" . ace-jump-word-mode)
           ("L" . ace-jump-line-mode)
           ("S" . sort-lines)
           ("SPC" . exchange-point-and-mark)
           ("a" . beginning-of-line)
           ("b" . backward-word)
           ("c" . kill-ring-save)
           ("d" . my/duplicate-line-or-region)
           ("e" . end-of-line)
           ("f" . forward-word)
           ("j" . next-line)
           ("k" . my/region-bindings-k)
           ("l" . my/select-line)
           ("m" . mc/edit-lines)
           ("r" . er/expand-region)
           )

(defun my/region-bindings-k ()
  (interactive)
  (if (eq major-mode 'magit-status-mode)
      (magit-discard)
    (previous-line)))

(add-to-list 'region-bindings-mode-disable-predicates
             'minibufferp)
