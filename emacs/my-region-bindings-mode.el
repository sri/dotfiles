(require 'region-bindings-mode)
(require 'multiple-cursors)

(region-bindings-mode-enable)

(bind-keys :map region-bindings-mode-map
           ("a" . beginning-of-line)
           ("l" . mc/edit-lines)
           ("j" . ace-jump-word-mode)
           ("J" . ace-jump-word-mode)
           ("L" . ace-jump-line-mode)
           ("e" . er/expand-region)
           ("c" . kill-ring-save)
           ("C" . kill-region)
           ;; ("u" . upcase-region)
           ;; ("d" . downcase-region)
           ("m" . apply-macro-to-region-lines)
           ;; ("C" . my/duplicate-line-or-region)
           (";" . my/comment-line-or-region)
           ("j" . next-line)
           ("k" . previous-line)
           ("SPC" . exchange-point-and-mark)

           ("a" . mc/mark-all-like-this)
           ("n" . mc/mark-next-like-this)
           )

(add-to-list 'region-bindings-mode-disable-predicates
             'minibufferp)
