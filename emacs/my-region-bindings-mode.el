(require 'region-bindings-mode)
(require 'multiple-cursors)

(region-bindings-mode-enable)

(bind-keys :map region-bindings-mode-map
           ("l" . mc/edit-lines)
           ("w" . count-words-region)
           ("u" . upcase-region)
           ("d" . downcase-region)
           ("m" . apply-macro-to-region-lines)
           ("C" . my/duplicate-line-or-region)
           (";" . my/comment-line-or-region)
           ("SPC" . exchange-point-and-mark)

           ("a" . mc/mark-all-like-this)
           ("n" . mc/mark-next-like-this)
           )

(add-to-list 'region-bindings-mode-disable-predicates
             'minibufferp)
