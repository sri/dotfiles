(require 'region-bindings-mode)
(require 'multiple-cursors)

(region-bindings-mode-enable)

(bind-keys :map region-bindings-mode-map
           ("p" . previous-line)
           ("e" . end-of-line)
           ("a" . beginning-of-line)
           ("w" . forward-word)
           ("b" . backward-word)
           ("a" . mc/mark-all-like-this)
           ("n" . mc/mark-next-like-this)
           ("l" . mc/edit-lines))

(add-to-list 'region-bindings-mode-disable-predicates
             'minibufferp)
