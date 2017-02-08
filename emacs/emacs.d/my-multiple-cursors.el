(require 'multiple-cursors)
(require 'region-bindings-mode)

(bind-keys :map region-bindings-mode-map
           ("a" . mc/mark-all-like-this)
           ("n" . mc/mark-next-like-this)
           ("l" . mc/edit-lines))
