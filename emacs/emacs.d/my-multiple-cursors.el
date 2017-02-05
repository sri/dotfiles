(require 'multiple-cursors)
(require 'region-bindings-mode)

(bind-keys :map region-bindings-mode-map
           ("a" . c/mark-all-like-this)
           ("n" . c/mark-next-like-this)
           ("l" . mc/edit-lines))
