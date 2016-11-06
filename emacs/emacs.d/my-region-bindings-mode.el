(require 'region-bindings-mode)

(region-bindings-mode-enable)

(add-to-list 'region-bindings-mode-disable-predicates
             'minibufferp)
