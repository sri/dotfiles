(require 'multiple-cursors)
(require 'region-bindings-mode)

(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "l" 'mc/edit-lines)
