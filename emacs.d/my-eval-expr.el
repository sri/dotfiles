(require 'eval-expr)

(eval-expr-install)
(setq eval-expr-print-function 'pp
      eval-expr-print-level nil
      eval-expr-print-length nil)
