(require 'bm)
(setq bm-highlight-style
      (if window-system
          'bm-highlight-only-fringe
        'bm-highlight-only-line))
