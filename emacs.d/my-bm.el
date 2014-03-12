(require 'bm)

(setq bm-highlight-style 'bm-highlight-only-fringe)

(my-global-set-key (kbd "<C-f2>") 'bm-toggle)
(my-global-set-key (kbd "C-S-SPC") 'bm-toggle)
(my-global-set-key (kbd "<f2>") 'bm-next)
(my-global-set-key (kbd "<S-f2>") 'bm-previous)
(my-global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(my-global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(my-global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
