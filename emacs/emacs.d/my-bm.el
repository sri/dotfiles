(require 'bm)

(setq bm-highlight-style
      (if window-system
          'bm-highlight-only-fringe
        'bm-highlight-only-line))

(defvar my-bm-next-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'bm-next)
    map))

(defun my-bm-next ()
  (interactive)
  (message "Hit 'n' to goto the next bookmark")
  (bm-next)
  (set-temporary-overlay-map my-bm-next-keymap t))

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "C-S-SPC") 'bm-toggle)
(global-set-key (kbd "C-c m") 'bm-toggle)
(global-set-key (kbd "C-c n") 'my-bm-next)
(global-set-key (kbd "<f2>") 'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
(global-set-key (kbd "<C-S-f2>") 'bm-remove-all-current-buffer)
(global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
