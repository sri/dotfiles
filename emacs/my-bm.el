(require 'bm)

(defun my/bm-next-this-file ()
  (interactive)
  (let ((bm-cycle-all-buffers nil))
    (bm-next)))

(setq bm-highlight-style 'bm-highlight-only-line
      bm-cycle-all-buffers t
      bm-recenter t
      bm-buffer-persistence t)
