(require 'advice)

(defadvice mouse-drag-region (after my-mouse-hacks (start-event))
  (let ((click-count (event-click-count start-event)))
    (cond ((= click-count 1)
           (when (string-match "^magit-" (format "%s" major-mode))
             (my-magit-click)))
          ((= click-count 2)
           (my-sublime-like-mouse-dblclick-select-fn)))))

(ad-activate 'mouse-drag-region)

(defun my-mouse-ctrl-click (event)
  (interactive "e")
  (mouse-minibuffer-check event)
  (unless (one-window-p t)
    (let ((window (posn-window (event-start event))))
      (select-window (if (framep window)
                         (frame-selected-window window)
                       window))
      (delete-other-windows))))
