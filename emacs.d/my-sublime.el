(defun my-sublime-like-mouse-dblclick-select-fn ()
  (let ((isearch-word t)
        (isearch-forward t)
        (beg (min (mark) (point)))
        (string (buffer-substring-no-properties (mark) (point))))
    (unless (string-match "^\n*$" string)
      (deactivate-mark)
      (save-excursion
        (call-interactively 'isearch-forward)
        (goto-char beg)
        (isearch-yank-string string)))))

(defun my-isearch-forward ()
  (interactive)
  (if (and transient-mark-mode (region-active-p))
      (my-sublime-like-mouse-dblclick-select-fn)
    (call-interactively 'isearch-forward)))

(setq isearch-allow-scroll t)

(define-key isearch-mode-map (kbd "<return>")
  'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<S-return>")
  'isearch-repeat-backward)
;(define-key isearch-mode-map (kbd "<backspace>") 'my-isearch-delete-region)

(defun my-isearch-delete-region ()
  (interactive)
  (when isearch-other-end
    (delete-region (point) isearch-other-end)
    (isearch-done)))

(setq isearch-lazy-highlight-initial-delay 0)

(require 'advice)

(defadvice mouse-drag-region (after my-sublime-like-mouse-select (start-event))
  (let ((click-count (event-click-count start-event)))
    (cond ((= click-count 1)
           (when (string-match "^magit-" (format "%s" major-mode))
             (my-magit-click)))
          ((= click-count 2)
           (my-sublime-like-mouse-dblclick-select-fn)))))

(ad-activate 'mouse-drag-region)

(defun my-sublime-expand-selection-to-indentation ()
  (interactive)
  "Expand selection to the next indentation level.
Inspired by Sublime Text."
  (let ((n (current-indentation))
        (beg (point-at-bol))
        (end (point-at-eol)))
    ;; when region is active & transient mark mode is
    ;; turned on, we expand to make that region bigger
    (when (and (region-active-p) transient-mark-mode)
      (setq beg (region-beginning)
            end (region-end))
      (save-excursion
        ;; get the min indentation within the region
        (goto-char beg)
        (forward-line 1)
        (while (< (point) end)
          (setq n (min n (current-indentation)))
          (forward-line 1))
        ;; get the min indentation of line before
        ;; region start, line after region start or n
        (setq n
              (max (progn
                     (goto-char beg)
                     (forward-line -1)
                     (if (bobp) 0 (current-indentation)))
                   (progn
                     (goto-char end)
                     (forward-line 1)
                     (if (eobp) 0 (current-indentation)))))))
    ;; now expand the region
    (save-excursion
      (goto-char beg)
      (forward-line -1)
      (while (and (>= (current-indentation) n) (not (bobp)))
        (forward-line -1))
      (forward-line 1)
      (setq beg (point-at-bol))
      (goto-char end)
      (forward-line 1)
      (while (and (>= (current-indentation) n) (not (eobp)))
        (forward-line 1))
      (forward-line -1)
      (setq end (point-at-eol)))
    (goto-char beg)
    (set-mark beg)
    (goto-char end)))