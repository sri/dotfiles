(defun my-swap-line-or-region-down ()
  "Move the current line or region down one line."
  (interactive "*")
  (let (beg end line reg-beg reg-end)
    (when (use-region-p)
      (setq reg-beg (region-beginning))
      (setq reg-end (region-end)))
    ;; Save & delete the next line.
    (save-excursion
      (forward-line 1)
      (setq beg (point))
      (forward-line 1)
      (setq end (point)))
    (setq line (buffer-substring beg end))
    (delete-region beg end)
    (save-excursion
      (when reg-beg (goto-char reg-beg))
      (beginning-of-line)
      (insert line))
    (when reg-beg
      (set-mark (+ reg-beg (length line)))
      (goto-char (+ reg-end (length line)))
      (setq deactivate-mark nil))))

(defun my-swap-line-or-region-up ()
  "Move the current line or region up one line."
  (interactive "*")
  (let (beg end line reg-beg reg-end)
    (when (use-region-p)
      (setq reg-beg (region-beginning))
      (setq reg-end (region-end)))
    ;; Save & delete the previous line.
    (save-excursion
      (when reg-beg (goto-char reg-beg))
      (forward-line -1)
      (setq beg (point))
      (forward-line 1)
      (setq end (point)))
    (setq line (buffer-substring beg end))
    (delete-region beg end)
    (save-excursion
      (when reg-end (goto-char (- reg-end (length line))))
      (forward-line 1)
      (insert line))
    (when reg-end
      (set-mark (- reg-beg (length line)))
      (goto-char (- reg-end (length line)))
      (setq deactivate-mark nil))))

(defvar my-line-or-region-swap-keymap
  (let ((map (make-sparse-keymap)))
    (my-define-key map [up] 'my-swap-line-or-region-up)
    (my-define-key map [down] 'my-swap-line-or-region-down)
    map))

(defun my-start-line-or-region-swap ()
  (interactive)
  (message "Hit [up] or [down] to move region or line in that direction")
  (set-temporary-overlay-map my-line-or-region-swap-keymap t))

(defun my-duplicate-line-or-region ()
  "Duplicate line or current region."
  (interactive "*")
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (point-at-region-beg (= (point) beg))
             (region (buffer-substring beg end)))
        (cond (point-at-region-beg
               (goto-char end)
               (insert region)
               (goto-char beg)
               (set-mark end))
              (t (insert region)
                 (set-mark end)))
        (setq deactivate-mark nil))
    (let ((line (buffer-substring (point-at-bol) (point-at-eol)))
          (column (current-column)))
      (end-of-line)
      (if (eobp)
          (insert "\n")
        (forward-char 1))
      (save-excursion
        (insert line)
        (unless (eobp) (insert "\n")))
      (move-to-column column))))

(defun my-comment-line-or-region ()
  "Comment or uncomment the current line or region."
  (interactive "*")
  (cond ((region-active-p)
         (comment-or-uncomment-region (region-beginning)
                                      (region-end))
         (setq deactivate-mark nil))
        (t
         (comment-or-uncomment-region (point-at-bol)
                                      (point-at-eol)))))

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

(my-define-key isearch-mode-map (kbd "<return>")
  'isearch-repeat-forward)
(my-define-key isearch-mode-map (kbd "<S-return>")
  'isearch-repeat-backward)
;(my-define-key isearch-mode-map (kbd "<backspace>") 'my-isearch-delete-region)

(defun my-isearch-delete-region ()
  (interactive)
  (when isearch-other-end
    (delete-region (point) isearch-other-end)
    (isearch-done)))

(setq isearch-lazy-highlight-initial-delay 0)

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
