(defun my-isearch-search-for-selected ()
  (let ((isearch-word t)
        (isearch-forward t)
        (beg (min (mark) (point)))
        (string (buffer-substring-no-properties (mark) (point))))
    (unless (string-match "^\n*$" string)
      (deactivate-mark)
      (save-excursion
        (call-interactively 'isearch-forward)
        (goto-char beg)
        (isearch-yank-string string)
        (message "%d matches" (count-matches string
                                             (point-min)
                                             (point-max)))))))
(defun my-isearch-forward ()
  (interactive)
  (if (let (use-empty-active-region)
        (use-region-p))
      (my-isearch-search-for-selected)
    (call-interactively 'isearch-forward)))

(defun my-isearch-delete-region ()
  (interactive)
  (when isearch-other-end
    (delete-region (point) isearch-other-end)
    (isearch-done)))

;; TODO: handle wrap around
(defun my-isearch-goto-next-non-visible-match ()
  "Go to the next (or previous) match that isn't visible on screen."
  (interactive)
  (let ((next-non-visible-match
         (let ((search-spaces-regexp search-whitespace-regexp))
           (save-excursion
             (cond (isearch-forward
                    (goto-char (window-end))
                    (re-search-forward (regexp-quote isearch-string) nil t))
                   (t
                    (goto-char (window-start))
                    (re-search-backward (regexp-quote isearch-string) nil t)))))))
    (if (null next-non-visible-match)
        (message "No matches found beyond this window")
      (goto-char next-non-visible-match)
      (cond (isearch-forward
             (goto-char (point-at-bol))
             (recenter 4))
            (t
             (goto-char (point-at-eol))
             (recenter -4)))
      (isearch-search)
      (isearch-update))))

(defun my-isearch-yank-whole-word ()
  (interactive)
  (when (= (length isearch-string) 0)
    (skip-chars-backward "a-zA-Z0-9_-"))
  (isearch-yank-internal
   (lambda ()
     (skip-chars-forward "a-zA-Z0-9_-")
     (point))))

(setq isearch-allow-scroll t)
(setq isearch-lazy-highlight-initial-delay 0)

(define-key isearch-mode-map (kbd "C-e") 'isearch-exit)
(define-key isearch-mode-map "\r" 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<S-return>") 'isearch-repeat-backward)
;(define-key isearch-mode-map (kbd "<backspace>") 'my-isearch-delete-region)
(define-key isearch-mode-map (kbd "C-K") 'isearch-query-replace-regexp)
(define-key isearch-mode-map (kbd "C-d") 'my-isearch-yank-whole-word)
(define-key isearch-mode-map (kbd "C-k") 'isearch-query-replace)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
(define-key isearch-mode-map (kbd "C-v") 'my-isearch-goto-next-non-visible-match)
