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
  (isearch-yank-internal
   (lambda ()
     ;; If in word. select word
     (skip-chars-forward "^ \n\t\r")
     (point))))


(define-key isearch-mode-map (kbd "C-v")
  'my-isearch-goto-next-non-visible-match)

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
(define-key isearch-mode-map (kbd "C-k") 'isearch-query-replace)
(define-key isearch-mode-map (kbd "C-K") 'isearch-query-replace-regexp)
(define-key isearch-mode-map (kbd "C-d") 'my-isearch-yank-whole-word)
