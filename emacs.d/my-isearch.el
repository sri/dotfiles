;; TODO: handle wrap around
(defun my-isearch-goto-next-non-visible-match ()
  "Go to the next (or previous) match that is beyond this window."
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

(define-key isearch-mode-map (kbd "C-v")
  'my-isearch-goto-next-non-visible-match)