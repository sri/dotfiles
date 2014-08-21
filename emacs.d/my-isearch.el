(require 'isearch)

;; TODO: handle for isearch-backward and wrap around
(defun my-isearch-goto-next-non-visible-match ()
  "Go to the next match that is beyond this window."
  (interactive)
  ;; See search-forward-lax-whitespace & friends for this pattern:
  (let* ((search-spaces-regexp search-whitespace-regexp)
         (next-non-visible-match
          (save-excursion
            (goto-char (window-end))
            (re-search-forward (regexp-quote isearch-string) nil t))))
    (if (null next-non-visible-match)
        (message "No matches found beyond this window")
      (goto-char next-non-visible-match)
      (goto-char (point-at-bol))
      (recenter 4)
      (isearch-search)
      (isearch-update))))

(define-key isearch-mode-map (kbd "C-v")
  'my-isearch-goto-next-non-visible-match)
