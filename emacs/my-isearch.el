(defun my/isearch-delete-region ()
  (interactive)
  (when isearch-other-end
    (delete-region (point) isearch-other-end)
    (isearch-done)))

;; TODO: handle wrap around
(defun my/isearch-goto-next-non-visible-match ()
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
        ;; TODO: maybe move to the last match
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

(defun my/isearch-yank-whole-word ()
  (interactive)
  (when (= (length isearch-string) 0)
    (skip-chars-backward "a-zA-Z0-9_-"))
  (isearch-yank-internal
   (lambda ()
     (skip-chars-forward "a-zA-Z0-9_-")
     (point))))

(defun my/isearch-goto-first-match ()
  (interactive)
  (widen)
  (goto-char (point-min))
  (isearch-repeat-forward)
  (isearch-update))

(defun my/isearch-goto-last-match ()
  (interactive)
  (widen)
  (goto-char (point-max))
  (isearch-repeat-backward)
  (isearch-update))

(defun my/isearch-repeat ()
  (interactive)
  (if isearch-forward
      (isearch-repeat-forward)
    (isearch-repeat-backward)))

(defun my/isearch-region ()
  (interactive)
  (let ((search-for (buffer-substring-no-properties (region-beginning)
                                                    (region-end))))
    (when (use-region-p)
      (deactivate-mark)
      (isearch-resume search-for nil nil t search-for nil))))

(setq isearch-allow-scroll 'unlimited)
(setq isearch-lazy-highlight-initial-delay 0)

(bind-keys :map isearch-mode-map
           ("M-a" . my/isearch-goto-first-match)
           ("M-e" . my/isearch-goto-last-match)
           ("C-e" . isearch-exit)
           ("<return>" . my/isearch-repeat)
           ("RET" . my/isearch-repeat)
           ("<C-return>" . isearch-exit)
           ("<S-return>" . isearch-repeat-backward)
           ("C-K" . isearch-query-replace-regexp)
           ("C-d" . my/isearch-yank-whole-word)
           ("C-k" . isearch-query-replace)
           ("C-o" . isearch-occur)
           ("C-v" . my/isearch-goto-next-non-visible-match)
           ("\r" . isearch-repeat-forward))
