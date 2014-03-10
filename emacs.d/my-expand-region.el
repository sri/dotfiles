(require 'expand-region)
(require 'advice)

(my-global-set-key (kbd "C-c s") 'er/expand-region)

(defadvice er/prepare-for-more-expansions-internal
  (after my-expand-region-extra-bindings (repeat-key-str))
  (let* ((result ad-return-value)
         (msg (car result))
         (bindings (cdr result))
         (new-msg (format "%s, 'a' to select all" msg))
         (new-bindings (append (list (cons "a" '(my-er/select-all)))
                               bindings)))
    (setq ad-return-value (cons new-msg new-bindings))))

(ad-activate 'er/prepare-for-more-expansions-internal)

(defun my-er/select-all ()
  (interactive)
  (set-mark (point-min))
  (goto-char (point-max))
  (er--expand-region-1))
