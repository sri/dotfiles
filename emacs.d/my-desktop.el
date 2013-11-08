(require 'desktop)

(add-to-list 'desktop-modes-not-to-save 'my-scratch-mode)

(setq desktop-locals-to-save '())
(desktop-save-mode 1)
(add-to-list 'desktop-path "~/.emacs.d")

(let ((del '(register-alist))
      (add '(read-expression-history kill-ring)))
  (dolist (s del)
    (setq desktop-globals-to-save (delq s desktop-globals-to-save)))
  (dolist (s add)
    (push s desktop-globals-to-save)))

(require 'advice)
(defadvice desktop-buffer-info (after my-desktop-buffer-info (buffer))
  ;; Don't save the major or minor modes.  I sometimes disable those
  ;; modes to try out something.  Unfortunately, Desktop remembers
  ;; that desicion causing a lot of confusion.  Also, if this my
  ;; scratch buffer, return things normally.  I don't want to
  ;; desktop-save that buffer. See `desktop-modes-not-to-save' above.
  (unless (eq (with-current-buffer buffer major-mode) 'my-scratch-mode)
    (setcar (nthcdr 3 ad-return-value) nil)
    (setcar (nthcdr 4 ad-return-value) nil)))
(ad-activate 'desktop-buffer-info)
