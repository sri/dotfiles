(require 'desktop)

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
  ;; Don't set the major modes or minor modes.
  ;; Basically I want desktop-save only to save the
  ;; filenames not other things that are session based.
  (setcar (nthcdr 3 ad-return-value) nil)
  (setcar (nthcdr 4 ad-return-value) nil))
(ad-activate 'desktop-buffer-info)
