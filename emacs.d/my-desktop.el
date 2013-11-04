(desktop-save-mode 1)
(add-to-list 'desktop-path "~/.emacs.d")

(setq desktop-globals-to-save
      (delq 'register-alist desktop-globals-to-save))

(require 'advice)
(defadvice desktop-buffer-info (after my-desktop-buffer-info (buffer))
  ;; Don't set the major modes or minor modes.
  ;; Basically I want desktop-save only to save the
  ;; filenames not other things that are session based.
  (setcar (nthcdr 3 ad-return-value) nil)
  (setcar (nthcdr 4 ad-return-value) nil))
(ad-activate 'desktop-buffer-info)
