;; Center Emacs's position on screen
(when window-system
  (let* ((height 40)
         (width 80)
         (screen-height (x-display-pixel-height))
         (screen-width (x-display-pixel-width))
         (top (/ (- screen-height (frame-pixel-height)) 2))
         (left (/ (- screen-width (frame-pixel-width)) 2)))
    (add-to-list 'default-frame-alist (cons 'height 40))
    (add-to-list 'default-frame-alist (cons 'width 80))
    (add-to-list 'default-frame-alist (cons 'top top))
    (add-to-list 'default-frame-alist (cons 'left left))))

(defun my-toggle-fullscreen ()
  (interactive)
  (cond ((eq window-system 'x)
         (let ((fullp (frame-parameter nil 'fullscreen)))
           (set-frame-parameter nil 'fullscreen
                                (if fullp nil 'fullscreen))))))

(defun my-frame-transparency (arg)
  (interactive "p")
  (set-frame-parameter nil 'alpha (list arg arg)))

(set-frame-parameter nil 'alpha '(100 100))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
