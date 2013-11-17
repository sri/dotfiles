(require 'view)

(defun my-view-scroll-down-one-line ()
  (interactive)
  (scroll-down 1))

(defun my-view-scroll-up-one-line ()
  (interactive)
  (scroll-up 1))

(defun my-view-top-of-window ()
  (interactive)
  (recenter 0))

(defun my-view-center-in-window ()
  (interactive)
  (recenter))

(defun my-view-botton-of-window ()
  (interactive)
  (recenter -1))

(define-key view-mode-map "q" 'View-exit-and-edit)
(define-key view-mode-map [up] 'View-scroll-page-backward)
(define-key view-mode-map [down] 'View-scroll-page-forward)
(define-key view-mode-map [left] 'beginning-of-buffer)
(define-key view-mode-map [right] 'end-of-buffer)
(define-key view-mode-map (kbd "SPC") 'my-view-scroll-up-one-line)
(define-key view-mode-map (kbd "S-SPC") 'my-view-scroll-down-one-line)
(define-key view-mode-map "t" 'my-view-top-of-window)
(define-key view-mode-map "c" 'my-view-center-in-window)
(define-key view-mode-map "b" 'my-view-botton-of-window)
