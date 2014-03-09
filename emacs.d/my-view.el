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

(my-define-key view-mode-map "q" 'View-exit-and-edit)
(my-define-key view-mode-map [up] 'View-scroll-page-backward)
(my-define-key view-mode-map [down] 'View-scroll-page-forward)
(my-define-key view-mode-map [left] 'beginning-of-buffer)
(my-define-key view-mode-map [right] 'end-of-buffer)
(my-define-key view-mode-map (kbd "SPC") 'my-view-scroll-up-one-line)
(my-define-key view-mode-map (kbd "S-SPC") 'my-view-scroll-down-one-line)
(my-define-key view-mode-map "t" 'my-view-top-of-window)
(my-define-key view-mode-map "c" 'my-view-center-in-window)
(my-define-key view-mode-map "b" 'my-view-botton-of-window)
