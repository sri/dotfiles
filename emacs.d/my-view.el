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

(my-define-key view-mode-map (kbd "SPC") 'View-scroll-page-forward)
(my-define-key view-mode-map (kbd "j") 'View-scroll-page-forward)
(my-define-key view-mode-map (kbd "S-SPC") 'View-scroll-page-backward)
(my-define-key view-mode-map (kbd "k") 'View-scroll-page-backward)
(my-define-key view-mode-map "q" 'View-exit-and-edit)
(my-define-key view-mode-map (kbd "a") 'beginning-of-buffer)
(my-define-key view-mode-map (kbd "z") 'end-of-buffer)
(my-define-key view-mode-map (kbd "f") 'my-isearch-forward)
