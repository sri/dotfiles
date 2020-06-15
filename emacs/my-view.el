(require 'view)

(defun my/view-scroll-down-one-line ()
  (interactive)
  (scroll-down 1))

(defun my/view-scroll-up-one-line ()
  (interactive)
  (scroll-up 1))

(defun my/view-top-of-window ()
  (interactive)
  (recenter 0))

(defun my/view-center-in-window ()
  (interactive)
  (recenter))

(defun my/view-botton-of-window ()
  (interactive)
  (recenter -1))

(bind-keys :map view-mode-map
           ("SPC" . View-scroll-page-forward)
           ("j" . View-scroll-page-forward)
           ("S-SPC" . View-scroll-page-backward)
           ("k" . View-scroll-page-backward)
           ("a" . beginning-of-buffer)
           ("z" . end-of-buffer)
           ("l" . recenter-top-bottom)
           ("f" . my/isearch-forward)
           ("q" . View-exit-and-edit))
