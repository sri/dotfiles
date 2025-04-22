;; -*- lexical-binding: t; -*-
(use-package bm
  :preface
  (defun my/bm-next-this-file ()
    (interactive)
    (let ((bm-cycle-all-buffers nil))
      (bm-next)))
  :bind
  ("M-n" . my/bm-next-this-file)
  ("M-a" . bm-show-all)
  ("M-v" . bm-toggle)
  ("ESC M-v" . bm-show)
  :custom
  (bm-highlight-style 'bm-highlight-only-line)
  (bm-cycle-all-buffers t)
  (bm-recenter t)
  (bm-in-lifo-order t)
  (bm-buffer-persistence t))
