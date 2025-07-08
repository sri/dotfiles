;; -*- lexical-binding: t; -*-
(defvar my/load-print-messages nil)

(defun my/load-full ()
  (interactive)
  (when (called-interactively-p 'any)
    (setq my/load-print-messages t))
  (load "~/my/dotfiles/emacs/my-dot-emacs-2"))

(let ((skip-customizations
       (file-exists-p (expand-file-name "~/.emacs.skip"))))
  (if skip-customizations
      (dired "~/my/dotfiles/emacs")
    (my/load-full)))
