;; -*- lexical-binding: t; -*-

(unless (boundp 'my/dotfiles-dir)
  ;;; ChatGPT 5.1
  (let* ((target (expand-file-name "~/my/dotfiles/emacs/my-early-init.el"))
         (link   (expand-file-name "~/.emacs.d/early-init.el"))
         (ok     (and (file-symlink-p link)
                      (string-equal (file-truename link)
                                    (file-truename target)))))
  (unless ok
    (when (file-exists-p link)
      (delete-file link))
    (make-symbolic-link target link t)
    (restart-emacs))))

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
