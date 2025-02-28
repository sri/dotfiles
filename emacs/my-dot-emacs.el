(let ((skip-customizations
       (equal ?y
              (ignore-errors
                ;; Mouse clicks throws an error.
                (read-char "Skip my customizations?" nil 2.0)))))
  (if skip-customizations
      (dired "~/my/dotfiles/emacs")
    (load "~/my/dotfiles/emacs/my-dot-emacs-2")))
