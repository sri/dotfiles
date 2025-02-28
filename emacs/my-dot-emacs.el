(if (equal (read-char "Load all my customizations?" nil 2.0) ?y)
    (load "~/my/dotfiles/emacs/my-dot-emacs-2")
  (dired "~/my/dotfiles/emacs"))
