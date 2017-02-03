(defun my-load-all ()
  (interactive)
  (let ((default-directory "~/my/dotfiles/emacs/emacs.d"))

    (my-load (if window-system "my-gui" "my-terminal"))

    ;; Files that aren't on MELPA or any other
    ;; package archive.
    (mapc 'my-load
          (directory-files "~/.emacs.d/third-party" 'full "\\.el$" t))

    (mapc 'my-load
          '(
            "my-fns"
            "my-env"
            "my-keys"
            "my-view"
            "my-sublime"
            "my-shell"
            "my-occur"
            "my-isearch"
            "my-help"
            "my-dired"
            "my-update-dot-emacs"
            ))

    (mapc (lambda (pkg)
            (my-load (format "my-%s" pkg) 'ignore-if-missing))
          (remove 'diminish package-selected-packages))

    (my-load "my-diminish")
    (my-load "~/.emacs.private.el" 'ignore-if-missing)))

(my-load-all)

(unless window-system
  (recentf-open-files))
