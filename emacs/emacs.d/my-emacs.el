(require 'dash)

(defun my-load-all ()
  (interactive)
  (let ((default-directory "~/my/dotfiles/emacs/emacs.d")
        (my-files
         (list "my-fns"
               "my-env"
               "my-keys"
               "my-view"
               "my-sublime"
               "my-shell"
               "my-occur"
               "my-isearch"
               "my-help"
               "my-dired"
               "my-org-activity-log"
               "my-update-dot-emacs"
               ))
        (packages-custom-order
         ;; Load order of packages customized by me.
         ;; Gets loaded in this order after
         ;; other package custom files.
         '(diminish))
        (my-non-packages
         ;; Files that aren't on MELPA or any other
         ;; package archive.
         (directory-files "~/.emacs.d/third-party" 'full "\\.el$" t))
        (my-private "~/.emacs.private.el"))

    (my-load (if window-system "my-gui" "my-terminal"))

    (mapc 'my-load my-non-packages)
    ;; Load all my files: main reason to load this
    ;; before package customizations: they might
    ;; depend on functions in my-fns.el (and other
    ;; dependencies like that).
    (mapc 'my-load my-files)

    (dolist (pkg package-selected-packages)
      (unless (memq pkg packages-custom-order)
        (my-load (format "my-%s.el" pkg) 'ignore-if-missing)))
    (dolist (pkg packages-custom-order)
      (my-load (format "my-%s.el" pkg) 'ignore-if-missing))

    (my-load my-private 'ignore-if-missing)))

(my-load-all)

(unless window-system
  (recentf-open-files))
