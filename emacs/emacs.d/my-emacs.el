(defvar my-packages
  '(
    autopair
    bm
    browse-kill-ring
    coffee-mode
    elisp-slime-nav
    flycheck
    go-mode
    swiper
    macrostep
    magit
    org
    projectile
    rainbow-mode
    ruby-end
    smart-mode-line
    solarized-theme
    visual-regexp
    yasnippet
    ))

(require 'dash)

(defun my-load-all ()
  (interactive)
  (let ((default-directory "~/my/dotfiles/emacs/emacs.d")
	(my-files
         '(
           "my-fns"
           "my-env"
           "my-keys"
           "my-win"
           "my-view"
           "my-sublime"
           "my-shell"
           "my-occur"
           "my-isearch"
           "my-help"
           "my-dired"
           ))
        (my-non-packages
         ;; Files that aren't on MELPA or any other
         ;; package archive.
         (directory-files "~/.emacs.d/third-party" 'full "\\.el$" t))
        (my-private "~/.emacs.private.el"))

    ;; Install missing packages
    (-when-let (missing (-remove #'package-installed-p my-packages))
      (package-refresh-contents)
      (mapc 'package-install missing))
    (mapc 'my-load my-non-packages)
    ;; Load all my files: main reason to load this
    ;; before package customizations: they might
    ;; depend on functions in my-fns.el (and other
    ;; dependencies like that).
    (mapc 'my-load my-files)

    (dolist (package my-packages)
      (my-load (format "my-%s.el" package) 'ignore-if-missing))
    (my-load my-private 'ignore-if-missing)))

(my-load-all)
