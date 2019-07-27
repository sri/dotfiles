;; -*- mode: emacs-lisp -*-
(require 'cl)
(require 'subr-x)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 160 :family "Monaco" :foreground "#93a1a1"))))
 '(bm-face ((t (:background "#9e7b00" :foreground "#333333"))))
 '(comint-highlight-prompt ((t nil)))
 '(cursor ((t nil)))
 '(diff-refine-added ((t (:background "#22aa22" :foreground "black"))))
 '(diff-refine-removed ((t (:background "red" :foreground "black"))))
 '(isearch ((t (:background "light green" :foreground "#002b36" :weight normal))))
 '(magit-diff-added ((t (:foreground "#22aa22"))))
 '(magit-diff-added-highlight ((t (:foreground "#22aa22"))))
 '(magit-diff-context-highlight ((t nil)))
 '(magit-diff-file-heading ((t nil)))
 '(magit-diff-file-heading-highlight ((t nil)))
 '(magit-diff-hunk-heading ((t (:foreground "#2aa198" :background nil))))
 '(magit-diff-hunk-heading-highlight ((t nil)))
 '(magit-diff-removed ((t (:foreground "#aa2222"))))
 '(magit-diff-removed-highlight ((t (:foreground "#aa2222"))))
 '(magit-section-highlight ((t nil)))
 '(minibuffer-prompt ((t (:foreground "#888888"))))
 '(org-done ((t (:foreground "#003d4d" :strike-through t))))
 '(org-headline-done ((t (:foreground "#003d4d" :strike-through t :slant italic))))
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#5e6e6f" :height 1.1 :weight bold))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#839496" :height 1.0))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#839496" :height 1.0))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#839496" :height 1.0))))
 '(org-level-5 ((t (:inherit variable-pitch :foreground "#839496" :height 1.0))))
 '(org-level-6 ((t (:inherit variable-pitch :foreground "#839496" :height 1.0))))
 '(org-level-7 ((t (:inherit variable-pitch :foreground "#839496" :height 1.0))))
 '(org-level-8 ((t (:inherit variable-pitch :foreground "#839496" :height 1.0))))
 '(org-link ((t (:foreground "#839496"))))
 '(org-todo ((t (:foreground "#6c71c4" :weight bold))))
 '(region ((t (:background "#073642" :foreground nil))))
 '(show-paren-match ((t (:foreground "black" :background "#22aa22"))))
 '(show-paren-mismatch ((t (:foreground "black" :background "red"))))
 '(whitespace-line ((t (:underline t)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default))
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-velocity))
 '(package-selected-packages
   '(typescript-mode protobuf-mode s rjsx-mode ample-theme leuven-theme neotree use-package yaml-mode diminish anzu region-bindings-mode hydra multiple-cursors undo-tree powerline spacemacs-theme zenburn-theme org yasnippet web-mode visual-regexp solarized-theme smart-mode-line ruby-end rainbow-mode projectile org-bullets magit macrostep helm helm-ls-git go-mode flycheck elisp-slime-nav bm ace-jump-mode))
 '(typescript-indent-level 2))

;; Load the byte-compiled version of file.
(defun my/load (arg &optional ignore-if-missing)
  (let* ((source (expand-file-name
                  (if (string-suffix-p ".el" arg) arg (concat arg ".el"))))
         (compiled (concat source "c")))
    (cond ((file-exists-p source)
           (if (file-newer-than-file-p source compiled)
               (let ((byte-compile-verbose nil))
                 (if (null (byte-compile-file source))
                     (error "my/load: ERROR byte compiling file %s" source))))
           (load compiled nil t t))
          (t
           (if (file-exists-p compiled)
               (delete-file compiled))
           (if (null ignore-if-missing)
               (error "my/load: missing %s" source))))))


(defun my/load-all ()
  ;; Load packages and install them if necessary.
  (let* ((package--builtins '())
         (missing (remove-if 'package-installed-p package-selected-packages)))
    (when missing
      (package-refresh-contents)
      (mapc 'package-install missing)))

  ;; Load my files
  (let ((default-directory "~/my/dotfiles/emacs"))
    (my/load (if window-system "my-gui" "my-terminal"))
    ;; Files that aren't on MELPA or any other package archive.
    (mapc 'my/load
          (directory-files "third-party" 'full "\\.el$" t))

    (mapc 'my/load
          '(
            "my-fns"
            "my-register"
            "my-env"
            "my-keys"
            "my-view"
            "my-sublime"
            "my-vscode"
            "my-shell"
            "my-occur"
            "my-isearch"
            "my-help"
            "my-dired"
            "my-update-dot-emacs"
            "my-modeline"))

    (mapc (lambda (pkg)
            (my/load (format "my-%s" pkg) 'ignore-if-missing))
          (remove 'diminish package-selected-packages))

    (my/load "my-diminish")
    (my/load "~/.emacs.private.el" 'ignore-if-missing))

  (unless window-system (recentf-open-files)))

(my/load-all)
