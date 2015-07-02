(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(let ((missing '()))
  (dolist (p my-packages)
    (unless (package-installed-p p)
      (push p missing)))
  (when missing
    (package-refresh-contents)
    (dolist (p missing)
      (package-install p))))

(when window-system
  (load-theme 'solarized-dark t)
  (custom-theme-set-faces
   'solarized-dark
   '(dired-header ((t (:foreground "#268bd2" :underline t :background nil)))))
  )

(require 'autopair)
(autopair-global-mode)

(require 'bm)
(setq bm-highlight-style
      (if window-system
          'bm-highlight-only-fringe
        'bm-highlight-only-line))

(require 'coffee-mode)
(setq coffee-tab-width 2)

(require 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
