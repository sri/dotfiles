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
