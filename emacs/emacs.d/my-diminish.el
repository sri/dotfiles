(require 'diminish)

(let ((modes
       '(yas-minor-mode
         anzu-mode
         auto-fill-function
         helm-mode
         elisp-slime-nav-mode
         org-indent-mode
         dired-omit-mode
         region-bindings-mode
         auto-revert-mode)))
  (mapc 'diminish modes))
