(require 'diminish)

;; To see which variable needs to be added below:
;; (progn (describe-variable 'minor-mode-alist) (switch-to-buffer-other-window "*Help*"))

(let ((modes
       '(yas-minor-mode
         anzu-mode
         auto-fill-function
         helm-mode
         elisp-slime-nav-mode
         org-indent-mode
         dired-omit-mode
         ruby-end-mode
         region-bindings-mode
         which-key-mode
         auto-revert-mode)))
  (mapc 'diminish modes))
