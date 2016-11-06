(require 'diminish)

;; To see which variable needs to be added below:
;; (progn (describe-variable 'minor-mode-alist) (switch-to-buffer-other-window "*Help*"))

(defun my-diminish-all ()
  (mapc 'diminish
        '(yas-minor-mode
          anzu-mode
          auto-fill-function
          helm-mode
          elisp-slime-nav-mode
          org-indent-mode
          ruby-end-mode
          region-bindings-mode
          which-key-mode
          auto-revert-mode)))

(add-hook 'after-init-hook 'my-diminish-all)
