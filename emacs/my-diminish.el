(require 'diminish)

(require 'ruby-end)

;; To see which variable needs to be added below:
;; (progn (describe-variable 'minor-mode-alist) (switch-to-buffer-other-window "*Help*"))

(defun my/diminish-all ()
  (mapc 'diminish
        '(yas-minor-mode
          anzu-mode
          ruby-end-mode
          auto-fill-function
          elisp-slime-nav-mode
          org-indent-mode
          whitespace-mode
          region-bindings-mode
          auto-revert-mode)))
(add-hook 'after-init-hook 'my/diminish-all)
