(require 'helm-config)
(helm-mode 1)

(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(setq helm-M-x-fuzzy-match t)

;; (setq helm-ff-skip-boring-files t)
;; ;; TODO: these don't work. Looks like having the '.' and '..' on top
;; ;; of the list is by design.
;; (push "\\.\\.$" helm-boring-file-regexp-list)
;; (push "\\.$" helm-boring-file-regexp-list)
