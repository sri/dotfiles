(require 'helm-config)
(helm-mode 1)

(define-key helm-map (kbd "<up>") 'previous-history-element)
(define-key helm-map (kbd "<down>") 'next-history-element)

(setq helm-ff-skip-boring-files t)
;; TODO: these don't work. Looks like having the '.' and '..' on top
;; of the list is by design.
(push "\\.\\.$" helm-boring-file-regexp-list)
(push "\\.$" helm-boring-file-regexp-list)
