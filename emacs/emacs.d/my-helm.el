(require 'helm-config)
(helm-mode 1)

(setq helm-ff-skip-boring-files t)
;; TODO: these don't work. Looks like having the '.' and '..' on top
;; of the list is by design.
(push "\\.\\.$" helm-boring-file-regexp-list)
(push "\\.$" helm-boring-file-regexp-list)
