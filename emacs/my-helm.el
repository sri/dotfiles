(require 'helm-config)
(helm-mode 1)

(bind-keys :map shell-mode-map
          ("C-c C-l" . helm-comint-input-ring))

(bind-keys :map minibuffer-local-map
           ("C-c C-l" . helm-minibuffer-history))

(setq helm-grep-save-buffer-name-no-confirm t)
(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)

(require 'helm-ls-git)

(setq helm-ls-git-show-abs-or-relative 'relative)

;; (setq helm-ff-skip-boring-files t)
;; ;; TODO: these don't work. Looks like having the '.' and '..' on top
;; ;; of the list is by design.
;; (push "\\.\\.$" helm-boring-file-regexp-list)
;; (push "\\.$" helm-boring-file-regexp-list)
