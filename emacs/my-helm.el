;; (require 'helm)
;; (require 'helm-config)
;; (helm-mode -1)

;; (bind-keys :map shell-mode-map
;;           ("C-c C-l" . helm-comint-input-ring))

;; (bind-keys :map minibuffer-local-map
;;            ("C-c C-l" . helm-minibuffer-history))

;; (setq helm-grep-save-buffer-name-no-confirm t)
;; (setq helm-M-x-fuzzy-match t)
;; (setq helm-buffers-fuzzy-matching t)
;; (setq helm-display-header-line nil)
;; (setq helm-display-source-at-screen-top nil)
;; (setq helm-move-to-line-cycle-in-source nil)
;; (setq helm-allow-mouse t)

;; (setq helm-echo-input-in-header-line t)
;; (add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)

;; (define-key helm-map (kbd "C-n") 'helm-next-source)
;; (define-key helm-map (kbd "C-p") 'helm-previous-source)
;; (define-key helm-map (kbd "C-b") 'backward-kill-word)
;; (define-key helm-map (kbd "C-z") 'undo)

;; (require 'helm-ls-git)

;; (setq helm-ls-git-show-abs-or-relative 'relative)

;; ;; (add-hook 'helm-before-initialize-hook
;; ;;           (lambda ()
;; ;;             (setq-default override-global-mode nil)))

;; ;; (add-hook 'helm-cleanup-hook
;; ;;           (lambda ()
;; ;;             (setq-default override-global-mode t)))

;; ;; (setq helm-ff-skip-boring-files t)
;; ;; ;; TODO: these don't work. Looks like having the '.' and '..' on top
;; ;; ;; of the list is by design.
;; ;; (push "\\.\\.$" helm-boring-file-regexp-list)
;; ;; (push "\\.$" helm-boring-file-regexp-list)
