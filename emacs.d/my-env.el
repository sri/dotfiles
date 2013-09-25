;(require 'filladapt)
;(setq filladapt-mode-line-string nil)
;(setq-default filladapt-mode t)

(set-register ?d '(file . "~/Desktop"))
(set-register ?e '(file . "~/my/dotfiles/emacs.d"))
(set-register ?~ '(file . "~"))

(menu-bar-mode -1)

(defun my-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(let ((trailing-whitespace-mode-hooks
       '(ruby-mode-hook python-mode-hook
                        c-mode-hook
                        c++-mode-hook
                        js-mode-hook
                        java-mode-hook
                        emacs-lisp-mode-hook)))
  (dolist (hook trailing-whitespace-mode-hooks)
    (add-hook hook 'my-show-trailing-whitespace)))

; (setq-default visual-line-mode t)
(setq mouse-drag-copy-region t)
(setq-default indent-tabs-mode nil)
(setq make-backup-files nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "sri")
(setq initial-scratch-message nil)
(setq visible-bell nil)
(setq ring-bell-function (lambda ()))
(setq disabled-command-hook nil)
(setq kill-whole-line t)
;; Help char is `?' so C-x ? will list all the
;; keys bound to C-x.
(setq help-char ??)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

(put 'erase-buffer 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(cua-selection-mode 1)
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode t)
(column-number-mode t)
(blink-cursor-mode -1)
(auto-compression-mode t)
(transient-mark-mode 1)
(show-paren-mode t)
(ido-mode 1)
;(auto-revert-mode 1)
(global-hl-line-mode 1)
(electric-pair-mode 1)
;(outline-minor-mode 1)

(desktop-save-mode 1)
(add-to-list 'desktop-path "~/.emacs.d")

(make-variable-buffer-local
 'line-number-mode)

(require 'hippie-exp)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

(add-hook 'view-mode-hook
          (lambda ()
            (define-key view-mode-map (kbd "<up>") 'scroll-down)
            (define-key view-mode-map (kbd "<down>") 'scroll-up)))

(global-font-lock-mode t)

(setq linum-format " %d ")
