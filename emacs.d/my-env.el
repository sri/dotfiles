;(require 'filladapt)
;(setq filladapt-mode-line-string nil)
;(setq-default filladapt-mode t)

(when window-system
  (let ((shell-path (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" shell-path)
    (setq exec-path (split-string shell-path path-separator))))

(let ((registers '((?d . "~/Desktop")
                   (?e . "~/my/dotfiles/emacs.d")
                   (?~ . "~"))))
  (dolist (reg registers)
    (set-register (car reg) (cons 'file (cdr reg)))))

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

(visual-line-mode 1)
(setq echo-keystrokes 0.1)
(setq vc-follow-symlinks t)
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
(setq kill-read-only-ok t)
;; Help char is `?' so C-x ? will list all the
;; keys bound to C-x.
(setq help-char ??)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

(put 'erase-buffer 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(winner-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode t)
(column-number-mode t)
(blink-cursor-mode -1)
(auto-compression-mode t)
(transient-mark-mode 1)
(show-paren-mode t)
(global-hl-line-mode 1)

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

(require 'hippie-exp)

(setq hippie-expand-try-functions-list
      '(
        yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

(global-font-lock-mode t)

(cond (window-system
       (server-start))
      (t
       (xterm-mouse-mode)))
