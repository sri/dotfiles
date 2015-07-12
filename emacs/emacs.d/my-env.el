(defun my-set-major-mode ()
  "For temporary buffers, set the mode based on the name.
Defaults to text mode. Yasnippets won't be turned on for
Fundamental mode."
  (and (null (buffer-file-name))
       (eq major-mode 'fundamental-mode)
       (let ((mode (assoc-default (buffer-name)
                                  auto-mode-alist 'string-match)))
         (if (and mode (not (consp mode)))
             (funcall mode)
           (text-mode))
         t)))

(setq-default major-mode 'my-set-major-mode)

(let ((registers '((?d . "~/Desktop")
                   (?e . "~/my/dotfiles/emacs/emacs.d")
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
                        diff-mode-hook
                        emacs-lisp-mode-hook)))
  (dolist (hook trailing-whitespace-mode-hooks)
    (add-hook hook 'my-show-trailing-whitespace)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq message-log 16384)
(visual-line-mode 1)
(setq save-interprogram-paste-before-kill t)
(setq highlight-nonselected-windows t)
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
(setq mouse-yank-at-point t)
(setq sentence-end-double-space nil)
;; Help char is `?' so C-x ? will list all the
;; keys bound to C-x.
(setq help-char ??)
(setq scroll-error-top-bottom t)

(setq Man-width 80)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)
(setq enable-recursive-minibuffers t)

(put 'erase-buffer 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(winner-mode 1)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(line-number-mode t)
(column-number-mode t)
(blink-cursor-mode -1)
(auto-compression-mode t)
(transient-mark-mode 1)
(show-paren-mode t)
(server-start)

(global-linum-mode 1)
(setq linum-format
      (if window-system "%d" "%d "))

(setq diff-switches '("-u"))

(make-variable-buffer-local 'line-number-mode)
(make-variable-buffer-local 'column-number-mode)

(require 'hippie-exp)

(setq hippie-expand-try-functions-list
      '(
        yas-hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

(global-font-lock-mode t)

(add-hook 'focus-out-hook
          (lambda ()
            (when (and buffer-file-name
                       (buffer-modified-p))
              (save-buffer))))

;; Eval expr:
(require 'eval-expr)

(eval-expr-install)
(setq eval-expr-print-function 'pp
      eval-expr-print-level nil
      eval-expr-print-length nil)
