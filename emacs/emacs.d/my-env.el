;(require 'filladapt)
;(setq filladapt-mode-line-string nil)
;(setq-default filladapt-mode t)

(defun my-set-major-mode ()
  "For temporary buffers, set the mode based on the name."
  (and (null (buffer-file-name))
       (eq major-mode 'fundamental-mode)
       (let ((mode (assoc-default (buffer-name)
                                  auto-mode-alist 'string-match)))
         (when (and mode (not (consp mode)))
           (funcall mode)
           t))))

(setq-default major-mode 'my-set-major-mode)

;; Without the -i, shell-command-to-string has errors in its output:
;; "bash: cannot set terminal process group (-1): Invalid argument",
;; and "bash: no job control in this shell"
(setq shell-command-switch "-ic")

(when window-system
  (let ((shell-path (shell-command-to-string "$SHELL -i -c 'echo -n $PATH'")))
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
                        diff-mode-hook
                        emacs-lisp-mode-hook)))
  (dolist (hook trailing-whitespace-mode-hooks)
    (add-hook hook 'my-show-trailing-whitespace)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(recentf-mode 1)
(add-hook 'emacs-startup-hook 'recentf-open-files)

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
;; Help char is `?' so C-x ? will list all the
;; keys bound to C-x.
(setq help-char ??)

(setq Man-width 80)

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
(when window-system
  (global-hl-line-mode 1))

(setq diff-switches '("-u"))

(make-variable-buffer-local 'line-number-mode)
(make-variable-buffer-local 'column-number-mode)

;; (ido-mode 1)
;; (setq ido-enable-flex-matching t)
;; (setq ido-default-file-method 'selected-window)
;; (setq ido-default-buffer-method 'selected-window)
;; (setq ido-create-new-buffer 'always)

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