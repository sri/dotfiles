(defun my-set-major-mode ()
  "For temporary buffers, set the mode based on the name.
Defaults to text mode. Yasnippets won't be turned on for
Fundamental mode."
  (when (and (eq major-mode 'fundamental-mode)
             (null (buffer-file-name)))
    (let ((mode (assoc-default (buffer-name)
                               auto-mode-alist 'string-match)))
      (if (and mode
               (not (consp mode)))
          (funcall mode)
        (text-mode))
      t)))

(setq-default major-mode 'my-set-major-mode)

(let ((registers '((?d . "~/Desktop")
                   (?e . "~/my/dotfiles/emacs/emacs.d")
                   (?~ . "~"))))
  (dolist (reg registers)
    (set-register (car reg) (cons 'file (cdr reg)))))

(require 'whitespace)
;; For some reason tabs don't work, but tab-mark does...
(setq whitespace-style
      '(face tabs trailing lines-tail space-before-tab newline indentation empty space-after-tab tab-mark))
(setq whitespace-line-column 78)

(mapc (lambda (hook)
        (add-hook hook (lambda () (whitespace-mode 1))))
      '(ruby-mode-hook
        python-mode-hook
        c-mode-hook
        c++-mode-hook
        js-mode-hook
        java-mode-hook
        diff-mode-hook
        web-mode-hook
        emacs-lisp-mode-hook))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq large-file-warning-threshold nil)
(set-language-environment "UTF-8")
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
(setq js-indent-level 2)

(setq Man-width 80)

(defun my-delete-trailing-whitespace ()
  (unless (memq major-mode '(org-mode))
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'my-delete-trailing-whitespace)

(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)
(setq enable-recursive-minibuffers t)

(put 'erase-buffer 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(winner-mode 1)
(if (and (fboundp 'menu-bar-mode)
         (null window-system))
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(line-number-mode t)
(column-number-mode t)
(blink-cursor-mode -1)
(auto-compression-mode t)
(transient-mark-mode 1)
(show-paren-mode t)
(server-start)

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
(setq ido-create-new-buffer 'always)

(global-linum-mode 1)
(setq linum-format
      (if window-system
          'dynamic
        ;; Just like the existing dynamic formatting
        ;; but add a space at the end of the number.
        (lambda (line)
          (let* ((nlines (count-lines (point-min) (point-max)))
                 (width (length (number-to-string nlines)))
                 (fmt (concat "%" (number-to-string width) "d ")))
             (propertize (format fmt line) 'face 'linum)))))


(setq diff-switches '("-u"))

(make-variable-buffer-local 'line-number-mode)
(make-variable-buffer-local 'column-number-mode)

(global-font-lock-mode t)



(require 'hippie-exp)

(setq hippie-expand-try-functions-list
      '(
        yas-hippie-try-expand
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Ediff:
(require 'ediff)

(setq ediff-diff-options "-w")
(setq ediff-highlight-all-diffs nil)
(setq ediff-show-clashes-only t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-combination-pattern
      '("<<<<<<< A: HEAD" A
        "||||||| Ancestor" Ancestor
        "=======" B ">>>>>>> B: Incoming"))

;; Eval expr:
(require 'eval-expr)

(eval-expr-install)
(setq eval-expr-print-function 'pp
      eval-expr-print-level nil
      eval-expr-print-length nil)

;; Recent files:
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 100)

(unless window-system
  (load-theme 'tango-dark))
