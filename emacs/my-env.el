(defun my/set-major-mode ()
  "For temporary buffers, set the mode based on the name.
Defaults to text mode. Yasnippets won't be turned on for
Fundamental mode."
  (when (and (eq major-mode 'fundamental-mode)
             (null (buffer-file-name)))
    (set-auto-mode)
    t))

(setq-default major-mode 'my/set-major-mode)


(require 'whitespace)
;; For some reason tabs don't work, but tab-mark does...
(setq whitespace-style
      '(face tabs trailing space-before-tab newline indentation empty space-after-tab tab-mark))
(setq whitespace-line-column 78)

(defun my/turn-on-whitespace ()
  (whitespace-mode 1))
(add-hook 'prog-mode-hook 'my/turn-on-whitespace)
(add-hook 'diff-mode-hook 'my/turn-on-whitespace)

(defun my/turn-off-whitespace ()
  (whitespace-mode -1))
(add-hook 'emacs-lisp-mode-hook 'my/turn-off-whitespace)
(add-hook 'go-mode-hook 'my/turn-off-whitespace)

(defun my/turn-on-linenumbers ()
  (setq display-line-numbers t))
(add-hook 'prog-mode-hook 'my/turn-on-linenumbers)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq-default bidi-display-reordering nil)
(setq auto-hscroll-mode t)
(setq text-quoting-style 'straight) ; text quoting in help & messages
(put 'narrow-to-region 'disabled nil)
(setq mouse-drag-and-drop-region 'shift)
(setq large-file-warning-threshold 1000000000) ; 1GB
(set-language-environment "UTF-8")
(setq messages-buffer-max-lines t)
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
(setq initial-scratch-message nil)
(setq visible-bell nil)
(setq ring-bell-function (lambda ()))
(setq disabled-command-hook nil)
(setq kill-ring-max 1000)
(setq kill-whole-line t)
(setq kill-read-only-ok t)
(setq mouse-yank-at-point t)
(setq sentence-end-double-space nil)
;; Help char is `?' so C-x ? will list all the
;; keys bound to C-x.
(setq help-char ??)
(setq scroll-error-top-bottom t)
(setq js-indent-level 2)
(global-eldoc-mode -1)
(global-hl-line-mode 1)
(setq mode-line-compact t)

(setq fill-column 80)

;; when running make, set NATIVE_FULL_AOT=1 to
;; native compile all libs
;(setq package-native-compile t)

(with-demoted-errors "error loading tab-bar"
  (require 'tab-bar)
  (setq tab-bar-show 1)
  (setq tab-bar-tab-name-truncated-max 12)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-truncated))

;; Ignore accidentally hitting the trackpad while typing and having it
;; pop up a menu.
(let ((mouse-keys-to-ignore
       '("<C-down-mouse-3>"
         "<C-down-mouse-1>"
         "<C-mouse-1>")))
  (dolist (key mouse-keys-to-ignore)
    (global-set-key (kbd key) (lambda () (interactive)))))

(setq Man-width 80)

(require 'undo-tree)
(setq undo-tree-visualizer-diff t)
(setq undo-tree-visualizer-timestamps t)

(defun my/delete-trailing-whitespace ()
  (unless (memq major-mode '(org-mode))
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'my/delete-trailing-whitespace)

(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)
(setq enable-recursive-minibuffers t)

(put 'erase-buffer 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(winner-mode 1)
(if (and (fboundp 'menu-bar-mode)
         (memq window-system '(nil x)))
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(line-number-mode t)
(column-number-mode t)
;; When `column-number-indicator-zero-based' is nil,
;; modeline internally uses 1-based indexing.
;; If you mess with modeline (like I've done), the
;; you'll need to use "%C".
(setq-default column-number-indicator-zero-based nil)
(blink-cursor-mode -1)
(auto-compression-mode t)
(transient-mark-mode 1)
(show-paren-mode t)
(server-start)
(electric-pair-mode)

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
(setq ido-create-new-buffer 'always)

(setq diff-switches '("-u"))

(make-variable-buffer-local 'line-number-mode)
(make-variable-buffer-local 'column-number-mode)

(global-font-lock-mode -1)
(which-function-mode 1)

;; Calendar
(add-hook 'calendar-mode-hook
          (lambda ()
            (define-key calendar-mode-map (kbd "[") 'calendar-backward-year)
            (define-key calendar-mode-map (kbd "]") 'calendar-forward-year)
            (define-key calendar-mode-map (kbd "n") 'calendar-scroll-left-three-months)
            (define-key calendar-mode-map (kbd "p") 'calendar-scroll-right-three-months)))


(require 'hippie-exp)
(setq hippie-expand-try-functions-list
      '(
        yas-hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
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
