;; -*- lexical-binding: t; -*-
(defun my/set-major-mode (&optional name)
  "For temporary buffers, set the mode based on the name.
Defaults to text mode. Yasnippets won't be turned on for
Fundamental mode."
  (when (and (eq major-mode 'fundamental-mode)
             (null (buffer-file-name)))
    (let ((mode (assoc-default (or name (buffer-name))
                               auto-mode-alist 'string-match)))
      (if (and mode
               (not (consp mode)))
          (funcall mode)
        (text-mode))
    t)))

(add-to-list 'auto-mode-alist '("\\.gem\\'" . tar-mode))

(defun my/view-sqlite-file ()
  (require 'sqlite-mode)
  (sqlite-mode-open-file
   (prog1 buffer-file-name
     (kill-current-buffer))))

(add-to-list 'magic-mode-alist '("SQLite format 3\x00" . my/view-sqlite-file))

(setq-default major-mode 'my/set-major-mode)

(put 'upcase-region 'disabled nil)

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

(add-hook 'prog-mode-hook 'hs-minor-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq-default bidi-display-reordering nil)
(setq auto-hscroll-mode t)
(setq text-quoting-style 'straight) ; text quoting in help & messages
(put 'narrow-to-region 'disabled nil)

;; On Mac, when in fullscreen, I keep accidently running into this;
;; when you drag the region, it switches the Desktop.
;; When this is set to 'shift (or maybe t), then handle-switch-frame
;; gets invoked
(setq mouse-drag-and-drop-region nil)

(setq large-file-warning-threshold 1000000000) ; 1GB
(set-language-environment "UTF-8")
(setq messages-buffer-max-lines t)
(visual-line-mode 1)
(setq save-interprogram-paste-before-kill t)

;; Problem: when this is t, splitting a buffer into 2 windows, and
;; clicking into the windows causes weird region UI highlighting
(setq highlight-nonselected-windows nil)

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
(setq confirm-kill-emacs 'yes-or-no-p)
(setq use-dialog-box nil)

(setq fill-column 80)

(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

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

(context-menu-mode 1)

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
(electric-pair-mode)

(server-start)

(setq mode-line-collapse-minor-modes t)

(add-to-list 'display-buffer-alist
             '("^\\*shell\\*" . (display-buffer-same-window)))
(add-to-list 'display-buffer-alist
             '("^\\*Embark Export:.*" display-buffer-at-bottom))


(setq diff-switches '("-u"))

(make-variable-buffer-local 'line-number-mode)
(make-variable-buffer-local 'column-number-mode)

(global-font-lock-mode 1)


(setq-default which-function-mode nil)
(add-hook 'prog-mode-hook (lambda ()
                            (setq which-function-mode t)))

;; Calendar
(add-hook 'calendar-mode-hook
          (lambda ()
            (define-key calendar-mode-map (kbd "[") 'calendar-backward-year)
            (define-key calendar-mode-map (kbd "]") 'calendar-forward-year)
            (define-key calendar-mode-map (kbd "n") 'calendar-scroll-left-three-months)
            (define-key calendar-mode-map (kbd "p") 'calendar-scroll-right-three-months)))

;; Better completions
;; consult - previews, grouping, narrowing,
;; vertico,
;;  marginalia
(require 'orderless)
(setq completion-styles '(orderless basic substring initials flex))
(setq completion-category-overrides
      '((file (styles basic partial-completion))))
(setq completion-category-defaults nil)

(require 'vertico)
(vertico-mode 1)
(setq vertico-cycle t)

(require 'vertico-directory)
(require 'vertico-quick)
(require 'vertico-posframe)
(vertico-posframe-mode 1)
;; (vertico-multiform-mode -1)

;; (setq vertico-multiform-commands
;;       '((consult-grep buffer indexed)
;;         (consult-ripgrep buffer indexed)))

;; (setq vertico-multiform-categories
;;       '((consult-grep buffer)
;;         (consult-ripgrep buffer)))


(require 'marginalia)
(marginalia-mode 1)

(require 'consult)

(require 'savehist)
(savehist-mode 1)
(setq savehist-autosave-interval 60)

(require 'saveplace)
(save-place-mode 1)

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(defun my/enable-override-global-mode () (override-global-mode 1))
(defun my/disable-override-global-mode () (override-global-mode -1))

(add-hook 'minibuffer-setup-hook 'my/disable-override-global-mode)
(add-hook 'minibuffer-exit-hook 'my/enable-override-global-mode)

(require 'project)
(setq-default project-mode-line t)
(setq project-vc-merge-submodules nil)

(require 'grep)
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))
(require 'wgrep)
(setq wgrep-enable-key "r")

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
(setq recentf-max-saved-items 100)


;; IBuffer
(require 'ibuffer)
(require 'ibuf-ext)

;; Try by buffer by "project"
(define-ibuffer-filter buffer
    "Limit to exact buffer."
  (:description "buffer object"
   :reader nil)
  (eq buf qualifier))
;;(defun override-global-mode (arg) nil)
;; (setq ibuffer-saved-filter-groups
;;    '(("default"
;;       ("Magit"
;;        (or
;;         (mode . magit-status-mode)
;;         (mode . magit-log-mode)
;;         (name . "\\*magit")
;;         (name . "magit-")
;;         (name . "git-monitor")))
;;       ("Dired"
;;        (mode . dired-mode))
;;       ("Shells"
;;        (or
;;         (mode . shell-mode)
;;         (mode . eshell-mode)
;;         (mode . term-mode)
;;         (mode . compilation-mode)))
;;       ("Org"
;;        (or
;;         (name . "^\\*Calendar\\*$")
;;         (name . "^\\*Org Agenda")
;;         (name . "^ \\*Agenda")
;;         (name . "^diary$")
;;         (mode . org-mode)))
;;       ("Lisp"
;;        (mode . emacs-lisp-mode))
;;       ("Emacs"
;;        (or
;;         (name . "^\\*scratch\\*$")
;;         (name . "^\\*Messages\\*$")
;;         (name . "^\\*\\(Customize\\|Help\\)")
;;         (name . "\\*\\(Echo\\|Minibuf\\)"))))))

(defun my/ibuffer-by-projects ()
  "Group by projects (using the git root directory)."
  (let ((groups '())
        (hash (make-hash-table :test #'equal)))
    (dolist (buf (buffer-list))
      (let ((dir (abbreviate-file-name (my/git-root buf))))
        (puthash dir
                 (cons buf (gethash dir hash))
                 hash)))
    (maphash (lambda (dir buffers)
               (let ((value (mapcar (lambda (buf) (cons 'buffer buf))
                                    buffers)))
                 (push (list dir (cons 'or value)) groups)))
             hash)
    (setq groups (sort groups :key (lambda (x) (length (cdadr x)))))
    `(("zdefault" ,@groups))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
            (setq ibuffer-hidden-filter-groups '("Default"))
            (setq ibuffer-saved-filter-groups (my/ibuffer-by-projects))
	    (ibuffer-switch-to-saved-filter-groups "zdefault")))
