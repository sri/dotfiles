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

(when window-system
  (let ((shell-path (shell-command-to-string "$SHELL -c 'echo -n $PATH'")))
    (setenv "PATH" shell-path)
    (setq exec-path (split-string shell-path path-separator))))

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

(when window-system
  (global-hl-line-mode 1))

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

;; Help mode:
(require 'help-mode)

(defun my-help-push-next-button ()
  (interactive)
  (forward-button 1 t)
  (push-button))

(define-key help-mode-map (kbd "b") 'help-go-back)
(define-key help-mode-map (kbd "f") 'help-go-forward)
(define-key help-mode-map (kbd "n") 'forward-button)
(define-key help-mode-map (kbd "p") 'backward-button)
(define-key help-mode-map (kbd "x") 'delete-window)
(define-key help-mode-map (kbd "g") 'my-help-push-next-button)

;; Occur:
(defun my-occur-mode-display-occurrence ()
  (interactive)
  (occur-mode-display-occurrence)
  (occur-next))

(define-key occur-mode-map (kbd "n")
  'my-occur-mode-display-occurrence)

;; Eval expr:
(require 'eval-expr)

(eval-expr-install)
(setq eval-expr-print-function 'pp
      eval-expr-print-level nil
      eval-expr-print-length nil)

;; Shell:
(setenv "PAGER" "cat")

(defun my-shell-forward-char-or-previous-history (&optional arg)
  (interactive "p")
  (if (eobp)
      (comint-previous-input arg)
    (forward-char arg)))

(defun my-shell-next-line-or-next-history (&optional arg)
  (interactive "p")
  (if (eobp)
      (comint-next-input arg)
    (next-line arg)))

(defun my-shell-erase-buffer ()
  (interactive)
  (erase-buffer)
  (comint-send-input))

(defvar my-shell-bash-esc-dot-counter 0)
(defvar my-shell-bash-esc-dot-last-insertion nil)

(defun my-shell-bash-esc-dot ()
  "Same as Esc-. in bash; insert previous command's last word."
  (interactive)
  (let* ((continue (eq last-command 'my-shell-bash-esc-dot))
         (count (if continue (1+ my-shell-bash-esc-dot-counter) 0))
         (cmd (comint-previous-input-string count))
         (last (if (string-match "\\([`'\"]\\)[^`'\"]+?\\1\\s-*$" cmd)
                   (match-string 0 cmd)
                 (car (last (split-string cmd " " t))))))
    (setq my-shell-bash-esc-dot-counter count)
    (when last
      (when continue
        (delete-region (point)
                       (save-excursion
                         (search-backward my-shell-bash-esc-dot-last-insertion
                                          (point-at-bol)))))
      (setq my-shell-bash-esc-dot-last-insertion last)
      (insert last))))

(defun my-shell-bash-clear-screen ()
  (interactive)
  (recenter-top-bottom 0))

(defun my-shell (&optional arg)
  "Create a new shell (with prefix arg) or switch to a shell buffer."
  (interactive "P")
  (if arg
      (shell (generate-new-buffer-name "*shell*"))
    (let (shells others)
      (dolist (buf (buffer-list))
        (when (eq (with-current-buffer buf major-mode) 'shell-mode)
          (if (string-match "^[*]shell[*]" (buffer-name buf))
              (push buf shells)
            (push buf others))))
      ;; Sort the shells named "*shell*", "*shell*<1>" by their names.
      (setq shells (sort shells (lambda (x y)
                                  (string-lessp (buffer-name x)
                                                (buffer-name y)))))
      ;; Sort the shells not named "*shell*" etc. by their
      ;; creation time.
      (setq others (sort others (lambda (x y)
                                  (< (with-current-buffer x
                                       my-shell-mode-created-at)
                                     (with-current-buffer y
                                       my-shell-mode-created-at)))))
      (cond ((and (null shells) (null others))
             (shell))
            ((eq major-mode 'shell-mode)
             (let ((cur (current-buffer)))
               (switch-to-buffer (if (string-match "^[*]shell[*]" (buffer-name))
                                     (or (cadr (memq cur shells))
                                         (car others)
                                         (car shells))
                                   (or (cadr (memq cur others))
                                       (car shells)
                                       (car others))))))
            (t (switch-to-buffer (or (car shells)
                                     (car others))))))))

(defvar my-shell-mode-created-at nil)
(make-variable-buffer-local 'my-shell-mode-created-at)

(add-hook 'shell-mode-hook
          (lambda ()
            (setq my-shell-mode-created-at (float-time))
            (linum-mode -1)
            (setq line-number-mode nil
                  column-number-mode nil)
            (setq comint-input-ignoredups t)
            (setq comint-scroll-to-bottom-on-input nil)
            (setq comint-scroll-show-maximum-output nil)
            (toggle-truncate-lines 1)
            (define-key shell-mode-map (kbd "C-<up>")
              'comint-previous-prompt)
            (define-key shell-mode-map (kbd "C-<down>")
              'comint-next-prompt)
            (define-key shell-mode-map (kbd "C-c e")
              'my-shell-erase-buffer)
            (define-key shell-mode-map (kbd "C-l")
              'my-shell-bash-clear-screen)
            (define-key shell-mode-map (kbd "<right>")
              'my-shell-forward-char-or-previous-history)
            (define-key shell-mode-map (kbd "<down>")
              'my-shell-next-line-or-next-history)
            (define-key shell-mode-map (kbd "M-.")
              'my-shell-bash-esc-dot)))

;; Dired:
(require 'dired-x)

(defun my-dired ()
  (interactive)
  (let ((file-name buffer-file-name))
    (dired default-directory)
    (when file-name
      (dired-goto-file file-name))))

(defun my-dired-find-file ()
  (interactive)
  (if (/= (line-beginning-position) 1)
      (dired-find-file)
    (let ((dir (expand-file-name default-directory)))
      (kill-new dir)
      (message "Copied: '%s'" dir))))

(defun my-dired-first-file ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line 1))

(defun my-dired-last-file ()
  (interactive)
  (goto-char (point-max))
  (dired-previous-line 1))

(defun my-dired-right-arrow-key ()
  (interactive)
  (if (or (let ((use-empty-active-region t))
            (use-region-p))
          (= (line-beginning-position) 1))
      (forward-char 1)
    (dired-find-file)))

(when (eq system-type 'darwin)
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "open -a Preview")
          ("\\.html?\\'" "open -a 'Google Chrome'"))))

(add-hook 'dired-mode-hook
          (lambda ()
            (linum-mode -1)
            (dired-omit-mode 1)
            (dired-hide-details-mode 1)
            (define-key dired-mode-map (kbd "D") 'dired-hide-details-mode)
            (setq dired-dwim-target t)
            (setq dired-omit-size-limit nil)
            (define-key dired-mode-map (kbd ",") 'dired-prev-dirline)
            (define-key dired-mode-map (kbd ".") 'dired-next-dirline)
            (define-key dired-mode-map [mouse-2] 'dired-find-file)
            (define-key dired-mode-map "a" 'my-dired-first-file)
            (define-key dired-mode-map "z" 'my-dired-last-file)
            (define-key dired-mode-map "f" 'my-isearch-forward)
            (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
            (define-key dired-mode-map (kbd "C-m") 'my-dired-find-file)
            (define-key dired-mode-map (kbd "SPC") 'scroll-up)
            (define-key dired-mode-map (kbd "S-SPC") 'scroll-down)
            (define-key dired-mode-map [left] 'dired-up-directory)
            (define-key dired-mode-map [right] 'my-dired-right-arrow-key)))

;; View:
(require 'view)

(defun my-view-scroll-down-one-line ()
  (interactive)
  (scroll-down 1))

(defun my-view-scroll-up-one-line ()
  (interactive)
  (scroll-up 1))

(defun my-view-top-of-window ()
  (interactive)
  (recenter 0))

(defun my-view-center-in-window ()
  (interactive)
  (recenter))

(defun my-view-botton-of-window ()
  (interactive)
  (recenter -1))

(define-key view-mode-map (kbd "SPC") 'View-scroll-page-forward)
(define-key view-mode-map (kbd "j") 'View-scroll-page-forward)
(define-key view-mode-map (kbd "S-SPC") 'View-scroll-page-backward)
(define-key view-mode-map (kbd "k") 'View-scroll-page-backward)
(define-key view-mode-map "q" 'View-exit-and-edit)
(define-key view-mode-map (kbd "a") 'beginning-of-buffer)
(define-key view-mode-map (kbd "z") 'end-of-buffer)
(define-key view-mode-map (kbd "f") 'my-isearch-forward)
