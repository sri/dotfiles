(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;(require 'filladapt)

(setq filladapt-mode-line-string nil)
(setq-default filladapt-mode t)

(set-register ?e '(file . "~/.emacs"))

; (setq-default visual-line-mode t)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq make-backup-files nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "sthaiyar")
(setq initial-scratch-message nil)
(setq visible-bell nil)
(setq ring-bell-function (lambda ()))
(setq disabled-command-hook nil)
(setq kill-whole-line t)
;; Help char is `?' so C-x ? will list all the
;; keys bound to C-x.
(setq help-char ??)


(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;; (setq mode-line-format
;;       '("" mode-line-modified
;;      mode-line-frame-identification
;;      mode-line-buffer-identification
;;      mode-line-position
;;      (vc-mode vc-mode)
;;      mode-line-modes
;;      ))

(put 'erase-buffer 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode 1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(line-number-mode t)
(column-number-mode -1)
(blink-cursor-mode -1)
(auto-compression-mode t)
(transient-mark-mode 1)
(show-paren-mode t)
(ido-mode 1)
(auto-revert-mode 1)
(global-hl-line-mode 1)
(desktop-save-mode 1)

(require 'dired-x)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map [left] 'dired-up-directory)
            (define-key dired-mode-map [right] 'dired-find-file)))

(defun my-kill-line-or-region (&optional arg)
  (interactive "P")
  (if (region-active-p)
      (kill-region (point) (mark))
    (kill-line arg)))

;; Key bindings

;; Use arrows & PageDown/PageUp for navigation

(global-set-key (kbd "C-b") 'backward-kill-word)
(global-set-key (kbd "C-d") 'kill-word)
(global-set-key (kbd "C-f") 'my-isearch-forward)
(global-set-key (kbd "C-i") 'my-hippie-tab)
(global-set-key (kbd "C-j") 'other-window)
(global-set-key (kbd "C-k") 'my-kill-line-or-region)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-o") 'ffap)
(global-set-key (kbd "C-n") 'execute-extended-command)
(global-set-key (kbd "C-p") 'shell)
;;****(global-set-key (kbd "C-q") 'magit-status) ;; was quoted-insert
(global-set-key (kbd "C-r") 'isearch-forward)
(global-set-key (kbd "C-s") 'save-buffer)
;;****(global-set-key (kbd "C-t") 'dabbrev-expand)
(global-set-key (kbd "C-v") 'clipboard-yank)
;;****(global-set-key (kbd "C-w") 'other-window)
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-x C-q") 'quoted-insert) ; was toggle-read-only


(global-set-key (kbd "<M-down>") 'scroll-up)
(global-set-key (kbd "<M-up>") 'scroll-down)

(global-set-key (kbd "C-x k") 'my-kill-current-buffer)

(global-set-key (kbd "<f5>") 'magit-status)

(global-set-key (kbd "<f6>") 'find-tag)
(global-set-key (kbd "<S-f6>") 'my-find-tag-next)
(global-set-key (kbd "<f7>") 'pop-tag-mark)

(global-set-key (kbd "s-J")
                'my-sublime-expand-selection-to-indentation)
(global-set-key (kbd "M-J")
                'my-sublime-expand-selection-to-indentation)

;; Selection


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'hippie-exp)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

(defun my-hippie-tab (arg)
  (interactive "*P")
  (cond ((and transient-mark-mode (region-active-p))
         (indent-region (region-beginning) (region-end) nil))
        ((and (eq (char-syntax (preceding-char)) ?w)
              (not (zerop (current-column))))
         (hippie-expand arg))
        (t
         (indent-for-tab-command))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-find-tag-next ()
  (interactive)
  (find-tag nil t nil))

(defun my-kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

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

(make-variable-buffer-local
 'line-number-mode)

(defun my-shell-erase-buffer ()
  (interactive)
  (erase-buffer)
  (comint-next-input t))

(add-hook 'shell-mode-hook
          (lambda ()
            (setq line-number-mode nil)
            (setq comint-input-sender
                  'my-emacs-rspec-command)
            (set (make-variable-buffer-local
                  'show-trailing-whitespace)
                 nil)
            (define-key shell-mode-map (kbd "C-c e")
              'my-shell-erase-buffer)
            (define-key shell-mode-map (kbd "<right>")
              'my-shell-forward-char-or-previous-history)
            (define-key shell-mode-map (kbd "<down>")
              'my-shell-next-line-or-next-history)))

(add-hook 'view-mode-hook
          (lambda ()
            (define-key view-mode-map (kbd "<up>")
              'scroll-down)
            (define-key view-mode-map (kbd "<down>")
              'scroll-up)))

(global-font-lock-mode -1)

(setq linum-format " %d ")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(unless window-system
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(isearch ((((class color) (min-colors 8)) (:background "cyan" :foreground "black"))))
   '(linum ((t (:inherit (shadow default) :foreground "gray" :width extra-expanded))))
   '(magit-item-highlight ((t nil)))
   '(minibuffer-prompt ((t (:foreground "black"))))
   '(region ((t (:background "cyan" :foreground "black"))))
   '(show-paren-match ((((class color) (background light)) (:background "cyan"))))
   '(trailing-whitespace ((((class color) (background light)) (:background "yellow"))))))

(defvar my-packages
  '(color-theme color-theme-solarized magit))

(when window-system
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)

  (let ((missing '()))
    (dolist (p my-packages)
      (unless (package-installed-p p)
        (push p missing)))
    (when missing
      (package-refresh-contents)
      (dolist (p missing)
        (package-install p))))

  (load-theme 'solarized-dark t))

(require 'magit)
(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)

;; Some Sublime Text-isms:

(defun my-sublime-like-mouse-dblclick-select-fn ()
  (let ((isearch-word t)
	(isearch-forward t)
	(beg (min (mark) (point)))
	(string (buffer-substring-no-properties (mark) (point))))
    (unless (string-match "^\n*$" string)
      (deactivate-mark)
      (save-excursion
	(call-interactively 'isearch-forward)
	(goto-char beg)
	(isearch-yank-string string)))))

(defun my-isearch-forward ()
  (interactive)
  (if (and transient-mark-mode (region-active-p))
      (my-sublime-like-mouse-dblclick-select-fn)
    (call-interactively 'isearch-forward)))

(setq isearch-allow-scroll t)

(define-key isearch-mode-map (kbd "<return>")
  'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<S-return>")
  'isearch-repeat-backward)

(setq isearch-lazy-highlight-initial-delay 0)

(require 'advice)

(defadvice mouse-drag-region (after my-sublime-like-mouse-select (start-event))
  (when (= (event-click-count start-event) 2)
    (my-sublime-like-mouse-dblclick-select-fn)))

(ad-activate 'mouse-drag-region)

(defun my-sublime-expand-selection-to-indentation ()
  (interactive)
  "Expand selection to the next indentation level.
Inspired by Sublime Text."
  (let ((n (current-indentation))
        (beg (point-at-bol))
        (end (point-at-eol)))
    ;; when region is active & transient mark mode is
    ;; turned on, we expand to make that region bigger
    (when (and (region-active-p) transient-mark-mode)
      (setq beg (region-beginning)
            end (region-end))
      (save-excursion
        ;; get the min indentation within the region
        (goto-char beg)
        (forward-line 1)
        (while (< (point) end)
          (setq n (min n (current-indentation)))
          (forward-line 1))
        ;; get the min indentation of line before
        ;; region start, line after region start or n
        (setq n
              (max (progn
                     (goto-char beg)
                     (forward-line -1)
                     (if (bobp) 0 (current-indentation)))
                   (progn
                     (goto-char end)
                     (forward-line 1)
                     (if (eobp) 0 (current-indentation)))))))
    ;; now expand the region
    (save-excursion
      (goto-char beg)
      (forward-line -1)
      (while (and (>= (current-indentation) n) (not (bobp)))
        (forward-line -1))
      (forward-line 1)
      (setq beg (point-at-bol))
      (goto-char end)
      (forward-line 1)
      (while (and (>= (current-indentation) n) (not (eobp)))
        (forward-line 1))
      (forward-line -1)
      (setq end (point-at-eol)))
    (goto-char beg)
    (set-mark beg)
    (goto-char end)))

;; Miscellaneous functions:

;; If there is a visible "rspec" (*_spec.rb) buffer in the
;; current frame, running a "rspec" command is a shell process,
;; will run rspec against that buffer's file name (at the line
;; where the cursor is in that buffer). Add this to your shell-mode-hook
;; to enable this feature:
;;     (add-hook 'shell-mode-hook
;;               (lambda ()
;;                 (setq comint-input-sender 'my-emacs-rspec-command)))
;;
(defun my-emacs-rspec-command (proc string)
  (when (string-match "^rspec\n?$" string)
    (let ((buffers (mapcar #'window-buffer (window-list)))
          (spec-buffer nil))
      (dolist (buf buffers)
        (when (string-match "_spec[.]rb$" (or (buffer-file-name buf) ""))
          (setq spec-buffer buf)))
      (when spec-buffer
        (let ((n (with-current-buffer spec-buffer
                   (line-number-at-pos))))
          (setq string (format "rspec %s:%d"
                               (buffer-file-name spec-buffer)
                               n))
          (message "Running \"%s\"" string)))))
  (comint-simple-send proc string))

(defun my-transpose-buffers (&optional arg)
  (interactive "p")
  (let* ((windows (window-list nil 'never-minibuffer))
         (selected (pop windows))
         (selected-buffer (window-buffer selected)))
    (when (< arg 0)
      (setq windows (reverse windows)))
    (dotimes (i (length windows))
      (switch-to-buffer (window-buffer (pop windows)))
      (other-window arg))
    (switch-to-buffer selected-buffer)
    (other-window arg)))

(message "")
