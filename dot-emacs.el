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
(setq inhibit-startup-echo-area-message "sri")
(setq initial-scratch-message nil)
(setq visible-bell nil)
(setq ring-bell-function (lambda ()))
(setq disabled-command-hook nil)
(setq kill-whole-line t)

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
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key (kbd "C-o") 'ffap)
;;(global-set-key (kbd "C-n") ')
(global-set-key (kbd "C-p") 'shell)
(global-set-key (kbd "C-q") 'magit-status) ;; was quoted-insert
(global-set-key (kbd "C-r") 'isearch-forward)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-t") 'dabbrev-expand)
(global-set-key (kbd "C-v") 'clipboard-yank)
(global-set-key (kbd "C-w") 'other-window)
(global-set-key (kbd "C-z") 'undo)


(global-set-key (kbd "<M-down>") 'scroll-up)
(global-set-key (kbd "<M-up>") 'scroll-down)

(global-set-key (kbd "C-x k") 'my-kill-current-buffer)

;; Selection


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'hippie-exp)

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
  (comint-send-input))

(add-hook 'shell-mode-hook
          (lambda ()
            (setq line-number-mode nil)
            (set (make-variable-buffer-local
                  'show-trailing-whitespace)
                 nil)
            (define-key shell-mode-map (kbd "C-c e")
              'my-shell-erase-buffer)
            (define-key shell-mode-map (kbd "<right>")
              'my-shell-forward-char-or-previous-history)
            (define-key shell-mode-map (kbd "<down>")
              'my-shell-next-line-or-next-history)))

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
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
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

(message "")
