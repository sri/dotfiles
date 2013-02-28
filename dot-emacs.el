(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

; (require 'filladapt)

(set-register ?e '(file . "~/.emacs"))

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

(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(line-number-mode t)
(column-number-mode -1)
(blink-cursor-mode -1)
(auto-compression-mode t)
(transient-mark-mode 1)
(show-paren-mode t)

(require 'dired-x)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map [left] 'dired-up-directory)
            (define-key dired-mode-map [right] 'dired-find-file)))


;(require 'helm-config)
;(helm-mode 1)

(defun my-kill-line-or-region (&optional arg)
  (interactive "P")
  (if (region-active-p)
      (kill-region (point) (mark))
    (kill-line arg)))

;; Key bindings

;; Use arrows & PageDown/PageUp for navigation

(global-set-key (kbd "C-b") 'backward-kill-word)
(global-set-key (kbd "C-d") 'kill-word)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-j") 'other-window)
(global-set-key (kbd "C-k") 'my-kill-line-or-region)
(global-set-key (kbd "C-o") 'ffap)
(global-set-key (kbd "C-n") 'dabbrev-expand)
(global-set-key (kbd "C-p") 'shell)
(global-set-key (kbd "C-q") 'magit-status) ;; was quoted-insert
(global-set-key (kbd "C-r") 'isearch-forward)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-t") 'dabbrev-expand)
(global-set-key (kbd "C-v") 'clipboard-yank)
(global-set-key (kbd "C-w") 'other-window)
(global-set-key (kbd "C-z") 'undo)

;; Selection


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'my-kill-current-buffer)

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
            (set (make-variable-buffer-local
                  'show-trailing-whitespace)
                 nil)
            (define-key shell-mode-map (kbd "C-c e") 'my-shell-erase-buffer)
            (define-key shell-mode-map (kbd "<right>") 'my-shell-forward-char-or-previous-history)
            (define-key shell-mode-map (kbd "<down>") 'my-shell-next-line-or-next-history)))

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

(when window-system
  (require 'package)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  (load-theme 'solarized-dark t))

;; Some Sublime Text-isms:

(define-key isearch-mode-map (kbd "<return>")
  'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<S-return>")
  'isearch-repeat-backward)

(require 'advice)

(defadvice mouse-drag-region (after my-sublime-like-mouse-select (start-event))
  (when (= (event-click-count start-event) 2)
    (let ((isearch-word t)
          (isearch-forward t)
          (beg (min (mark) (point)))
          (string (buffer-substring-no-properties (mark) (point))))
      (unless (string-match "^\n?$" string)
	(deactivate-mark)
	(save-excursion
	  (call-interactively 'isearch-forward)
	  (goto-char beg)
	  (isearch-yank-string string))))))

(ad-activate 'mouse-drag-region)

(message "")
