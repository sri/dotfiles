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

(setq-default dired-listing-switches "-alh")

;; From jwiegley's dotfiles:
(defun my-recentf-add-dired-directory ()
  (when (and dired-directory
             (file-directory-p dired-directory)
             (not (string= dired-directory "/")))
    (recentf-add-file dired-directory)))

(add-hook 'dired-mode-hook
          (lambda ()
            (linum-mode -1)
            (dired-omit-mode 1)
            (dired-hide-details-mode 1)
            (define-key dired-mode-map (kbd "D") 'dired-hide-details-mode)
            (setq dired-dwim-target t)
            (setq dired-omit-size-limit nil)
            (my-recentf-add-dired-directory)
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
