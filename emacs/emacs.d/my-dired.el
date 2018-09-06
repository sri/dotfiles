(require 'dired-x)

(defun my/dired ()
  (interactive)
  (let ((file-name buffer-file-name))
    (dired default-directory)
    (when file-name
      (dired-goto-file file-name))))

(defun my/dired-find-file ()
  (interactive)
  (if (/= (line-beginning-position) 1)
      (dired-find-file)
    (let ((dir (expand-file-name default-directory)))
      (kill-new dir)
      (message "Copied: '%s'" dir))))

(defun my/dired-first-file ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line 1))

(defun my/dired-last-file ()
  (interactive)
  (goto-char (point-max))
  (dired-previous-line 1))

(defun my/dired-right-arrow-key ()
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
(defun my/recentf-add-dired-directory ()
  (when (and dired-directory
             (file-directory-p dired-directory)
             (not (string= dired-directory "/")))
    (recentf-add-file dired-directory)))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)
            (diminish 'dired-omit-mode)
            (dired-hide-details-mode 1)
            (setq dired-dwim-target t)
            (setq dired-omit-size-limit nil)
            (my/recentf-add-dired-directory)

            (bind-keys :map dired-mode-map
                       ("C-c C-d" . dired-hide-details-mode)
                       ("," . dired-prev-dirline)
                       ("." . dired-next-dirline)
                       ([mouse-2] . dired-find-file)
                       ("a" . my/dired-first-file)
                       ("z" . my/dired-last-file)
                       ("f" . find-name-dired)
                       ("r" . wdired-change-to-wdired-mode)
                       ("C-o" . my/ffap-or-find-file)
                       ("C-m" . my/dired-find-file)
                       ("SPC" . scroll-up)
                       ("S-SPC" . scroll-down)
                       ([left] . dired-up-directory)
                       ([right] . my/dired-right-arrow-key))))
