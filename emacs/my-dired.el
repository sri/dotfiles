;; -*- lexical-binding: t; -*-
(require 'dired-x)

(when (executable-find "gls")
  (setq insert-directory-program (executable-find "gls")))
(setq dired-listing-switches "-alh")

(defun my/dired-sort-by-size ()
  (interactive)
  (dired-hide-details-mode -1)
  (let ((old dired-actual-switches))
    (dired-sort-other "-alhS")
    (setq dired-actual-switches old)))

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

(setq-default dired-listing-switches "-alhF")

;; From jwiegley's dotfiles:
(defun my/recentf-add-dired-directory ()
  (when (and (stringp dired-directory)
             dired-directory
             (file-directory-p dired-directory)
             (not (string= dired-directory "/")))
    (recentf-add-file dired-directory)))

(defun my/open-in-finder ()
  (interactive)
  (call-process "open" nil nil nil (expand-file-name default-directory)))

(defun my/dired-goto-git-root ()
  (interactive)
  (let ((root (my/git-repo-root)))
    (if (s-blank? root)
        (message "not in git repo")
      (dired root)
      (message "in git repo root"))))


(require 'casual-dired-sort-by)

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)
            ;(dired-hide-details-mode 0)
            (setq dired-dwim-target t)
            (setq dired-omit-size-limit nil)
            (setq dired-vc-rename-file t)
            (setq delete-by-moving-to-trash t)
            (setq wdired-allow-to-change-permissions t)
            (my/recentf-add-dired-directory)

            (bind-keys :map dired-mode-map
                       ("S" . my/dired-sort-by-size)
                       ("C-c C-d" . dired-hide-details-mode)
                       ("`" . my/dired-goto-git-root)
                       ("," . dired-prev-dirline)
                       ("." . dired-next-dirline)
                       ;([mouse-2] . dired-find-file)
                       ("a" . my/dired-first-file)
                       ("z" . my/dired-last-file)
                       ("f" . find-name-dired)
                       ("r" . wdired-change-to-wdired-mode)
                       ("C-o" . my/ffap-or-find-file)
                       ("C-m" . my/dired-find-file)
                       ("SPC" . scroll-up)
                       ("J"   . my/open-in-finder)
                       ("G"   . magit-status)
                       ("S-SPC" . scroll-down)
                       ("N" . dired-create-empty-file)
                       ([left] . dired-up-directory)
                       ([right] . my/dired-right-arrow-key)
                       ("s" . casual-dired-sort-by-tmenu)

                       ;; casual-dired
                       ;; ("o" . casual-dired-tmenu)
                       ;; ("s" . casual-dired-sort-by-tmenu)
                       ;; ("/" . casual-dired-search-replace-tmenu)

                       )))
