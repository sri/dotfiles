;; -*- lexical-binding: t; -*-
(require 'rg)
(require 's)

;; Create a unique buffer -- from the rg.el docs
(defadvice rg-run (before rg-run-before activate)
  ;; TODO: buffer name should be
  ;; "repo ROOT <query>" or "repo query <dir>"
  (rg-save-search))

(defun my/rg (&optional use-git-repo-root)
  (interactive)
  (let* ((pattern (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning)
                                                      (region-end))
                    (rg-read-pattern nil)))
         ;; ChatGPT 5.1
         (looks-like-regexp (string-match-p "[.*+?[^]$(){}|\\]" pattern))
         (dir (my/git-repo-root)))
    (rg-run pattern "everything" dir
            (if looks-like-regexp nil t))))

(defun my/redo-search-from-git-repo-root ()
  (interactive)
  (setf (rg-search-dir rg-cur-search) (my/git-repo-root))
  (rg-rerun))

(defun my/rg-menu ()
  (interactive)
  (let ((transient-default-level 7))
    (rg-menu)))

;; ChatGPT
(defun my/rg-tag-default-around (orig-fun &rest args)
  "Return nil in dired buffers, otherwise call ORIG-FUN."
  (if (derived-mode-p 'dired-mode)
      nil
    (apply orig-fun args)))

(advice-add 'rg-tag-default :around #'my/rg-tag-default-around)

(define-key rg-mode-map (kbd "R") 'my/redo-search-from-git-repo-root)
(define-key rg-mode-map (kbd "m") 'my/rg-menu)
(define-key rg-mode-map (kbd "N") 'rg-next-file)
(define-key rg-mode-map (kbd "P") 'rg-prev-file)
