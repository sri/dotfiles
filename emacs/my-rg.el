(require 'rg)
(require 's)

;; C-c r

;; Create a unique buffer -- from the rg.el docs
(defadvice rg-run (before rg-run-before activate)
  ;; TODO: buffer name should be
  ;; "repo ROOT <query>" or "repo query <dir>"
  (rg-save-search))

(defun my/rg (&optional use-git-repo-root)
  ;; Use case: I want to do a quick search, either in the curernt
  ;; directory or at the repo's root. And be able to modify the
  ;; parameters of that quickly.
  ;;
  (interactive)
  (let ((pattern (rg-read-pattern nil))
        (dir (if use-git-repo-root
                 (my/git-repo-root)
               default-directory)))
    (rg pattern (car (rg-default-alias)) dir)))

(defun my/rg-from-repo-root ()
  (interactive)
  (my/rg t))

(defun my/redo-search-from-git-repo-root ()
  (interactive)
  (setf (rg-search-dir rg-cur-search) (my/git-repo-root))
  (rg-rerun))

(define-key rg-mode-map (kbd "R") 'my/redo-search-from-git-repo-root)

(add-hook 'rg-mode-hook 'font-lock-mode)
