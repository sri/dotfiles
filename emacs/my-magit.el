;; -*- lexical-binding: t; -*-
(require 'magit)

(require 'git-link)

;; https://stackoverflow.com/questions/462974/what-are-the-differences-between-double-dot-and-triple-dot-in-git-com
;; git log  A..B   # Show me commits only on B.
;; git log  A...B  # Show me commits only on A or only on B.
;; git diff A..B   # Show me changes only on A or only on B.
;; git diff A...B  # Show me changes only on B.

(put 'magit-diff-edit-hunk-commit 'disabled nil)

(setq git-link-open-in-browser t)
(setq git-link-use-commit t)

(setq magit-section-visibility-indicator nil)
(setq magit-save-repository-buffers 'dontask)
(setq magit-commit-show-diff t)
(setq magit-display-buffer-function
      'magit-display-buffer-same-window-except-diff-v1)

(add-to-list 'magit-section-initial-visibility-alist
             '(untracked . hide))
(add-to-list 'magit-section-initial-visibility-alist
             '(unpushed . hide))

(setq magit-status-sections-hook
      '(magit-insert-status-headers
        magit-insert-merge-log
        magit-insert-rebase-sequence
        magit-insert-am-sequence
        magit-insert-sequencer-sequence
        magit-insert-bisect-output
        magit-insert-bisect-rest
        magit-insert-bisect-log
        magit-insert-unstaged-changes
        magit-insert-staged-changes
        magit-insert-stashes
        magit-insert-unpushed-to-pushremote
        magit-insert-unpushed-to-upstream-or-recent
        magit-insert-unpulled-from-pushremote
        magit-insert-unpulled-from-upstream
        magit-insert-untracked-files))

;; Show these many commits in the "Recent commits" section.
(setq magit-log-section-commit-count 10)

;(set-face-attribute 'magit-diff-added-highlight nil :foreground "#22aa22")

(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)

(setq magit-diff-refine-hunk t)
(defun my/magit-diff-toggle-refine-hunk ()
  (interactive)
  (let* ((vals '((t . "current hunk")
                 (all . "all")
                 (nil . "off")))
         (next (or (cadr (cl-member magit-diff-refine-hunk vals
                                    :key #'car))
                   (car vals))))
    (setq magit-diff-refine-hunk (car next))
    (message "Word-diff: %s" (cdr next))))

(defun my/open-repo-in-browser ()
  (interactive)
  (let ((url (magit-get "remote" "origin" "url")))
    (when (string-match "^ssh" url)
      (setq url (s-replace-all ":22" "" (s-chop-prefix "ssh" url)))
      (setq url (concat "https://" url)))
    (unless (string-match "^http" url)
      (setq url (replace-regexp-in-string (rx (group (zero-or-more any)) "@"
                                              (group (zero-or-more any)) ":"
                                              (group (zero-or-more any))
                                              (group ".git"))
                                          "https://\\2/\\3"
                                          url)))
    (message "Opening %s" url)
    (browse-url url)))

(add-hook 'magit-mode-hook
          (lambda ()
            (bind-keys :map magit-mode-map
                       ("~" . my/open-repo-in-browser)
                       ("C-c C-s" . magit-stash-list)
                       ("C-c C-w" . my/magit-diff-toggle-refine-hunk))))
