(require 'magit)

(setf (nth 0 magit-status-margin) t)
(setf (nth 3 magit-status-margin) t)
(require 'git-link)

(setq git-link-open-in-browser t)
(setq git-link-use-commit t)

(setq magit-section-visibility-indicator nil)
(setq magit-section-highlight-hook nil)
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

(setq magit-diff-highlight-hunk-region-functions nil)
;; Don't show "Recent commits" section.
(setq magit-log-section-commit-count 10)

;(set-face-attribute 'magit-diff-added-highlight nil :foreground "#22aa22")

(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)

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

(add-hook 'magit-blame-mode-hook
          (lambda ()
            (font-lock-mode 1)))

(add-hook 'magit-mode-hook
          (lambda ()
            (font-lock-mode 1)
            (bind-keys :map magit-mode-map
                       ("C-c C-s" . magit-stash-list)
                       ("C-c C-w" . my/magit-diff-toggle-refine-hunk)
                       ("1" . magit-section-show-level-1-all)
                       ("2" . magit-section-show-level-2-all)
                       ("3" . magit-section-show-level-3-all)
                       ("4" . magit-section-show-level-4-all))))
