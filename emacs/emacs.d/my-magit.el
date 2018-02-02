(require 'magit)

(setq magit-section-highlight-hook nil)
(setq magit-save-repository-buffers 'dontask)
(setq magit-commit-show-diff t)
(setq magit-display-buffer-function
      'magit-display-buffer-same-window-except-diff-v1)

;; Don't show "Recent commits" section.
(setq magit-log-section-commit-count 0)

(set-face-attribute 'magit-diff-added-highlight nil :foreground "#22aa22")

(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)

(defun my-magit-diff-toggle-refine-hunk ()
  (interactive)
  (let* ((vals '((t . "current hunk")
                 (all . "all")
                 (nil . "off")))
         (next (or (cadr (cl-member magit-diff-refine-hunk vals
                                    :key #'car))
                   (car vals))))
    (setq magit-diff-refine-hunk (car next))
    (message "Word-diff: %s" (cdr next))))

(add-hook 'magit-mode-hook
          (lambda ()
            (bind-keys :map magit-mode-map
                       ("C-c C-s" . magit-stash-list)
                       ("C-c C-w" . my-magit-diff-toggle-refine-hunk)
                       ("1" . magit-section-show-level-1-all)
                       ("2" . magit-section-show-level-2-all)
                       ("3" . magit-section-show-level-3-all)
                       ("4" . magit-section-show-level-4-all))))
