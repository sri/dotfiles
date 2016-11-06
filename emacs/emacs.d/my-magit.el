(require 'magit)

(setq magit-section-highlight-hook nil)
(setq magit-save-repository-buffers 'dontask)
(setq magit-commit-show-diff nil)
(setq magit-display-buffer-function
      'magit-display-buffer-same-window-except-diff-v1)

(remove-hook 'magit-status-sections-hook
             'magit-insert-stashes)

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
            (define-key magit-mode-map (kbd "C-c C-s") 'magit-stash-list)
            (define-key magit-mode-map (kbd "C-c C-w") 'my-magit-diff-toggle-refine-hunk)
            (define-key magit-mode-map (kbd "1") 'magit-section-show-level-1-all)
            (define-key magit-mode-map (kbd "2") 'magit-section-show-level-2-all)
            (define-key magit-mode-map (kbd "3") 'magit-section-show-level-3-all)
            (define-key magit-mode-map (kbd "4") 'magit-section-show-level-4-all)))
