(require 'magit)

(setq magit-save-repository-buffers 'dontask)
(setq magit-diff-refine-hunk nil)
(setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

(set-face-attribute 'magit-diff-added-highlight nil :foreground "#22aa22")

(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)

(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd "C-c C-w") 'magit-diff-toggle-refine-hunk)
            (define-key magit-mode-map (kbd "1") 'magit-section-show-level-1-all)
            (define-key magit-mode-map (kbd "2") 'magit-section-show-level-2-all)
            (define-key magit-mode-map (kbd "3") 'magit-section-show-level-3-all)
            (define-key magit-mode-map (kbd "4") 'magit-section-show-level-4-all)))
