(require 'magit)

(setq magit-save-repository-buffers 'dontask)
(setq magit-diff-refine-hunk nil)

(set-face-attribute 'magit-diff-added-highlight nil :foreground "#22aa22")

(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)
