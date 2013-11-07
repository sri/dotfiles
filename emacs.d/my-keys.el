(global-set-key (kbd "C-b") 'backward-kill-word)
(global-set-key (kbd "C-d") 'kill-word)
(global-set-key (kbd "C-f") 'my-isearch-forward)
(global-set-key (kbd "C-i") 'my-hippie-tab)
(global-set-key (kbd "C-j") 'other-window)
(global-set-key (kbd "C-k") 'my-kill-line-or-region)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-n") 'execute-extended-command)
(global-set-key (kbd "C-o") 'ffap)
(global-set-key (kbd "C-p") 'shell)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-t") 'ido-switch-buffer)
;(global-set-key (kbd "C-v") 'clipboard-yank)
(global-set-key (kbd "C-w") 'my-kill-current-buffer)
(global-set-key (kbd "C-y") 'my-yank)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-/") 'my-comment-line-or-region)

(global-set-key (kbd "C-c c") 'calendar)
(global-set-key (kbd "C-c i") 'my-change-inside-pair)
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)

(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-vertically)
(global-set-key (kbd "C-3") 'split-window-horizontally)
(global-set-key (kbd "C-4") 'universal-argument)

(global-set-key (kbd "S-C-k") 'kill-whole-line)
(global-set-key (kbd "S-C-j") 'join-line)
(global-set-key (kbd "S-C-d") 'my-duplicate-line-or-region)

(global-set-key (kbd "<M-down>") 'scroll-up)
(global-set-key (kbd "<M-up>") 'scroll-down)

(global-set-key (kbd "C-x k") 'my-kill-current-buffer)
(global-set-key (kbd "C-x s") 'my-start-line-or-region-swap)

(global-set-key (kbd "<f1>") 'magit-status)
(global-set-key (kbd "<f2>") 'ido-switch-buffer)
(global-set-key (kbd "<f6>") 'find-tag)
(global-set-key (kbd "<S-f6>") 'my-find-tag-next)
(global-set-key (kbd "<f7>") 'pop-tag-mark)

(global-set-key (kbd "<S-return>") 'my-dired)

(when (eq system-type 'darwin)
  (global-set-key (kbd "<s-up>") 'scroll-down)
  (global-set-key (kbd "<s-down>") 'scroll-up))

(global-set-key (kbd "<C-tab>") 'my-switch-to-buffer)

(my-overwrite-key-bindings-in-mode "C-j" 'other-window
                                   '(lisp-interaction-mode))

(my-overwrite-key-bindings-in-mode "C-w" 'my-kill-current-buffer
                                   '(magit-log-mode magit-branch-manager-mode
                                                    magit-status-mode
                                                    magit-wazzup-mode
                                                    magit-commit-mode
                                                    magit-log-edit-mode
                                                    magit-stash-mode
                                                    magit-reflog-mode
                                                    magit-diff-mode))

;; Try out:
;; [C-down-mouse-1]
;; Make join-line respect region
