(my-global-set-key (kbd "C-b") 'backward-kill-word)
(my-global-set-key (kbd "C-d") 'kill-word)
(my-global-set-key (kbd "C-f") 'my-isearch-forward)
(my-global-set-key (kbd "C-i") 'my-hippie-tab)
(my-global-set-key (kbd "C-j") 'other-window)
(my-global-set-key (kbd "C-k") 'my-kill-line-or-region)
(my-global-set-key (kbd "C-m") 'newline-and-indent)
(my-global-set-key (kbd "C-n") 'execute-extended-command)
(my-global-set-key (kbd "C-o") 'ffap)
(my-global-set-key (kbd "C-p") 'my-shell)
(my-global-set-key (kbd "C-r") 'query-replace)
(my-global-set-key (kbd "C-s") 'save-buffer)
(my-global-set-key (kbd "C-t") 'magit-status)
(my-global-set-key (kbd "C-v") 'ido-switch-buffer)
(my-global-set-key (kbd "C-w") 'my-kill-current-buffer)
(my-global-set-key (kbd "C-y") 'my-yank)
(my-global-set-key (kbd "C-z") 'undo)
(my-global-set-key (kbd "C-/") 'my-comment-line-or-region)

(my-global-set-key (kbd "C-c c") 'calendar)
(my-global-set-key (kbd "C-c d") 'my-toggle-key-bindings)
(my-global-set-key (kbd "C-c i") 'my-change-inside-pair)
(my-global-set-key (kbd "C-c l") 'toggle-truncate-lines)
(my-global-set-key (kbd "C-c r") 'find-library)
(my-global-set-key (kbd "C-c v") 'view-mode)
(my-global-set-key (kbd "C-c w") 'compare-windows)
(my-global-set-key (kbd "C-c TAB") 'yas/expand)

(my-global-set-key (kbd "C-,") 'beginning-of-buffer)
(my-global-set-key (kbd "C-.") 'end-of-buffer)
(my-global-set-key (kbd "C-;") 'eval-expression)
(my-global-set-key (kbd "C-'") 'my-eval-last-sexp-or-eval-defun)
(my-global-set-key (kbd "C-0") 'delete-window)
(my-global-set-key (kbd "C-1") 'delete-other-windows)
(my-global-set-key (kbd "C-2") 'split-window-vertically)
(my-global-set-key (kbd "C-3") 'split-window-horizontally)

(my-global-set-key (kbd "S-C-d") 'my-duplicate-line-or-region)
(my-global-set-key (kbd "S-C-f") 'my-find-in-directory)
(my-global-set-key (kbd "S-C-h") 'my-quick-hotkey)
(my-global-set-key (kbd "S-C-j") 'my-join-line)
(my-global-set-key (kbd "S-C-k") 'my-kill-whole-line)
(my-global-set-key (kbd "S-C-n") 'make-frame-command)
(my-global-set-key (kbd "S-C-r") 'query-replace-regexp)
(my-global-set-key (kbd "S-C-w") 'delete-frame)

(my-global-set-key (kbd "<M-down>") 'scroll-up)
(my-global-set-key (kbd "<M-up>") 'scroll-down)
(my-global-set-key (kbd "<M-SPC>") 'my-just-one-space)

(my-global-set-key (kbd "C-x l") 'my-count-lines-buffer)
(my-global-set-key (kbd "C-x s") 'my-start-line-or-region-swap)

(my-global-set-key (kbd "<f1>") 'magit-status)
(my-global-set-key (kbd "<f6>") 'find-tag)
(my-global-set-key (kbd "<S-f6>") 'my-find-tag-next)
(my-global-set-key (kbd "<f7>") 'pop-tag-mark)

(my-global-set-key (kbd "<C-S-iso-lefttab>") 'other-frame)

(my-global-set-key (kbd "<S-return>") 'my-dired)

(when (eq system-type 'darwin)
  (my-global-set-key (kbd "<s-up>") 'scroll-down)
  (my-global-set-key (kbd "<s-down>") 'scroll-up))

(my-global-set-key (kbd "<C-tab>") 'my-switch-to-buffer)

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
