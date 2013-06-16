(global-set-key (kbd "C-b") 'backward-kill-word)
(global-set-key (kbd "C-d") 'kill-word)
(global-set-key (kbd "C-f") 'my-isearch-forward)
;; (global-set-key (kbd "C-S-f") 'ag-regexp-project-at-point)
(global-set-key (kbd "C-i") 'my-hippie-tab)
(global-set-key (kbd "C-j") 'other-window)
(global-set-key (kbd "C-k") 'my-kill-line-or-region)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-o") 'ffap)
(global-set-key (kbd "C-n") 'execute-extended-command)
(global-set-key (kbd "C-p") 'shell)
;;(global-set-key (kbd "C-q") 'ido-switch-buffer)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-t") 'ido-switch-buffer)
(global-set-key (kbd "C-v") 'clipboard-yank)
(global-set-key (kbd "C-w") 'my-kill-current-buffer)
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "S-C-j") 'join-line)

(global-set-key (kbd "C-x C-q") 'quoted-insert) ; was toggle-read-only

(global-set-key (kbd "<M-down>") 'scroll-up)
(global-set-key (kbd "<M-up>") 'scroll-down)

(global-set-key (kbd "C-x k") 'my-kill-current-buffer)

(global-set-key (kbd "<f1>") 'view-mode)
(global-set-key (kbd "<f5>") 'magit-status)
(global-set-key (kbd "<f6>") 'find-tag)
(global-set-key (kbd "<S-f6>") 'my-find-tag-next)
(global-set-key (kbd "<f7>") 'pop-tag-mark)

(when (eq system-type 'darwin)
  (global-set-key (kbd "<s-up>") 'scroll-down)
  (global-set-key (kbd "<s-down>") 'scroll-up))

;(global-set-key (kbd "s-J")
;                'my-sublime-expand-selection-to-indentation)
;(global-set-key (kbd "M-J")
;                'my-sublime-expand-selection-to-indentation)

(global-set-key [C-down-mouse-1] 'mouse-delete-other-windows)

(my-overwrite-key-bindings-in-mode "C-j" 'other-window
                                   '(lisp-interaction-mode))
