(global-set-key (kbd "<C-tab>") 'my-switch-to-buffer)
(global-set-key (kbd "C-\\") 'other-frame)
(global-set-key (kbd "C-a") 'my-beginning-of-line)
(global-set-key (kbd "C-b") 'backward-kill-word)
(global-set-key (kbd "C-d") 'kill-word)
(global-set-key (kbd "C-f") 'my-isearch-forward)
(global-set-key (kbd "C-i") 'my-hippie-tab)
(global-set-key (kbd "C-j") 'other-window)
(global-set-key (kbd "C-k") 'my-kill-line-or-region)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-n") 'helm-M-x)
(global-set-key (kbd "C-o") 'my-ffap-or-find-file)
(global-set-key (kbd "C-p") 'my-shell)
(global-set-key (kbd "C-r") 'vr/query-replace)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-v") 'helm-buffers-list)
(global-set-key (kbd "C-w") 'my-kill-current-buffer)
(global-set-key (kbd "C-y") 'my-yank)
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-c C") 'org-capture)
(global-set-key (kbd "C-c TAB") 'yas/expand)
(global-set-key (kbd "C-c \\") 'align-regexp)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'calendar)
(global-set-key (kbd "C-c g") 'my-git-grep)
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c s") 'sort-lines)

(global-set-key (kbd "<M-SPC>") 'my-just-one-space)
(global-set-key (kbd "<M-down>") 'scroll-up)
(global-set-key (kbd "<M-return>") 'my-dired)
(global-set-key (kbd "<M-up>") 'scroll-down)
(global-set-key (kbd "M-'") 'my-emacs-lisp-eval)
(global-set-key (kbd "M-,") 'beginning-of-buffer)
(global-set-key (kbd "M-.") 'end-of-buffer)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-;") 'my-comment-line-or-region)
(global-set-key (kbd "M-<down>") 'scroll-up)
(global-set-key (kbd "M-<up>") 'scroll-down)
(global-set-key (kbd "M-D") 'my-duplicate-line-or-region)
(global-set-key (kbd "M-E") 'mc/edit-lines)
(global-set-key (kbd "M-N") 'bm-previous)
(global-set-key (kbd "M-\\") 'my-delete-horizontal-space)
(global-set-key (kbd "M-b") 'bm-toggle)
(global-set-key (kbd "M-d") 'my-dired)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-i") 'my-change-inside-pair)
(global-set-key (kbd "M-k") 'my-kill-whole-line)
(global-set-key (kbd "M-m") 'magit-status)
(global-set-key (kbd "M-n") 'bm-next)
(global-set-key (kbd "M-o") 'helm-projectile)

(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

(defvar ctl-c-r-map)
(define-prefix-command 'ctl-c-r-map)
(define-key global-map (kbd "C-c r") ctl-c-r-map)
(global-set-key (kbd "C-c r n") 'my-remove-non-ascii-chars)

(global-set-key (kbd "C-x l") 'my-count-lines-buffer)
(global-set-key (kbd "C-x r K") 'my-copy-from-starting-col-till-eol)
(global-set-key (kbd "C-x s") 'my-start-line-or-region-swap)
(global-set-key (kbd "C-x v -") 'my-unsaved-changes)

;; Unbind
(cl-flet ((unset-key-in-mode (mode &rest keys)
            (lexical-let ((keys keys)
                          (hook (intern (format "%s-hook" mode))))
              (add-hook hook
                        (lambda ()
                          (dolist (key keys)
                            (local-unset-key (kbd key))))))))

  (unset-key-in-mode 'lisp-interaction-mode "C-j")
  (unset-key-in-mode 'magit-status-mode "M-1" "M-2" "M-3")

  (let ((magit-modes '(magit-log-mode
                       magit-branch-manager-mode
                       magit-status-mode
                       magit-wazzup-mode
                       magit-commit-mode
                       magit-log-edit-mode
                       magit-stash-mode
                       magit-reflog-mode
                       magit-diff-mode)))
    (dolist (mode magit-modes)
      (unset-key-in-mode mode "C-w")))

  (unset-key-in-mode 'shell-mode "C-d")

  (unset-key-in-mode 'org-mode "C-j" "C-," "<S-return>")

  )
