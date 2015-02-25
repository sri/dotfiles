(global-set-key (kbd "C-/") 'my-comment-line-or-region)
(global-set-key (kbd "C-=") 'my-increase-font-size)
(global-set-key (kbd "C--") 'my-decrease-font-size)
(global-set-key (kbd "C-a") 'my-beginning-of-line)
(global-set-key (kbd "C-b") 'backward-kill-word)
(global-set-key (kbd "C-d") 'kill-word)
(global-set-key (kbd "C-f") 'my-isearch-forward)
(global-set-key (kbd "C-i") 'my-hippie-tab)
(global-set-key (kbd "C-j") 'other-window)
(global-set-key (kbd "C-k") 'my-kill-line-or-region)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-n") 'execute-extended-command)
(global-set-key (kbd "C-o") 'my-ffap-or-find-file)
(global-set-key (kbd "C-p") 'my-shell)
(global-set-key (kbd "C-r") 'vr/query-replace)
(global-set-key (kbd "C-s") 'save-buffer)
;; (global-set-key (kbd "C-t") 'magit-status)
(global-set-key (kbd "C-v") 'ido-switch-buffer)
(global-set-key (kbd "C-w") 'my-kill-current-buffer)
(global-set-key (kbd "C-y") 'my-yank)
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

(global-set-key (kbd "C-c TAB") 'yas/expand)
(global-set-key (kbd "C-c \\") 'align-regexp)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C") 'org-capture)
(global-set-key (kbd "C-c c") 'calendar)
(global-set-key (kbd "C-c d") 'my-toggle-key-bindings)
(global-set-key (kbd "C-c e") 'er/expand-region)
(global-set-key (kbd "C-c g") 'my-git-grep)
(global-set-key (kbd "C-c i") 'my-change-inside-pair)
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
(global-set-key (kbd "C-c o") 'occur)
;; (global-set-key (kbd "C-c r") 'find-library)
(global-set-key (kbd "C-c s") 'sort-lines)
(global-set-key (kbd "C-c v") 'view-mode)
(global-set-key (kbd "C-c w") 'compare-windows)

(defvar ctl-c-r-map)
(define-prefix-command 'ctl-c-r-map)
(define-key global-map (kbd "C-c r") ctl-c-r-map)

(global-set-key (kbd "C-c r n") 'my-remove-non-ascii-chars)

(global-set-key (kbd "C-'") 'eval-last-sexp)
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-vertically)
(global-set-key (kbd "C-3") 'split-window-horizontally)
(global-set-key (kbd "C-;") 'eval-expression)
(global-set-key (kbd "C-\"") 'my-emacs-lisp-eval)
(global-set-key (kbd "<C-return>") 'kill-ring-save)
;; (global-set-key (kbd "C-\\") ')

;; I like to use Emacs in a console (via iTerm)
(unless window-system
  (global-set-key (kbd "º") 'delete-window)               ; 0
  (global-set-key (kbd "¡") 'delete-other-windows)        ; 1
  (global-set-key (kbd "™") 'split-window-vertically)     ; 2
  (global-set-key (kbd "£") 'split-window-horizontally)   ; 3
  (global-set-key (kbd "≤") 'beginning-of-buffer)         ; ,
  (global-set-key (kbd "≥") 'end-of-buffer)               ; .
  (global-set-key (kbd "…") 'eval-expression)             ; ;
  (global-set-key (kbd "«") 'my-emacs-lisp-eval)          ; \
  (global-set-key (kbd "æ") 'eval-last-sexp)
  (global-set-key (kbd "ESC <up>") 'scroll-down)
  (global-set-key (kbd "ESC <down>") 'scroll-up)
  (global-set-key (kbd "∂") 'my-dired)                    ; d
  (global-set-key (kbd "¿") 'my-comment-line-or-region)   ; S-/
  (global-set-key (kbd " ") 'bm-toggle)                   ; SPC
  (global-set-key (kbd "∫") 'bm-next)                     ; b
  (global-set-key (kbd "∆") 'other-window)                ; j

  (global-set-key (kbd "Î") 'my-duplicate-line-or-region) ; S-d
  (global-set-key (kbd "") 'my-kill-whole-line)          ; S-k
  (global-set-key (kbd "¬") 'mc/edit-lines)               ; l

  (global-set-key (kbd "µ") 'magit-status)                ; m
  (global-set-key (kbd "ƒ") 'my-isearch-forward)          ; f

  (global-set-key (kbd "ß") 'save-buffer)                 ; s
  (global-set-key (kbd "Ω") 'undo)
  ;; (global-set-key (kbd "") ')
  ;; (global-set-key (kbd "") ')
  ;; (global-set-key (kbd "") ')
  ;; (global-set-key (kbd "") ')
)


(global-set-key (kbd "S-C-b") 'bm-next)
(global-set-key (kbd "S-C-c") 'my-duplicate-line-or-region)
(global-set-key (kbd "S-C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "S-C-f") 'my-find-in-directory)
(global-set-key (kbd "S-C-h") 'my-quick-hotkey)
(global-set-key (kbd "S-C-j") 'my-join-line)
(global-set-key (kbd "S-C-k") 'my-kill-whole-line)
(global-set-key (kbd "S-C-l") 'mc/edit-lines)
(global-set-key (kbd "S-C-n") 'make-frame-command)
(global-set-key (kbd "S-C-o") 'open-line)
(global-set-key (kbd "S-C-r") 'query-replace-regexp)
(global-set-key (kbd "S-C-w") 'delete-frame)

;(global-set-key (kbd "M-n") 'my-sticky-buf-new)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "<M-SPC>") 'my-just-one-space)
(global-set-key (kbd "<M-down>") 'scroll-up)
(global-set-key (kbd "<M-up>") 'scroll-down)
(global-set-key (kbd "M-\\") 'my-delete-horizontal-space)

(global-set-key (kbd "C-x l") 'my-count-lines-buffer)
(global-set-key (kbd "C-x r K") 'my-copy-from-starting-col-till-eol)
(global-set-key (kbd "C-x s") 'my-start-line-or-region-swap)

(global-set-key (kbd "C-x v -") 'my-unsaved-changes)

(global-set-key (kbd "<S-f6>") 'my-find-tag-next)
(global-set-key (kbd "<f1>") 'magit-status)
(global-set-key (kbd "<f6>") 'find-tag)
(global-set-key (kbd "<f7>") 'pop-tag-mark)

(global-set-key (kbd "<C-S-iso-lefttab>") 'other-frame)

(global-set-key (kbd "<S-return>") 'my-dired)

(when (eq system-type 'darwin)
  (global-set-key (kbd "<s-down>") 'scroll-up)
  (global-set-key (kbd "<s-up>") 'scroll-down))

(global-set-key (kbd "<C-tab>") 'my-switch-to-buffer)

(my-override-keys "C-j" 'other-window '(lisp-interaction-mode))


(my-override-keys "C-w" 'my-kill-current-buffer
                  '(magit-log-mode magit-branch-manager-mode
                                   magit-status-mode
                                   magit-wazzup-mode
                                   magit-commit-mode
                                   magit-log-edit-mode
                                   magit-stash-mode
                                   magit-reflog-mode
                                   magit-diff-mode))
