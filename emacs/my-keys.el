;; M-C-s was isearch-forward-regexp; its now S-C-f

(eval-when-compile
  (require 'cl))

(require 'subword)
(require 'bind-key)

(bind-keys
 ;; Key which don't want to override in all modes.
 ;; For example, Magit does useful things with C-i
 ;; (TAB) -- show/hide file diffs.
 ("C-i" . my/hippie-tab)
 ("<M-return>" . my/dired)
 ("C-m" . newline-and-indent))

;; Mac trackpad
;; Problem with wheel (2-finger swipe on os x)
;; is that the even will keep call the function
;; over and over again.
;; (bind-keys*
;; ("<wheel-left>" . (lambda ()
;; (interactive)
;; (message "mouse wheel left")
;; )))

(bind-keys*
 ("S-s-g" . helm-grep-do-git-grep)
 ("s-0" . delete-window)
 ("s-1" . delete-other-windows)
 ("s-2" . split-window-vertically)
 ("s-3" . split-window-horizontally)
 ("s-c" . my/copy-line-or-region)
 ("s-x" . my/kill-line-or-region)
 ("s-g" . magit-status)
 ("s-," . beginning-of-buffer)
 ("s-o" . my/ffap-or-find-file)
 ("s-." . end-of-buffer)
 ("s-j" . other-window)
 ("s-p" . my/shell))

(bind-keys*
 ("<C-tab>" . my/switch-to-buffer)
 ("C-`" . my/shell-for-buffer)
 ("C-\\" . other-frame)
 ("C-a" . my/beginning-of-line)
 ("C-b" . backward-kill-word)
 ("C-d" . kill-word)
 ("C-f" . isearch-forward)
 ("S-C-f" . isearch-forward-regexp)
 ("C-j" . other-window)
 ("C-k" . my/kill-line-or-region)
 ("S-C-k" . my/copy-line-or-region)
 ("C-n" . helm-M-x)
 ("C-o" . my/ffap-or-find-file)
 ("C-p" . my/shell)
 ("C-r" . vr/query-replace)
 ("C-s" . save-buffer)
 ;; ("C-t" . )
 ("C-v" . helm-buffers-list)
 ("C-w" . my/kill-current-buffer)
 ("C-y" . my/yank)
 ("C-z" . undo)

 ("C-c C" . org-capture)
 ("C-c TAB" . yas/expand)
 ("C-c \\" . align-regexp)
 ("C-c a" . org-agenda)
 ("C-c b" . rename-buffer)
 ("C-c c" . calendar)
 ("C-c f" . rg)
 ("C-c i" . my/change-inside-pair)
 ("C-c j" . ace-jump-word-mode)
 ("C-c k" . ace-jump-line-mode)
 ("C-c l" . toggle-truncate-lines)
 ("C-c m" . imenu)
 ("C-c n" . my/neotree)
 ("C-c o" . my/occur)
 ("C-c p" . my/copy-full-path)
 ("C-c s" . sort-lines)
 ("C-c F" . helm-recentf)
 ("C-c O" . open-line)
 ("C-c C-l" . my/toggle-auto-hscroll-mode)
 ("<C-backspace>" . subword-backward-kill)
 ("C-h SPC" . helm-all-mark-rings)

 ("M-0" . delete-window)
 ("M-1" . delete-other-windows)
 ("M-2" . split-window-vertically)
 ("M-3" . split-window-horizontally)
 ("<M-SPC>" . my/just-one-space)
 ("M-;" . my/comment-line-or-region)
 ("M-<down>" . scroll-up)
 ("M-<up>" . scroll-down)
 ("M-D" . my/duplicate-line-or-region)
 ("M-E" . mc/edit-lines)
 ("M-\\" . my/delete-horizontal-space)
 ("M-d" . my/dired)
 ("M-g" . goto-line)
 ("M-k" . my/kill-whole-line)
 ("M-o" . helm-projectile)

 ("M-n" . bm-next)
 ("M-p" . bm-previous)
 ("M-N" . bm-show-all)
 ("M-v" . bm-toggle)
 ("ESC M-v" . bm-show)

 ("M-x" . helm-M-x)
 ("M-y" . helm-show-kill-ring)

 ("<home>" . beginning-of-buffer)
 ("<end>" . end-of-buffer)

 ("C-x b" . helm-buffers-list)
 ("C-x g" . magit-status)
 ("C-x l" . my/count-lines-buffer)
 ("C-x r K" . my/copy-from-starting-col-till-eol)
 ("C-x s" . my/start-line-or-region-swap)
 ("C-x v -" . my/unsaved-changes)
 ("C-x C-v" . my/find-file-as-sudo)
 ("C-x <up>" . windmove-up)
 ("C-x <down>" . windmove-down)
 ("C-x <left>" . windmove-left)
 ("C-x <right>" . windmove-right)
 )

;; Git related under: C-c g <letter>
(bind-keys :prefix-map my/ctl-c-g-map
           :prefix "C-c g"
           ("l" . git-link)
           ("b" . magit-blame-addition)
           ("f" . magit-log-buffer-file)
           ("r" . my/git-grep-from-root)
           ("g" . helm-grep-do-git-grep))

(when (eq system-type 'darwin)
  ;; Command-<enter>
  (bind-key* "<s-return>" 'toggle-frame-fullscreen))
