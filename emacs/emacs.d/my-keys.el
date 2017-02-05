(eval-when-compile
  (require 'cl))

(require 'bind-key)

(bind-keys
 ;; Key which don't want to override in all modes.
 ;; For example, Magit does useful things with C-i
 ;; (TAB) -- show/hide file diffs.
 ("C-i" . my-hippie-tab)
 ("C-m" . newline-and-indent))

(bind-keys*
 ("<C-tab>" . my-switch-to-buffer)
 ("C-\\" . other-frame)
 ("C-a" . my-beginning-of-line)
 ("C-b" . backward-kill-word)
 ("C-d" . kill-word)
 ("C-f" . my-isearch-forward)
 ("C-j" . other-window)
 ("C-k" . my-kill-line-or-region)
 ("C-n" . helm-M-x)
 ("C-o" . my-ffap-or-find-file)
 ("C-p" . my-shell)
 ("C-r" . vr/query-replace)
 ("C-s" . save-buffer)
 ("C-v" . helm-buffers-list)
 ("C-w" . my-kill-current-buffer)
 ("C-y" . my-yank)
 ("C-z" . undo)

 ("C-c C" . org-capture)
 ("C-c TAB" . yas/expand)
 ("C-c \\" . align-regexp)
 ("C-c a" . org-agenda)
 ("C-c b" . rename-buffer)
 ("C-c c" . calendar)
 ("C-c f" . my-find-file-in-other-window)
 ("C-c i" . my-change-inside-pair)
 ("C-c j" . ace-jump-mode)
 ("C-c l" . toggle-truncate-lines)
 ("C-c m" . imenu)
 ("C-c o" . my-occur)
 ("C-c s" . sort-lines)
 ("C-c F" . recentf-open-files)

 ("C-h SPC" . helm-all-mark-rings)

 ("M-0" . delete-window)
 ("M-1" . delete-other-windows)
 ("M-2" . split-window-vertically)
 ("M-3" . split-window-horizontally)
 ("<M-SPC>" . my-just-one-space)
 ("<M-return>" . my-dired)
 ("M-;" . my-comment-line-or-region)
 ("M-<down>" . scroll-up)
 ("M-<up>" . scroll-down)
 ("M-D" . my-duplicate-line-or-region)
 ("M-E" . mc/edit-lines)
 ("M-\\" . my-delete-horizontal-space)
 ("M-d" . my-dired)
 ("M-g" . goto-line)
 ("M-k" . my-kill-whole-line)
 ("M-n" . bm-next)
 ("M-o" . helm-projectile)
 ("M-p" . bm-previous)
 ("M-v" . bm-toggle)
 ("M-x" . helm-M-x)
 ("M-y" . helm-show-kill-ring)

 ("<home>" . beginning-of-buffer)
 ("<end>" . end-of-buffer)

 ("C-x b" . helm-buffers-list)
 ("C-x g" . magit-status)
 ("C-x l" . my-count-lines-buffer)
 ("C-x r K" . my-copy-from-starting-col-till-eol)
 ("C-x s" . my-start-line-or-region-swap)
 ("C-x v -" . my-unsaved-changes)
 ("C-x C-v" . my-find-file-as-sudo)
 ("C-x <up>" . windmove-up)
 ("C-x <down>" . windmove-down)
 ("C-x <left>" . windmove-left)
 ("C-x <right>" . windmove-right)
 )

;; Git related under: C-c g <letter>
(bind-keys :prefix-map my-ctl-c-g-map
           :prefix "C-c g"
           ("b" . magit-blame)
           ("f" . magit-log-buffer-file)
           ("g" . helm-grep-do-git-grep))

(when (eq system-type 'darwin)
  ;; Command-<enter>
  (bind-key* "<s-return>" 'toggle-frame-fullscreen))
