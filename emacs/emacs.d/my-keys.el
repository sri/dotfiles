(eval-when-compile
  (require 'cl))

(global-set-key (kbd "<C-tab>") 'my-switch-to-buffer)
(global-set-key (kbd "C-\\") 'other-frame)
(global-set-key (kbd "C-a") 'my-beginning-of-line)
(global-set-key (kbd "C-b") 'backward-kill-word)
(global-set-key (kbd "C-d") 'kill-word)
(global-set-key (kbd "C-f") 'my-isearch-forward)
(global-set-key (kbd "C-i") 'my-hippie-tab)
(global-set-key (kbd "C-k") 'my-kill-line-or-region)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-n") 'helm-M-x)
(global-set-key (kbd "C-o") 'my-ffap-or-find-file)
(global-set-key (kbd "C-p") 'my-shell)
(global-set-key (kbd "C-r") 'vr/query-replace)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-v") 'helm-buffers-list)
(global-set-key (kbd "C-y") 'my-yank)
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-c C") 'org-capture)
(global-set-key (kbd "C-c TAB") 'yas/expand)
(global-set-key (kbd "C-c \\") 'align-regexp)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'rename-buffer)
(global-set-key (kbd "C-c c") 'calendar)
(global-set-key (kbd "C-c f") 'recentf-open-files)
(global-set-key (kbd "C-c g") 'my-git-grep)
(global-set-key (kbd "C-c i") 'my-change-inside-pair)
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
(global-set-key (kbd "C-c o") 'my-occur)
(global-set-key (kbd "C-c s") 'sort-lines)

(global-set-key (kbd "<M-SPC>") 'my-just-one-space)
(global-set-key (kbd "<M-return>") 'my-dired)
(global-set-key (kbd "M-;") 'my-comment-line-or-region)
(global-set-key (kbd "M-<down>") 'scroll-up)
(global-set-key (kbd "M-<up>") 'scroll-down)
(global-set-key (kbd "M-D") 'my-duplicate-line-or-region)
(global-set-key (kbd "M-E") 'mc/edit-lines)
(global-set-key (kbd "M-\\") 'my-delete-horizontal-space)
(global-set-key (kbd "M-d") 'my-dired)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-k") 'my-kill-whole-line)
(global-set-key (kbd "M-n") 'bm-next)
(global-set-key (kbd "M-o") 'helm-projectile)
(global-set-key (kbd "M-p") 'bm-previous)
(global-set-key (kbd "M-v") 'bm-toggle)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "<M-down>") 'scroll-up)
;; (global-set-key (kbd "<M-up>") 'scroll-down)
;; (global-set-key (kbd "M-'") 'my-emacs-lisp-eval)
;; (global-set-key (kbd "M-,") 'beginning-of-buffer)
;; (global-set-key (kbd "M-.") 'end-of-buffer)

(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

(defvar ctl-c-r-map)
(define-prefix-command 'ctl-c-r-map)
(define-key global-map (kbd "C-c r") ctl-c-r-map)
(global-set-key (kbd "C-c r n") 'my-remove-non-ascii-chars)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x l") 'my-count-lines-buffer)
(global-set-key (kbd "C-x r K") 'my-copy-from-starting-col-till-eol)
(global-set-key (kbd "C-x s") 'my-start-line-or-region-swap)
(global-set-key (kbd "C-x v -") 'my-unsaved-changes)
(global-set-key (kbd "C-x C-v") 'my-find-file-as-sudo)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; Add keys that should always be present in all modes. Some modes
;; override these keys (for example, Magit overrides M-0 to show/hide
;; parts of the buffer contents). We can't have all keys use this
;; method as we want modes to provide their specific keys. For
;; example, TAB in org-mode should show/hide sections/lists and not do
;; my tab completion.
(defvar my-keys-map (make-sparse-keymap))
(define-minor-mode my-keys-mode "My keys." t nil my-keys-map)

(define-key my-keys-map (kbd "C-j") 'other-window)
(define-key my-keys-map (kbd "C-w") 'my-kill-current-buffer)
(define-key my-keys-map (kbd "M-0") 'delete-window)
(define-key my-keys-map (kbd "M-1") 'delete-other-windows)
(define-key my-keys-map (kbd "M-2") 'split-window-vertically)
(define-key my-keys-map (kbd "M-3") 'split-window-horizontally)

(when (eq system-type 'darwin)
  ;; Command-<enter>
  (global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen))
