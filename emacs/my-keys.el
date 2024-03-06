;; M-C-s was isearch-forward-regexp; its now S-C-f

(eval-when-compile
  (require 'cl))

(require 'subword)
(require 'bind-key)

(bind-keys :map vertico-map
           ("C-." . embark-act)
           ("C-SPC" . embark-select)
           ("C-x" . embark-export)
           ("ESC p" . consult-history)
           ("C-'" . vertico-quick-insert))

(require 'embark)
(bind-keys :map embark-region-map
           ("g" . my/google-search))

(bind-keys
 ;; Key which don't want to override in all modes.
 ;; For example, Magit does useful things with C-i
 ;; (TAB) -- show/hide file diffs.
 ("C-i" . my/hippie-tab)
 ("<M-return>" . my/dired)
 ("C-m" . newline-and-indent)
 ("M-n" . my/bm-next-this-file)
 ("M-N" . bm-next)
 ("M-p" . bm-previous)
 ("M-N" . bm-show-all)
 ("M-v" . bm-toggle)
 ("ESC M-v" . bm-show))

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
 ("s-0" . delete-window)
 ("s-1" . delete-other-windows)
 ("s-2" . split-window-vertically)
 ("s-3" . split-window-horizontally)
 ("s-c" . my/copy-line-or-region)
 ("s-x" . my/kill-line-or-region)
 ("s-g" . consult-git-grep)
 ("M-s-g" . consult-ripgrep)
 ("s-m" . magit-status)
 ("s-," . beginning-of-buffer)
 ("s-o" . my/ffap-or-find-file)
 ("s-." . end-of-buffer)
 ("s-j" . other-window)
 ("s-p" . my/shell))

;; https://emacs.stackexchange.com/questions/32183/how-to-make-exceptions-to-bind-key-overriding-behavior

(bind-key* "C-d" 'kill-word (not (minibufferp)))

(bind-keys :map minibuffer-mode-map
           ;("C-d" . kill-word)
           ("C-b" . backward-kill-word))

(bind-keys*
 ("<C-tab>" . tab-next)
 ("<S-C-tab>" . tab-previous)
 ("M-t" . tab-new)
 ("C-." . embark-act)
 ("C-`" . my/shell-for-buffer)
 ("C-'" . my/jump-to-matching-char)
 ("C-\\" . other-frame)
 ("C-|" . tab-bar-switch-to-next-tab)
 ("C-a" . my/beginning-of-line)
 ("C-b" . backward-kill-word)
 ("C-f" . isearch-forward)
 ("S-C-f" . isearch-forward-regexp)
 ("C-j" . other-window)
 ("C-S-j" . ace-jump-word-mode)
 ("C-k" . my/kill-line-or-region)
 ("S-C-k" . my/copy-line-or-region)
 ("C-n" . execute-extended-command)
 ("C-o" . my/ffap-or-find-file)
 ("C-p" . my/shell)
 ("C-r" . vr/query-replace)
 ("C-s" . save-buffer)
 ;; ("C-t" . )
 ("C-v" . consult-buffer)
 ("C-w" . my/kill-current-buffer)
 ("C-y" . my/yank)
 ("C-z" . undo)
 ("C-c C" . org-capture)
 ("S-SPC" . yas-expand)
 ("C-c \\" . align-regexp)
 ("C-c a" . org-agenda)
 ("C-c b" . rename-buffer)
 ("C-c c" . calendar)
 ("C-c f" . my/rg-from-repo-root)
 ("C-c F" . my/rg)
 ("C-c i" . my/change-inside-pair)
 ("C-c j" . ace-jump-word-mode)
 ("C-c k" . ace-jump-line-mode)
 ("C-c l" . toggle-truncate-lines)
 ("C-c m" . imenu)

 ("C-c I" . my/find-matching-indentation-level)

 ("C-S-<up>" . backward-paragraph)
 ("C-S-<down>" . forward-paragraph)

 ("C-c o" . my/occur)
 ("C-c p" . my/copy-full-path)
 ("C-c q" . quoted-insert)
 ("C-c r" . consult-recent-file)
 ("C-c s" . sort-lines)
 ("C-c v" . my/new-buffer)
 ("C-c V" . my/new-buffer-from-clip)
 ("C-c O" . open-line)
 ("C-c C-l" . my/toggle-auto-hscroll-mode)
 ("<C-backspace>" . subword-backward-kill)

 ("M-`" . my/shell-for-buffer) ; works under both guis and terminals
 ("M-0" . delete-window)
 ("M-1" . delete-other-windows)
 ("M-2" . split-window-vertically)
 ("M-3" . split-window-horizontally)
 ("<M-SPC>" . my/just-one-space)
 ("M-;" . my/comment-line-or-region)
 ("M-<up>" . scroll-down)
 ("M-<down>" . scroll-up)
 ("M-D" . my/duplicate-line-or-region)
 ("M-E" . mc/edit-lines)
 ("M-\\" . my/delete-horizontal-space)
 ("M-d" . my/dired)
 ("C-x g" . magit-status)
 ("M-k" . my/kill-whole-line)
 ("M-o" . project-find-file)
 ("M-r" . vr/mc-mark)

 ("M-y" . consult-yank-pop)

 ("<home>" . beginning-of-buffer)
 ("<end>" . end-of-buffer)

 ;("C-x C-f" . consult-find-)

 ("C-x b" . ibuffer)
 ("C-x C-b" . ibuffer)
 ("C-x c" . compile)
 ("M-g" . consult-goto-line)
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
           ("d" . magit-file-dispatch)
           ("b" . magit-blame-addition)
           ("f" . magit-log-buffer-file)
           ("r" . my/git-grep-from-root)
           ("g" . consult-git-grep))

(when (eq system-type 'darwin)
  ;; Command-<enter>
  (bind-key* "<s-return>" 'toggle-frame-fullscreen))

(define-key emacs-lisp-mode-map (kbd "C-x x") 'eval-defun)

'(when window-system
  (define-key input-decode-map [?\C-\[] [C-\[])
  (global-set-key (kbd "<C-[>") 'shell)
  (define-key input-decode-map [?\C-\[] [C-\[])
  (global-set-key ()))

(defhydra my/win-hydra (override-global-map "C-c <left>")
  ("<left>" winner-undo)
  ("<right>" winner-redo))
