;; -*- lexical-binding: t; -*-
;; M-C-s was isearch-forward-regexp; its now S-C-f

(require 'subword)
(require 'bind-key)
(require 'repeat)

(repeat-mode 1)

;; https://www.reddit.com/r/emacs/comments/1uafbsn/underappreciated_emacs_builtins_hideshow_60/
(defvar my/hs-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'hs-cycle)
    (define-key map (kbd "TAB") #'hs-cycle)
    (define-key map (kbd "t") #'hs-toggle-hiding)
    (define-key map (kbd "h") #'hs-hide-block)
    (define-key map (kbd "s") #'hs-show-block)
    (define-key map (kbd "H") #'hs-hide-all)
    (define-key map (kbd "S") #'hs-show-all)
    (define-key map (kbd "a") #'hs-toggle-all)
    map)
  "Repeat map for hideshow commands.")

(dolist (command '(hs-cycle
                   hs-toggle-hiding
                   hs-hide-block
                   hs-show-block
                   hs-hide-all
                   hs-show-all
                   hs-toggle-all))
  (put command 'repeat-map 'my/hs-repeat-map))

(when (boundp 'vertico-map)
  (bind-keys :map vertico-map
             ("C-c ." . embark-act)
             ("C-j" . embark-select)
             ("C-x" . embark-export)
             ("ESC p" . consult-history)
             ("C-c q" . vertico-quick-insert)))

(require 'embark)
(bind-keys :map embark-region-map
           ("g" . my/google-search))

(bind-keys
 ;; Key which don't want to override in all modes.
 ;; For example, Magit does useful things with C-i
 ;; (TAB) -- show/hide file diffs.
 ("C-i" . my/hippie-tab)
;; ("<M-return>" . my/dired)
 ("C-m" . newline-and-indent)
 )

;; Mac trackpad
;; Problem with wheel (2-finger swipe on os x)
;; is that the even will keep call the function
;; over and over again.
;; (bind-keys*
;; ("<wheel-left>" . (lambda ()
;; (interactive)
;; (message "mouse wheel left")
;; )))

(bind-keys :map tab-bar-map
           ("<wheel-up>"      . ignore)
           ("<wheel-down>"    . ignore)
           ("<wheel-left>"    . ignore)
           ("<wheel-right>"   . ignore)
           ("S-<wheel-up>"    . ignore)
           ("S-<wheel-down>"  . ignore)
           ("S-<wheel-left>"  . ignore)
           ("S-<wheel-right>" . ignore)
           ("<down-mouse-3>"  . my/tab-bar-mouse-context-menu))

;; Apple CMD Key
(bind-keys*
 ("M-0" . delete-window)
 ("M-1" . delete-other-windows)
 ("M-2" . split-window-vertically)
 ("M-3" . split-window-horizontally)
 ("M-c" . my/copy-line-or-region)
 ("M-d" . my/dired)
 ("M-t" . tab-new)
 ("M-v" . consult-buffer)
 ;; ("M-`" . my/open-shell-window-for-buffer) ; works under both guis and terminals
 ("<M-SPC>" . my/just-one-space)
 ("M-;" . my/comment-line-or-region)
 ("M-<up>" . scroll-down)
 ("M-<down>" . scroll-up)
 ("M-D" . my/duplicate-line-or-region)
 ("M-E" . mc/edit-lines)
 ("M-\\" . my/delete-horizontal-space)
 ;; ("M-d" . my/dired)

 ("M-a" . beginning-of-buffer)
 ("M-e" . end-of-buffer)
 ("M-j" . ace-jump-word-mode)
 ("M-k" . my/kill-whole-line)
 ("M-o" . my/ffap-or-find-file)
 ("M-r" . vr/mc-mark)

 ("M-y" . consult-yank-pop)
 ("M-g" . consult-goto-line)
 )

;; https://emacs.stackexchange.com/questions/32183/how-to-make-exceptions-to-bind-key-overriding-behavior

(bind-key* "C-d" 'kill-word (not (minibufferp)))

(bind-keys :map minibuffer-mode-map
           ;("C-d" . kill-word)
           ("C-b" . backward-kill-word))

;; Ctrl-i
;; Ctrl-p
;; improve C-r
;; other unused:
;; - C-c d, C-c e, C-c h, C-c u, C-c w, C-c x, C-c y, C-c z
;;  - C-,, C-;, C-=
;;  - M-[, M-]



(bind-keys*
 ("C-t" . tab-next)
 ;; ("???" . tab-previous)
 ("C-c `" . my/open-shell-window-for-buffer)
 ("C-c '" . my/jump-to-matching-char)
 ("C-c x" . er/expand-region)
 ("C-a" . my/beginning-of-line)
 ("C-b" . backward-kill-word)
 ("C-f" . my/isearch)
 ("C-j" . other-window)
 ("C-k" . my/kill-line-or-region)
 ("C-n" . execute-extended-command)
 ("C-o" . my/ffap-or-find-file)
 ("C-p" . my/shell-switch-to-next-most-recent)
 ("C-r" . vr/query-replace)
 ("C-s" . save-buffer)
 ;; ("C-t" . )
 ("C-v" . consult-project-buffer)
  ("C-y" . my/yank)
 ("C-z" . undo)
 ("C-c ~" . my/open-repo-in-browser)
 ("C-c ." . embark-act) ;; aka, right click
 ("C-c C" . org-capture)
 ("S-SPC" . dabbrev-completion) ;;; PROBLEM
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
 ("C-c m" . my/imenu)
 ("C-c C-t" . my/frame-always-on-top-toggle)
 ;; when i accidentally have an input prompt
 ;; waiting for me in the minibuffer, but am
 ;; doing something else
 ("C-c Q" . (lambda ()
              (interactive)
              (when (active-minibuffer-window)
                (save-window-excursion
                  (abort-recursive-edit)))))


 ("C-c I" . my/find-matching-indentation-level)

 ("C-S-<up>" . backward-paragraph)
 ("C-S-<down>" . forward-paragraph)

 ("C-c n" . next-error)
 ("C-c N" . previous-error)
 ("C-c o" . my/occur)
 ("C-c p" . pi-coding-agent)
 ("C-c q" . quoted-insert)
 ("C-c r" . consult-recent-file)
 ("C-c s" . sort-lines)
 ("C-c t" . my/scratch-new-temp)
 ("C-c T" . my/scratch-new-temp-from-clipboard)
 ("C-c v" . my/scratch-new)
 ("C-c V" . my/scratch-new-from-clipboard)
 ("C-c O" . open-line)
 ("C-c C-l" . my/toggle-auto-hscroll-mode)
 ("C-c C-b" . subword-backward-kill)

 ("C-x g" . magit-status)

 ("<home>" . beginning-of-buffer)
 ("<end>" . end-of-buffer)

 ;("C-x C-f" . consult-find-)

 ("C-x b" . ibuffer)
 ("C-x C-b" . ibuffer)
 ("C-x c" . rceoompile)
 ("C-x l" . my/count-lines-buffer)
 ("C-x r K" . my/copy-from-starting-col-till-eol)
 ("C-x s" . my/start-line-or-region-swap)
 ("C-x u" . ace-swap-window)
 ("C-x v -" . my/unsaved-changes)
 ("C-x C-v" . my/find-file-as-sudo)
 ("C-x <up>" . windmove-up)
 ("C-x <down>" . windmove-down)
 ("C-x <left>" . windmove-left)
 ("C-x <right>" . windmove-right)
 )

;; Don't let global C-w kill the Pi prompt buffer.
(bind-key* "C-w" 'my/kill-current-buffer
           (not (derived-mode-p 'pi-coding-agent-input-mode)))

;; Git related under: C-c g <letter>
(bind-keys :prefix-map my/ctl-c-g-map
           :prefix "C-c g"
           ("l" . git-link)
           ("d" . magit-file-dispatch)
           ("b" . magit-blame-addition)
           ("f" . magit-log-buffer-file)
           ("r" . my/git-grep-from-root)
           ("v" . my/github-visit-file)
           ("g" . consult-git-grep))

(bind-key* "C-c h" #'hs-cycle)

(define-key emacs-lisp-mode-map (kbd "C-x x") 'eval-defun)

'(when window-system
  (define-key input-decode-map [?\C-\[] [C-\[])
  (global-set-key (kbd "<C-[>") 'shell)
  (define-key input-decode-map [?\C-\[] [C-\[])
  (global-set-key ()))

(defhydra my/win-hydra (override-global-map "C-c <left>")
  ("<left>" winner-undo)
  ("<right>" winner-redo))
