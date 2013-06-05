(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;(require 'filladapt)

(setq filladapt-mode-line-string nil)
(setq-default filladapt-mode t)

(set-register ?e '(file . "~/.emacs"))

(defun my-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(let ((trailing-whitespace-mode-hooks
       '(ruby-mode-hook python-mode-hook
                        c-mode-hook
                        c++-mode-hook
                        js-mode-hook
                        java-mode-hook
                        emacs-lisp-mode-hook)))
  (dolist (hook trailing-whitespace-mode-hooks)
    (add-hook hook 'my-show-trailing-whitespace)))

; (setq-default visual-line-mode t)
(setq-default indent-tabs-mode nil)
(setq make-backup-files nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "sri")
(setq initial-scratch-message nil)
(setq visible-bell nil)
(setq ring-bell-function (lambda ()))
(setq disabled-command-hook nil)
(setq kill-whole-line t)
;; Help char is `?' so C-x ? will list all the
;; keys bound to C-x.
(setq help-char ??)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

(put 'erase-buffer 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode t)
(column-number-mode t)
(blink-cursor-mode -1)
(auto-compression-mode t)
(transient-mark-mode 1)
(show-paren-mode t)
(ido-mode 1)
;(auto-revert-mode 1)
(global-hl-line-mode 1)
(electric-pair-mode 1)
;(outline-minor-mode 1)

(desktop-save-mode 1)
(add-to-list 'desktop-path "~/.emacs.d/desktop")


(require 'dired-x)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map [left] 'dired-up-directory)
            (define-key dired-mode-map [right] 'dired-find-file)))

(defun my-kill-line-or-region (&optional arg)
  (interactive "P")
  (if (region-active-p)
      (kill-region (point) (mark))
    (kill-line arg)))

;; Key bindings

;; Use arrows & PageDown/PageUp for navigation

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'hippie-exp)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

(defun my-hippie-tab (arg)
  (interactive "*P")
  (cond ((and transient-mark-mode (region-active-p))
         (indent-region (region-beginning) (region-end) nil))
        ((and (eq (char-syntax (preceding-char)) ?w)
              (not (zerop (current-column))))
         (hippie-expand arg))
        (t
         (indent-for-tab-command))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-mouse-ctrl-click (event)
  (interactive "e")
  (mouse-minibuffer-check event)
  (unless (one-window-p t)
    (let ((window (posn-window (event-start event))))
      (select-window (if (framep window)
                         (frame-selected-window window)
                       window))
      (delete-other-windows))))

(global-set-key [C-down-mouse-1] 'mouse-delete-other-windows)

(defun my-find-tag-next ()
  (interactive)
  (find-tag nil t nil))

(defun my-kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun my-shell-forward-char-or-previous-history (&optional arg)
  (interactive "p")
  (if (eobp)
      (comint-previous-input arg)
    (forward-char arg)))

(defun my-shell-next-line-or-next-history (&optional arg)
  (interactive "p")
  (if (eobp)
      (comint-next-input arg)
    (next-line arg)))

(make-variable-buffer-local
 'line-number-mode)

(defun my-shell-erase-buffer ()
  (interactive)
  (erase-buffer)
  (comint-send-input))

(add-hook 'shell-mode-hook
          (lambda ()
            (setq line-number-mode nil
                  column-number-mode nil)
            (setq comint-input-sender
                  'my-emacs-rspec-command)
            (toggle-truncate-lines 1)
            ;(buffer-disable-undo)
            (define-key shell-mode-map (kbd "C-<up>")
              'comint-previous-prompt)
            (define-key shell-mode-map (kbd "C-<down>")
              'comint-next-prompt)
            (define-key shell-mode-map (kbd "C-c e")
              'my-shell-erase-buffer)
            (define-key shell-mode-map (kbd "<right>")
              'my-shell-forward-char-or-previous-history)
            (define-key shell-mode-map (kbd "<down>")
              'my-shell-next-line-or-next-history)))

(add-hook 'view-mode-hook
          (lambda ()
            (define-key view-mode-map (kbd "<up>")
              'scroll-down)
            (define-key view-mode-map (kbd "<down>")
              'scroll-up)))

(global-font-lock-mode t)

(setq linum-format " %d ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-packages
  '(color-theme color-theme-solarized magit bm))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(let ((missing '()))
  (dolist (p my-packages)
    (unless (package-installed-p p)
      (push p missing)))
  (when missing
    (package-refresh-contents)
    (dolist (p missing)
      (package-install p))))

(load-theme 'solarized-light t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bm)
(setq bm-highlight-style 'bm-highlight-only-fringe)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>") 'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
(global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

(require 'magit)
(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)

;; Some Sublime Text-isms:

(defun my-sublime-like-mouse-dblclick-select-fn ()
  (let ((isearch-word t)
        (isearch-forward t)
        (beg (min (mark) (point)))
        (string (buffer-substring-no-properties (mark) (point))))
    (unless (string-match "^\n*$" string)
      (deactivate-mark)
      (save-excursion
        (call-interactively 'isearch-forward)
        (goto-char beg)
        (isearch-yank-string string)))))

(defun my-isearch-forward ()
  (interactive)
  (if (and transient-mark-mode (region-active-p))
      (my-sublime-like-mouse-dblclick-select-fn)
    (call-interactively 'isearch-forward)))

(setq isearch-allow-scroll t)

(define-key isearch-mode-map (kbd "<return>")
  'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<S-return>")
  'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<backspace>")
  'my-isearch-delete-region)

(defun my-isearch-delete-region ()
  (interactive)
  (when isearch-other-end
    (delete-region (point) isearch-other-end)
    (isearch-done)))

(setq isearch-lazy-highlight-initial-delay 0)

(require 'advice)

(defadvice mouse-drag-region (after my-sublime-like-mouse-select (start-event))
  (when (= (event-click-count start-event) 2)
    (my-sublime-like-mouse-dblclick-select-fn)))

(ad-activate 'mouse-drag-region)

(defun my-sublime-expand-selection-to-indentation ()
  (interactive)
  "Expand selection to the next indentation level.
Inspired by Sublime Text."
  (let ((n (current-indentation))
        (beg (point-at-bol))
        (end (point-at-eol)))
    ;; when region is active & transient mark mode is
    ;; turned on, we expand to make that region bigger
    (when (and (region-active-p) transient-mark-mode)
      (setq beg (region-beginning)
            end (region-end))
      (save-excursion
        ;; get the min indentation within the region
        (goto-char beg)
        (forward-line 1)
        (while (< (point) end)
          (setq n (min n (current-indentation)))
          (forward-line 1))
        ;; get the min indentation of line before
        ;; region start, line after region start or n
        (setq n
              (max (progn
                     (goto-char beg)
                     (forward-line -1)
                     (if (bobp) 0 (current-indentation)))
                   (progn
                     (goto-char end)
                     (forward-line 1)
                     (if (eobp) 0 (current-indentation)))))))
    ;; now expand the region
    (save-excursion
      (goto-char beg)
      (forward-line -1)
      (while (and (>= (current-indentation) n) (not (bobp)))
        (forward-line -1))
      (forward-line 1)
      (setq beg (point-at-bol))
      (goto-char end)
      (forward-line 1)
      (while (and (>= (current-indentation) n) (not (eobp)))
        (forward-line 1))
      (forward-line -1)
      (setq end (point-at-eol)))
    (goto-char beg)
    (set-mark beg)
    (goto-char end)))

;; Miscellaneous functions:

(defun my-toggle-fullscreen ()
  (interactive)
  (cond ((eq window-system 'x)
         (let ((fullp (frame-parameter nil 'fullscreen)))
           (set-frame-parameter nil 'fullscreen
                                (if fullp nil 'fullscreen))))))

;; If there is a visible "rspec" (*_spec.rb) buffer in the
;; current frame, running a "rspec" command is a shell process,
;; will run rspec against that buffer's file name (at the line
;; where the cursor is in that buffer). Add this to your shell-mode-hook
;; to enable this feature:
;;     (add-hook 'shell-mode-hook
;;               (lambda ()
;;                 (setq comint-input-sender 'my-emacs-rspec-command)))
;;
(defun my-emacs-rspec-command (proc string)
  (when (string-match "^rspec\n?$" string)
    (let ((buffers (mapcar #'window-buffer (window-list)))
          (spec-buffer nil))
      (dolist (buf buffers)
        (when (string-match "_(spec|test)[.]rb$" (or (buffer-file-name buf) ""))
          (setq spec-buffer buf)))
      (when spec-buffer
        (let ((n (with-current-buffer spec-buffer
                   (line-number-at-pos))))
          (setq string (format "rspec %s:%d"
                               (buffer-file-name spec-buffer)
                               n))
          (message "Running \"%s\"" string)))))
  (comint-simple-send proc string))

(defun my-transpose-buffers (&optional arg)
  (interactive "p")
  (let* ((windows (window-list nil 'never-minibuffer))
         (selected (pop windows))
         (selected-buffer (window-buffer selected)))
    (when (< arg 0)
      (setq windows (reverse windows)))
    (dotimes (i (length windows))
      (switch-to-buffer (window-buffer (pop windows)))
      (other-window arg))
    (switch-to-buffer selected-buffer)
    (other-window arg)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130))))
 '(bm-fringe-face ((t (:foreground "#859900")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line hacking

(defmacro def-with-selected-window (name &rest body)
  `(defun ,name (event)
     (interactive "e")
     (with-selected-window (posn-window (event-start event))
       ,@body)))

(def-with-selected-window my-mode-line-kill-file-full-path
  (let ((full (buffer-file-name)))
    (when full
      (kill-new full)
      (message "Copied: `%s'" full))))

(defvar my-mode-line-buffer-identification-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'my-mode-line-kill-file-full-path)
    (define-key map [header-line down-mouse-1] 'ignore)
    (define-key map [header-line mouse-1] 'my-mode-line-kill-file-full-path)
    (define-key map [mode-line mouse-3] 'ignore)
    (define-key map [header-line down-mouse-3] 'ignore)
    (define-key map [header-line mouse-3] 'ignore)
    map))

(defun my-mode-line-buffer-identification-help-echo (window object point)
  (let ((buffer (window-buffer window)))
    (with-current-buffer buffer
      (format "%s\n%s\nmouse-1: Copy file path to kill ring"
              (buffer-name buffer)
              (buffer-file-name buffer)))))

(make-face 'my-mode-line-buffer-name-face)
(set-face-attribute 'my-mode-line-buffer-name-face nil
    :inherit 'mode-line-faced
    ;:foreground "#4271ae"
    ;;:height 90
    ;:box '(:line-width 2 :color "#4271ae")
    )

(defun my-mode-line-buffer-name ()
  (let* ((boxsize 30)
         (bn (buffer-name))
         (len (length bn)))
    (if (> len boxsize)
        (concat (substring bn 0 (- boxsize 3)) "...")
      (concat bn (make-string (- boxsize len) ? )))))

(setq-default mode-line-buffer-identification
              `(:propertize (:eval (my-mode-line-buffer-name))
                           face my-mode-line-buffer-name-face
                           help-echo my-mode-line-buffer-identification-help-echo
                           mouse-face mode-line-highlight
                           local-map ,my-mode-line-buffer-identification-keymap))

(defvar my-mode-line-buffer-modified-p-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'save-buffer)
    (define-key map [mode-line down-mouse-1] 'ignore)
    map))

(setq-default mode-line-modified
              `(:propertize (:eval (if (buffer-modified-p) "•" " "))
                            help-echo "mouse-1: Save buffer"
                            mouse-face mode-line-highlight
                            local-map ,my-mode-line-buffer-modified-p-keymap))

(def-with-selected-window my-mode-line-beginning-or-end-of-buffer
  (if (bobp)
      (goto-char (point-max))
    (goto-char (point-min))))

(def-with-selected-window my-mode-line-scroll-up
  (scroll-up))

(def-with-selected-window my-mode-line-scroll-down
  (scroll-down))

(setq my-mode-line-buffer-percentage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'my-mode-line-scroll-up)
    (define-key map [mode-line S-mouse-1] 'my-mode-line-scroll-down)
    (define-key map [mode-line C-mouse-1] 'my-mode-line-beginning-or-end-of-buffer)
    (define-key map [mode-line down-mouse-1] 'ignore)
    map))

(def-with-selected-window my-mode-line-goto-line
  (let ((n (read-number "Goto line: ")))
    (goto-line n)))

(setq my-mode-line-column-line-number-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map [mode-line mouse-1] 'my-mode-line-goto-line)
        (define-key map [mode-line down-mouse-1] 'ignore)
        map))

(setq-default mode-line-position
  `((-3 ,(propertize
          "%p"
          'local-map my-mode-line-buffer-percentage-mode-map
          'mouse-face 'mode-line-highlight
          'help-echo "mouse-1: scroll up\nShift mouse-1: scroll down
Ctrl mouse-1: toggle between Beginning & End of buffer"))
    (line-number-mode
     ((column-number-mode
       (10 ,(propertize
	     " (L%l,C%c)"
	     'local-map my-mode-line-column-line-number-mode-map
	     'mouse-face 'mode-line-highlight
	     'help-echo "mouse-1: goto line"))
       (6 ,(propertize
	    " L%l"
	    'local-map my-mode-line-column-line-number-mode-map
	    'mouse-face 'mode-line-highlight
	    'help-echo "mouse-1: goto line"))))
     ((column-number-mode
       (5 ,(propertize
	    " C%c"
	    'local-map my-mode-line-column-line-number-mode-map
	    'mouse-face 'mode-line-highlight
	    'help-echo "mouse-1: goto line")))))))

(make-variable-buffer-local 'mode-line-position)



(def-with-selected-window my-mode-line-window-split-right
  (split-window-right))

(def-with-selected-window my-mode-line-window-split-below
  (split-window-below))

(def-with-selected-window my-mode-line-window-delete
  (delete-window))

(def-with-selected-window my-mode-line-window-delete-other-windows
  (delete-other-windows))

(defun my-make-mode-line-mouse-map (&rest args)
  (let ((map (make-sparse-keymap)))
    (while args
      (define-key map (vector 'mode-line (pop args)) (pop args)))
    map))


(setq-default my-mode-line-window-manipulation
  (list (propertize "[⇨]"
                    'mouse-face 'mode-line-highlight
                    'help-echo "split window right"
                    'local-map (my-make-mode-line-mouse-map
                                'down-mouse-1 #'ignore
                                'mouse-1 #'my-mode-line-window-split-right))
        (propertize "[⇩]"
                    'mouse-face 'mode-line-highlight
                    'help-echo "split window below"
                    'local-map (my-make-mode-line-mouse-map
                                'down-mouse-1 #'ignore
                                'mouse-1 #'my-mode-line-window-split-below))
        (propertize "[×]" ; "₀"
                    'mouse-face 'mode-line-highlight
                    'help-echo "delete window"
                   'local-map (my-make-mode-line-mouse-map
                                'down-mouse-1 #'ignore
                                'mouse-1 #'my-mode-line-window-delete))
        (propertize "[1]" ; "₁"
                    'mouse-face 'mode-line-highlight
                    'help-echo "delete other windows"
                    'local-map (my-make-mode-line-mouse-map
                                'down-mouse-1 #'ignore
                                'mouse-1 #'my-mode-line-window-delete-other-windows))))

(make-variable-buffer-local 'my-mode-line-window-manipulation)
(put 'my-mode-line-window-manipulation 'risky-local-variable t)

(setq-default mode-line-format
              '(" "
                mode-line-modified " "
                my-mode-line-window-manipulation " "
                mode-line-buffer-identification " "
;                mode-line-modes " "
                mode-line-position
                (defining-kbd-macro " Def")))

(let ((private-emacs (expand-file-name "~/.emacs.private")))
  (when (file-exists-p private-emacs)
    (load-file private-emacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "")
