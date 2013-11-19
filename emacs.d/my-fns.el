;;; -*- lexical-binding: t -*-
;;; This file uses lexical binding for the
;;; function `my-overwrite-key-bindings-in-mode'.

(defvar my-change-inside-pair-overlay nil)
(make-variable-buffer-local 'my-change-inside-pair-overlay)

(defun my-change-inside-pair-unhighlight ()
  (delete-overlay my-change-inside-pair-overlay))

;; This shows a way to briefly highlight a region.
;; This done using the run-at-time function.
;; But that function can't delay execution depending
;; on what emacs is doing. See Emacs's compile.el
;; and search for pre-command-hook. It adds a pre-command-hook
;; that cancels the stored timer if execution of run-at-time
;; takes too long. And function remove itself from the pre-command-hook
;; after that.
(defun my-change-inside-pair (arg)
  (interactive "P")
  (let* ((start-string (format "%c" (read-event)))
         (end-string (or (cdr (assoc start-string '(("(" . ")")
                                                    ("{" . "}")
                                                    ("[" . "]")
                                                    ("<" . ">"))))
                         start-string))
         (start nil)
         (end nil))
    (save-excursion
      (when (search-forward start-string nil t)
        (setq start (point))
        (when (search-forward end-string nil t)
          (setq end (1- (point))))))
    (cond ((null start) (message "Couldn't find starting `%s'" start-string))
          ((null end) (message "Couldn't find ending `%s'" end-string))
          (arg (kill-ring-save start end)
               ;; Briefly highlight the copied region if its visible
               ;; to the user.
               (when (and (pos-visible-in-window-p start (selected-window))
                          (pos-visible-in-window-p end (selected-window)))
                 (when (null my-change-inside-pair-overlay)
                   (setq my-change-inside-pair-overlay (make-overlay 0 0))
                   (overlay-put my-change-inside-pair-overlay
                                'face 'isearch))
                 (move-overlay my-change-inside-pair-overlay
                               start
                               end
                               (current-buffer))
                 (run-at-time 0.3 nil 'my-change-inside-pair-unhighlight))
               (message "Copied `%s'"
                        (buffer-substring-no-properties start end)))
          (t (goto-char end)
             (delete-region start end)))))

(defun my-kill-line-or-region (&optional arg)
  (interactive "P")
  (if (region-active-p)
      (kill-region (point) (mark))
    (kill-line arg)))

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

(defun my-find-tag-next ()
  (interactive)
  (find-tag nil t nil))

(defun my-kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

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

;; Since I have non-standard key bindings (C-j for other-window),
;; it clashes with some major modes that overrides that key.
;; For example, lisp-interaction-mode binds C-j to eval-print-last-sexp.
;; Now when I override that key, I would like to see what
;; function was shadowed.
;; CAUTION: this needs Emacs 24's lexical scoping to work.
(defun my-overwrite-key-bindings-in-mode (key new-fn modes)
  (dolist (mode modes)
    (let ((hook (intern (format "%s-hook" mode)))
          (map (intern (format "%s-map" mode)))
          (msg (format "%s was bound to `%%s'; it now runs `%s'" key new-fn)))
      (add-hook hook
                (lambda ()
                  (let ((current-binding (key-binding (kbd key) t)))
                    (unless (eq new-fn current-binding)
                      (define-key (symbol-value map) (kbd key)
                        (lambda ()
                          (interactive)
                          (when (symbolp current-binding)
                            (message msg current-binding))
                          (call-interactively new-fn)
                          (define-key (symbol-value map)
                            (kbd key) new-fn))))))))))

(defun my-switch-to-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defvar my-yank-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "y") 'yank-pop)
    map))

(defun my-yank (arg)
  (interactive "*P")
  (yank arg)
  (unless (window-minibuffer-p)
    (message "Press `y' to yank-pop"))
  (set-temporary-overlay-map my-yank-keymap
                             (lambda ()
                               (memq this-command
                                     '(yank-pop cua-paste-pop)))))

(defun my-quick-hotkey ()
  "Temporarily bind a key to a hotkey.
Key can be any key that invokes a command.  Hotkey is a single
key. Any other key other than the hotkey exits this mode."
  (interactive)
  (let* ((cmd-key (read-key-sequence "Command key: " nil t))
         (cmd (intern-soft (key-binding cmd-key))))
    (if (null cmd)
        (message "No command associated with key `%s'" cmd-key)
      (let* ((prompt (format "Hot key to run `%s': " cmd))
             (hotkey (read-key prompt))
             (hotkey-string (format (if (numberp hotkey) "%c" "<%s>") hotkey))
             (map (make-sparse-keymap)))
        (define-key map (kbd hotkey-string) cmd)
        (call-interactively cmd)
        (set-temporary-overlay-map map t)
        (unless (window-minibuffer-p)
          (with-temp-message (format "`%s' will run the command `%s'"
                                     hotkey-string cmd)
            (sit-for 1.0)))))))

(defun my-count-lines-buffer ()
  (interactive)
  (message "%d lines" (count-lines (point-min) (point-max))))

(defun my-just-one-space ()
  "Like just-one-space, but moves across newlines."
  (interactive)
  (when (eolp)
    (delete-region (point)
                   (save-excursion
                     (skip-chars-forward " \t\n\r")
                     (point))))
  (call-interactively 'just-one-space))
