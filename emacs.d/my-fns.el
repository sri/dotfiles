(defvar my-change-inside-pair-overlay nil)
(make-variable-buffer-local 'my-change-inside-pair-overlay)

(defun my-change-inside-pair-unhighlight ()
  (delete-overlay my-change-inside-pair-overlay))

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
    (cond ((null start) (message "Couldn't find starting `%s'" key))
          ((null end) (message "Couldn't find ending `%s'" key))
          (arg (kill-ring-save start end)
               (when (null my-change-inside-pair-overlay)
                 (setq my-change-inside-pair-overlay (make-overlay 0 0))
                 (overlay-put my-change-inside-pair-overlay
                              'face '(:background "gray")))
               ;; Briefly highlight the copied region if its visible
               ;; to the user.
               (when (and (pos-visible-in-window-p start (selected-window))
                          (pos-visible-in-window-p end (selected-window)))
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

(defun my-mouse-ctrl-click (event)
  (interactive "e")
  (mouse-minibuffer-check event)
  (unless (one-window-p t)
    (let ((window (posn-window (event-start event))))
      (select-window (if (framep window)
                         (frame-selected-window window)
                       window))
      (delete-other-windows))))

(defun my-find-tag-next ()
  (interactive)
  (find-tag nil t nil))

(defun my-kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))


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

;; Since I have non-standard key bindings (C-j for other-window),
;; it clashes with some major modes that overrides that key.
;; For example, lisp-interaction-mode binds C-j to eval-print-last-sexp.
;; Now when I override that key, I would like to see what
;; function was shadowed.

(defvar my-overwrite-key-bindings-in-mode-alist nil)
(make-variable-buffer-local 'my-overwrite-key-bindings-in-mode-alist)

(defun my-overwrite-key-bindings-in-mode (key new-fn modes)
  (dolist (mode modes)
    (let ((hook (intern (format "%s-hook" mode)))
          (map (intern (format "%s-map" mode)))
          (msg (format "%s was bound to `%%s' but is now bound to `%s'" key new-fn)))
      (add-hook hook `(lambda ()
                        (when (null my-overwrite-key-bindings-in-mode-alist)
                          (setq my-overwrite-key-bindings-in-mode-alist
                                (cons nil (key-binding (kbd ,key) t))))
                        (define-key ,map (kbd ,key)
                          (lambda ()
                            (interactive)
                            (when (null (car my-overwrite-key-bindings-in-mode-alist))
                              (message ,msg (cdr my-overwrite-key-bindings-in-mode-alist))
                              (setcar my-overwrite-key-bindings-in-mode-alist t))
                            (call-interactively ',new-fn))))))))

(defun my-switch-to-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
