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
          (t
           (goto-char end)
           (delete-region start end)))))

(defun my-kill-line-or-region (&optional arg)
  (interactive "P")
  (if (use-region-p)
      (kill-region (point) (mark))
    (kill-line arg)))

(defun my-hippie-tab (arg)
  "Hippie expand, do what I mean.
If in the middle of `hippie-expand' running thru all the
expansions (see `hippie-expand-try-functions-list'), then
continue with that. If a region is selected, indent that region.
If at the beginning of the line, call `indent-for-tab-command'.
Othewise, invoke `hippie-expand'."
  (interactive "*P")
  (cond ((eq last-command 'hippie-expand)
         (hippie-expand arg))
        ((and transient-mark-mode (use-region-p))
         (indent-region (region-beginning) (region-end) nil))
        ((let ((cs (char-syntax (preceding-char))))
           ;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html#Syntax-Class-Table
           (or (= cs ?w) (= cs ?\_)))
         (hippie-expand arg))
        (t
         (indent-for-tab-command))))

(defun my-kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

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
  (set-transient-map my-yank-keymap
                             (lambda ()
                               (memq this-command
                                     '(yank-pop cua-paste-pop)))))

(defun my-count-lines-buffer ()
  (interactive)
  (message "%d lines" (count-lines (point-min) (point-max))))

(defun my-just-one-space (&optional arg)
  "Like just-one-space, but moves across newlines."
  (interactive "*P")
  (just-one-space (if arg nil -1)))

(defun my-delete-horizontal-space (&optional arg)
  (interactive "*P")
  (if arg
      (delete-horizontal-space)
    (delete-region (progn (skip-chars-backward " \t\n\r")
                          (point))
                   (progn (skip-chars-forward " \t\n\r")
                          (point)))))

(defun my-kill-whole-line (&optional arg)
  "Like kill-whole-line but maintains column position."
  (interactive "p")
  (let ((col (current-column)))
    (kill-whole-line arg)
    (move-to-column col)))

(defun my-url-decode (&optional arg)
  "Decode the URL.
If a region is selected and the universal argument (C-u) is prefixed,
then the region is replaced with the decoded URL. Otherwise, show the
decoded URL in the minibuffer."
  (interactive "P")
  (let* ((region-active (use-region-p))
         (url (if region-active
                  (buffer-substring-no-properties (point) (mark))
                (read-string "Url: ")))
         (decoded (url-unhex-string url)))
    (cond ((and region-active arg)
           (delete-region (point) (mark))
           (insert decoded))
          (t (message "%s" decoded)))))

(defun my-beginning-of-line ()
  "Move to the beginning of line or beginning of non-whitespace chars."
  (interactive "^")
  (let ((indentation-start (save-excursion
                             (back-to-indentation)
                             (point))))
    (if (or (= (current-column) 0)
            (> (point) indentation-start))
        (goto-char indentation-start)
      (beginning-of-line))))

(require 'ffap)

(defun my-open-project-file-or-find-files ()
  (call-interactively
   (if (helm-ls-git-root-dir)
       'helm-ls-git-ls
     'helm-find-files)))

(defun my-ffap-or-find-file (arg)
  "Find the file at point or ask the user for file's path.
The latter method uses `helm-find-files'."
  (interactive "P")
  (if arg
      (my-open-project-file-or-find-files)
    (let (file)
      (unless (memq major-mode '(dired-mode))
        (setq file (ffap-file-at-point)))
      (if file
          (let ((linenum
                 (save-excursion
                   (goto-char (point-at-bol))
                   (when (and (search-forward file (point-at-eol) t 1)
                              (looking-at ":\\([0-9]+\\)"))
                     (string-to-number (buffer-substring-no-properties
                                        (match-beginning 1)
                                        (match-end 1)))))))
            (find-file file)
            (when linenum
              (goto-line linenum)
              (linum-mode 1)
              (recenter)))
        ;; No file at point
        (my-open-project-file-or-find-files)))))

(defun my-remove-non-ascii-chars ()
  (interactive)
  (query-replace-regexp "[^[:ascii:]]" ""))

(defun my-open-latest-downloaded-file ()
  (interactive)
  (let (downloads)
    (dolist (f (directory-files "~/Downloads" 'full nil 'nosort))
      (unless (member (file-name-nondirectory f) '("." ".."))
        (push (cons f (nth 5 (file-attributes f))) downloads)))
    (setq downloads
          (sort downloads (lambda (x y) (time-less-p (cdr y) (cdr x)))))
    (when downloads
      (find-file (caar downloads)))))

(require 'rect) ; for killed-rectangle
(defun my-copy-from-starting-col-till-eol (start end &optional evenly-sized-strings)
  "Copy from starting column till end of line for all lines in region.
With a prefix argument, makes all the copied lines the same
length -- spaces are appended to lines that aren't long enough.
Sets the result to `killed-rectangle', so that a `yank-rectangle'
will bring it back."
  (interactive "r\nP")
  (when (use-region-p)
    (let ((lines '())
          (line nil)
          (max 0)
          (done nil)
          (start-column nil))
      (save-excursion
        (goto-char start)
        (setq start-column (current-column))
        ;; Don't include the last line unless the
        ;; cursor is at the end of the line.
        (while (and (not done)
                    (<= (point-at-eol) end))
          (if (< (current-column) start-column)
              (push "" lines)
            (setq line (buffer-substring (point) (point-at-eol)))
            (setq max (max (length line) max))
            (push line lines))
          (forward-line 1)
          (when (eobp) (setq done t))
          (move-to-column start-column))
        (setq lines (nreverse lines))
        (setq killed-rectangle
              (if evenly-sized-strings
                  (mapcar (lambda (s)
                            (concat s (make-string (- max (length s)) ? )))
                          lines)
                lines))
        (setq deactivate-mark t)
        (message "Invoke `yank-rectangle' (%s) to get this rectangle"
                 (mapconcat 'key-description
                            (where-is-internal 'yank-rectangle) ", "))))))

(defun my-unsaved-changes ()
  (interactive)
  (diff-buffer-with-file (current-buffer)))

(defun my-find-file-as-sudo ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

(defun my-occur ()
  (interactive)
  (call-interactively
   (if (eq major-mode 'org-mode)
       'org-sparse-tree
     'occur)))

(defun my-pp-json ()
  (interactive)
  (shell-command-on-region (point-min)
                           (point-max)
                           "python -mjson.tool"
                           (current-buffer)
                           t))

(defun my-toggle-camel-case-and-underscore ()
  (interactive)
  (when-let ((bounds (bounds-of-thing-at-point 'sexp)))
    (let* ((word (buffer-substring-no-properties (car bounds)
                                                 (cdr bounds)))
           (underscore (string-match "_" word))
           (camelcase (let (case-fold-search) (string-match "[A-Z]" word))))

      (when (or underscore camelcase)
        (let ((original-col (current-column))
              (result (if underscore
                          ;; Convert underscore to camel case:
                          (let ((parts (split-string word "_")))
                            (concat (car parts)
                                    (mapconcat #'capitalize (cdr parts) "")))
                        ;; Convert camel case to underscore:
                        (let* ((case-fold-search nil)
                               (str (replace-regexp-in-string
                                     "\\([A-Z]\\)"
                                     (lambda (x) (concat "_" (downcase x)))
                                     word
                                     t)))
                          (if (string-match "^_" str)
                              (substring str 1)
                            str)))))

        (delete-region (car bounds) (cdr bounds))
        (insert result)
        (move-to-column original-col))))))

(defun my-find-file-in-other-window (&optional arg)
  (interactive "P")
  (cond ((not arg)
         (split-window-right)
         (windmove-right))
        (t
         (split-window-below)
         (windmove-down)))
  (my-open-project-file-or-find-files))

(defun my-frame-transparency (arg)
  (interactive "p")
  (set-frame-parameter nil 'alpha (list arg arg)))

(defun my-copy-full-path ()
  "Copies the buffer name to the kill ring.
If the current buffer isn't associated with a file and the major
mode is either Shell or Magit, then the current directory is
copied."
  (interactive)
  (let ((name (or (buffer-file-name)
                  (and (or (memq major-mode '(shell-mode dired-mode))
                           (string-prefix-p "Magit" mode-name))
                       default-directory))))
    (setq name (expand-file-name name))
    (if (not name)
        (message "Nothing to copy")
      (kill-new name)
      (message "Copied `%s'" name))))

(defun my-git-grep-from-root ()
  (interactive)
  (helm-grep-do-git-grep 1))
