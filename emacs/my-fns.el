(defvar my/change-inside-pair-overlay nil)
(make-variable-buffer-local 'my/change-inside-pair-overlay)

(defun my/change-inside-pair-unhighlight ()
  (delete-overlay my/change-inside-pair-overlay))

;; This shows a way to briefly highlight a region.
;; This done using the run-at-time function.
;; But that function can't delay execution depending
;; on what emacs is doing. See Emacs's compile.el
;; and search for pre-command-hook. It adds a pre-command-hook
;; that cancels the stored timer if execution of run-at-time
;; takes too long. And function remove itself from the pre-command-hook
;; after that.
(defun my/change-inside-pair (arg)
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
                 (when (null my/change-inside-pair-overlay)
                   (setq my/change-inside-pair-overlay (make-overlay 0 0))
                   (overlay-put my/change-inside-pair-overlay
                                'face 'isearch))
                 (move-overlay my/change-inside-pair-overlay
                               start
                               end
                               (current-buffer))
                 (run-at-time 0.3 nil 'my/change-inside-pair-unhighlight))
               (message "Copied `%s'"
                        (buffer-substring-no-properties start end)))
          (t
           (goto-char end)
           (delete-region start end)))))

(defun my/kill-line-or-region (&optional arg)
  (interactive "P")
  (if (use-region-p)
      (kill-region (point) (mark))
    (kill-line arg)))

(defun my/hippie-tab (arg)
  "Hippie expand, do what I mean.
If in the middle of `hippie-expand' running thru all the
expansions (see `hippie-expand-try-functions-list'), then
continue with that. If a region is selected, indent that region.
If at the beginning of the line, call `indent-for-tab-command'.
Othewise, invoke `hippie-expand'."
  (interactive "*P")
  (cond ((eq last-command 'hippie-expand)
         (hippie-expand arg))
        ((and transient-mark-mode
              (use-region-p))
         (indent-region (region-beginning)
                        (region-end)
                        nil))
        ((let ((cs (char-syntax (preceding-char))))
           ;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html#Syntax-Class-Table
           (or (= cs ?w) (= cs ?\_)))
         (hippie-expand arg))
        (t
         (indent-for-tab-command))))

(defun my/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (if (eq major-mode 'dired-sidebar-mode)
      (dired-sidebar-toggle-with-current-directory)
    (kill-buffer (current-buffer))))

(defun my/switch-to-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defvar my/yank-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "y") 'yank-pop)
    map))

(defun my/yank (arg)
  (interactive "*P")
  (yank arg)
  (unless (window-minibuffer-p)
    (message "Press `y' to yank-pop"))
  (set-transient-map my/yank-keymap
                     (lambda ()
                       (memq this-command
                             '(yank-pop cua-paste-pop)))))

(defun my/count-lines-buffer ()
  (interactive)
  (message "%d lines" (count-lines (point-min) (point-max))))

(defun my/just-one-space (&optional arg)
  "Like just-one-space, but moves across newlines."
  (interactive "*P")
  (just-one-space (if arg nil -1)))

(defun my/delete-horizontal-space (&optional arg)
  (interactive "*P")
  (if arg
      (delete-horizontal-space)
    (delete-region (progn (skip-chars-backward " \t\n\r")
                          (point))
                   (progn (skip-chars-forward " \t\n\r")
                          (point)))))

(defun my/kill-whole-line (&optional arg)
  "Like kill-whole-line but maintains column position."
  (interactive "p")
  (let ((col (current-column)))
    (kill-whole-line arg)
    (move-to-column col)))

(defun my/url-decode (&optional arg)
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

(defun my/beginning-of-line ()
  "Move to the beginning of line or beginning of non-whitespace chars."
  (interactive "^")
  (cond ((and (boundp 'multiple-cursors-mode)
              multiple-cursors-mode)
         (beginning-of-line))
        ((eq major-mode 'org-mode)
         (org-beginning-of-line))
        (t
         (let ((indentation-start (save-excursion
                                    (cond ((derived-mode-p 'magit-mode)
                                           (beginning-of-line)
                                           (if (looking-at "[+-]") (forward-char 1))
                                           (skip-chars-forward " \t"))
                                          (t
                                           (back-to-indentation)))
                                    (point))))
           (if (or (= (current-column) 0)
                   (> (point) indentation-start))
               (goto-char indentation-start)
             (beginning-of-line))))))

(require 'ffap)

(defun my/open-project-file-or-find-files ()
  (call-interactively 'project-find-file))

(defun my/ffap-or-find-file (arg)
  "Find the file at point or ask the user for file's path."
  (interactive "P")
  (if (or arg
          (memq major-mode '(dired-mode)))
      (my/open-project-file-or-find-files)
    (let ((file (ffap-file-at-point)))
      (if (not file)
          (my/open-project-file-or-find-files)
        (let (col line)
          (save-excursion
            (goto-char (point-at-bol))
            (when (search-forward file (point-at-eol) t 1)
              (cond ((looking-at ":\\([0-9]+\\)")
                     (setq line
                           (string-to-number (buffer-substring-no-properties
                                              (match-beginning 1)
                                              (match-end 1)))))
                    ((looking-at "(")
                     ;; Typescript compiler output file(line, col)
                     (let ((raw (buffer-substring-no-properties
                                 (point)
                                 (forward-list 1))))
                       (setq raw (substring raw 1 (1- (length raw))))
                       (setq raw (split-string raw ","))
                       (setq col (string-to-number (nth 1 raw)))
                       (setq line (string-to-number (nth 0 raw))))))))
          (let* ((buf (get-file-buffer file))
                 (win (and buf (get-buffer-window buf 'visible))))
            (if win
                (select-window win)
              (find-file-other-window file)))
          (when line
            (goto-line line)
            (recenter))
          (when col
            (move-to-column col)))))))

(defun my/remove-non-ascii-chars ()
  (interactive)
  (query-replace-regexp "[^[:ascii:]]" ""))

(defun my/open-latest-downloaded-file ()
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
(defun my/copy-from-starting-col-till-eol (start end &optional evenly-sized-strings)
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

(defun my/unsaved-changes ()
  (interactive)
  (diff-buffer-with-file (current-buffer)))

(defun my/find-file-as-sudo ()
  (interactive)
  (when-let ((file-name (buffer-file-name)))
    (find-alternate-file (concat "/sudo::" file-name))))

(defun my/occur ()
  (interactive)
  (call-interactively
   (if (eq major-mode 'org-mode)
       'org-sparse-tree
     'occur)))

(defun my/pp-json (start end)
  (interactive "r")
  (let ((py (or (executable-find "python3")
                (error "unable to find python interpreter"))))
    (unless (use-region-p)
      (setq start (point-min) end (point-max)))
    (shell-command-on-region start
                             end
                             (format "%s -mjson.tool --sort-keys" py)
                             (current-buffer)
                             'replace)))


(defun my/toggle-camel-case-and-underscore ()
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

(defun my/find-file-in-other-window (&optional arg)
  (interactive "P")
  (cond ((not arg)
         (split-window-right)
         (windmove-right))
        (t
         (split-window-below)
         (windmove-down)))
  (my/open-project-file-or-find-files))

(defun my/frame-transparency (arg)
  (interactive "p")
  (set-frame-parameter nil 'alpha (list arg arg)))

(defun my/copy-full-path ()
  "Copies the buffer name to the kill ring.
If the current buffer isn't associated with a file and the major
mode is either Shell or Magit, then the current directory is
copied."
  (interactive)
  (let ((name (or (buffer-file-name)
                  (if (or (memq major-mode '(shell-mode dired-mode))
                          (string-prefix-p "Magit" mode-name))
                      default-directory
                    nil))))
    (if (not name)
        (message "Nothing to copy")
      (setq name (expand-file-name name))
      (kill-new name)
      (message "Copied `%s'" name))))

(defun my/git-grep-from-root ()
  (interactive)
  (consult-git-grep (my/git-repo-root)))

(defun my/git-insert-current-branch ()
  (interactive)
  (let ((current-branch (magit-get-current-branch)))
    (when current-branch
      (insert current-branch " "))))

(defvar-local my/google-search-term-prefix nil)
(put 'my/google-search-term-prefix 'safe-local-variable #'always)

(defvar-local my/google-search-term-suffix nil)
(put 'my/google-search-term-suffix 'safe-local-variable #'always)

(defun my/google-search (&optional incognito)
  "Google the currently selected region or the previous word.
Shows the term before doing so."
  (interactive "P")
  (let* ((selection (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning)
                                                        (region-end))
                      ""))
         (prompt (format "Google%s search: " (if incognito " incognito" "")))
         (initial-input (format "%s %s %s"
                                (or my/google-search-term-prefix "")
                                selection
                                (or my/google-search-term-suffix "")))
         (term (s-trim (read-string prompt (s-trim initial-input))))
         (url (format "https://www.google.com/search?q=%s"
                      (url-encode-url term)))
         (chrome-args (append '("open" "-na" "Google Chrome" "--args")
                              (if incognito '("--incognito") '())
                              (list url))))
    (apply #'start-process
           "Google search"
           nil
           chrome-args)))

(defun my/toggle-auto-hscroll-mode ()
  "This comes into play when toggle-truncate-lines is enabled.
Enabled means that the line gets truncated as opposed to
showing everything in the current window.
When enabled, if we scroll to the end of partial line,
how does scrolling affect the window:
 - current-line: only horizontally scroll the current line
 - nil:          don't scroll the window at all
 - t:            horizontally scroll the whole window"
  (interactive)
  (setq auto-hscroll-mode
        (cond ((eq auto-hscroll-mode t) 'current-line)
              ((eq auto-hscroll-mode 'current-line) nil)
              (t t)))
  (message "auto-hscroll-mode is set to %s" auto-hscroll-mode))

(defun my/jump-to-matching-char ()
  (interactive)
  (let ((openings (rx (or "[" "(" "{" "'" "\"")))
        (closings (rx (or "]" ")" "}" "'" "\""))))
    (cond ((looking-at openings)
           (forward-sexp))
          ((save-excursion
             (forward-char -1)
             (looking-at closings))
           (backward-sexp))
          ((or (re-search-forward openings nil t)
               (re-search-forward closings nil t))
           (forward-char -1)))))


(defun my/git-repo-root ()
  (let ((cmd "git rev-parse --show-toplevel 2> /dev/null"))
    (s-trim (shell-command-to-string cmd))))

(defun my/select-line ()
  "Select the current line when region is active.
See my-region-bindings-mode.el on how this is activated."
  (interactive)
  (when (let ((use-empty-active-region t))
          (use-region-p))
    (goto-char (point-at-bol))
    (exchange-point-and-mark)
    (goto-char (point-at-eol))))

(defun my/select-word ()
  (interactive)
  (when (let ((use-empty-active-region t))
          (use-region-p))
    (ignore-errors (backward-sexp))
    (exchange-point-and-mark)
    (forward-sexp)))

(defvar my/find-matching-indentation-level-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [down] 'my/find-next-matching-indentation-level)
    (define-key map [up] 'my/find-prev-matching-indentation-level)
    map))

(defun my/find-matching-indentation-level ()
  (interactive)
  (message "Hit [up] or [down] to move to previous or next indentation level")
  (set-transient-map my/find-matching-indentation-level-keymap t))

(defun my/find-next-matching-indentation-level (&optional backward)
  (interactive)
  (let ((done nil)
        (col (current-column))
        (fn (if backward 'previous-logical-line 'next-logical-line)))
    (while (not done)
      (funcall fn 1)
      (move-to-column col)
      (when (and (= (current-column) col)
                 (not (looking-at (rx (or space ?\t ?\n)))))
        (setq done t)))))

(defun my/find-prev-matching-indentation-level ()
  (interactive)
  (my/find-next-matching-indentation-level t))

(defun my/new-buffer-from-clip (&optional arg)
  (interactive "P")
  (my/new-buffer arg t))

(defun my/new-buffer (&optional arg paste-from-kill-ring)
  (interactive "P")
  (let* ((name (if arg (read-string "Buffer name: ") "*scratch*"))
         (buf (generate-new-buffer name)))
    (switch-to-buffer buf)
    (my/set-major-mode name)
    (when paste-from-kill-ring
      (save-excursion (yank))
      (message "Pasted from kill-ring"))))


(defun my/uptime ()
  (interactive)
  (let ((system-uptime (s-trim (shell-command-to-string "uptime")))
        (emacs-uptime (emacs-uptime)))
    (message "System: %s\nEmacs:  %s" system-uptime emacs-uptime)))

;; https://stackoverflow.com/questions/6172054/how-can-i-random-sort-lines-in-a-buffer
(defun my/shuffle-lines (beg end)
  "Shuffle lines in region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (= (random 2) 0)))))))

(defun my/reset-text-size ()
  (interactive)
  (text-scale-adjust 0))


(defun my/explore-new-packages ()
  "Extract packages using use-package in current buffer and
formats them into a list of clickable links."
  (interactive)
  (let ((new-packages '())
        (original (current-buffer))
        (new (get-buffer-create (generate-new-buffer-name "*explore new packages*"))))
    (goto-char (point-min))
    (while (re-search-forward "^(use-package " nil t)
      (push (thing-at-point 'symbol) new-packages))
    (with-current-buffer new
      (insert "* new packages\n")
      (dolist (pkg (nreverse new-packages))
        (insert (format "** [[https://www.google.com/search?q=emacs+%s][%s]]\n" pkg pkg)))
      (goto-char (point-min))
      (org-mode))
    (delete-other-windows)
    (goto-char (point-min))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer new)))
