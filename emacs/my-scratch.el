;; My persistent scratch mode.
;; Includes a quick mode switcher (also switches the file's extension).
;; Autosave
;; Browsing files:
;;  Search - consult-ripgrep - how to C-u 7 my/scratch-search -> within last 7 days

(require 'f)
(require 'dash)

(defvar my/scratch-directory "~/.emacs-scratch")

(defvar my/scratch-switch-modes
  ;; Align-regexp's call interface is a little wonky; had to output how the
  ;; args were constructed to get the below.
  ;; (align-regexp (progn (forward-line 1) (point)) (progn (forward-sexp) (forward-line 1) (point)) "\\(\\s-*\\)[.]" 1 1 nil)
  '((python     . "py")
    (emacs-lisp . "el")
    (ruby       . "rb")
    (text       . "txt")
    (org        . "org")
    (shell      . "sh")
    (java       . "java")
    (javascript . "js")
    (json       . "json")
    (sql        . "sql")
    (yaml       . "yaml")))

(defun my/scratch-switch-modes ()
  "Switch current buffer to one of the modes defined by `my/scratch-switch-modes'.
If the current buffer is backed by a file, change the file extension to match the mode.
Finally, the dired buffer of the file is updated."
  (interactive)
  (when-let* ((selected (consult--read (--map (symbol-name (car it)) my/scratch-switch-modes))))
    (let ((selected-ext (alist-get (intern selected) my/scratch-switch-modes))
          (selected-mode (intern (format "%s-mode" selected))))
      (when (not (eq major-mode selected-mode))
        (call-interactively selected-mode)
        (when buffer-file-name
          (let ((new-name (f-swap-ext buffer-file-name selected-ext))
                (dirname (f-dirname new-name)))
            (when (not (string= (f-ext buffer-file-name) selected-ext))
              (save-buffer)
              (rename-file buffer-file-name new-name)
              (set-visited-file-name new-name t t)
              (when (dired-find-buffer-nocreate dirname)
                ;; Only when there is an existing dired buffer for
                ;; the parent directory, refresh it.
                (with-current-buffer (dired-noselect (f-dirname new-name))
                  (revert-buffer)))
              (message ""))))))))

(defun my/scratch-new (&optional paste-from-kill-ring)
  "Create a new persistent scratch buffer."
  (interactive)
  (let* ((now (format-time-string "%Y/%m/%d/%Y-%d-%m-%H%M%S"))
         (path (f-expand (concat now ".org") my/scratch-directory))
         (dir (f-dirname path)))
    (f-mkdir-full-path dir)
    (write-region "" nil path nil nil nil t) ; The `t' forces file to be new.
    (find-file path)
    ;; TODO: maybe save periodically?
    (setq buffer-save-without-query t)
    (when paste-from-kill-ring
      (save-excursion (yanked))
      (save-buffer)
      (message "Pasted from kill-ring"))))

(defun my/scratch-new-from-clipboard ()
  "Create a new persistent scratch buffer with initial contents
yanked from the kill-ring."
  (interactive)
  (my/scratch-new t))

(defvar my/scratch-browse-previous-window-configuration nil)

(defun my/scratch-browse-view-file (file)
  (other-window 1)
  (find-file (expand-file-name file my/scratch-directory))
  (other-window 1))

(defun my/scratch-browse-next ()
  (interactive)
  (forward-line 1)
  (my/scratch-browse-view-file
`   (buffer-substring-no-properties (point)
                                   (point-at-eol))))

(defun my/scratch-browse-quit ()
  (interactive)
  (when my/scratch-browse-previous-window-configuration
    (set-window-configuration my/scratch-browse-previous-window-configuration)
    (setq my/scratch-browse-previous-window-configuration nil)))

(defun my/scratch-browse ()
  (interactive)
  (let* ((default-directory my/scratch-directory)
         (du (->> "du -hs"
                  shell-command-to-string
                  split-string
                  car))
         (files (->> "find . -type f"
                     shell-command-to-string
                     split-string))
         (current-file (car files))
         (files-buffer (get-buffer-create "*scratch files*")))
    (setq my/scratch-browse-previous-window-configuration
          (current-window-configuration))
    (delete-other-windows)
    (split-window-vertically)
    (with-current-buffer files-buffer
      (setq default-directory my/scratch-directory)
      (erase-buffer)
      (insert "Size: " du
              "; total files: " (number-to-string (length files))
              "\n")
      (dolist (f files)
        (insert f "\n"))
      (goto-char (point-min))
      (forward-line 1)
      (local-set-key (kbd "q") 'my/scratch-browse-quit)
      (local-set-key (kbd "n") 'my/scratch-browse-next))
    (switch-to-buffer files-buffer)
    (my/scratch-browse-view-file current-file)))
