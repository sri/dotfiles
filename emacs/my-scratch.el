;; My persistent scratch mode.
;; Includes a quick mode switcher (also switches the file's extension).

(require 'f)
(require 'dash)

(defvar my/scratch-directory "~/.emacs-scratch")

(defvar my/scratch-switch-modes
  '((python . "py")
    (emacs-lisp . "el")
    (ruby . "rb")
    (text . "txt")
    (org . "org")
    (shell . "sh")
    (java . "java")
    (javascript . "js")
    (json . "json")
    (sql . "sql")
    (yaml . "yaml")))

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
          (let ((new-name (f-swap-ext buffer-file-name selected-ext)))
            (when (not (string= (f-ext buffer-file-name) selected-ext))
              (save-buffer)
              (rename-file buffer-file-name new-name)
              (set-visited-file-name new-name t t)
              (with-current-buffer (dired-noselect (f-dirname new-name))
                (revert-buffer))
              (message ""))))))))

(defun my/scratch-new (&optional paste-from-kill-ring)
  (interactive)
  (let* ((now (format-time-string "%Y/%m/%d/%Y-%d-%m-%H%M%S"))
         (path (f-expand (concat now ".org") my/scratch-directory))
         (dir (f-dirname path)))
    (f-mkdir-full-path dir)
    (write-region "" nil path nil nil nil t) ; The `t' forces file to be new.
    (find-file path)
    (when paste-from-kill-ring
      (save-excursion (yank))
      (save-buffer)
      (message "Pasted from kill-ring"))))

(defun my/scratch-new-from-clipboard ()
  (interactive)
  (my/scratch-new t))

(defun my/scratch-browse ()
  ;; TODO
  (dired my/scratch-directory))

;; Ideas:
;;  Search - consult-ripgrep - how to C-u 7 my/scratch-search -> within last 7 days
