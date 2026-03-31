;; -*- lexical-binding: t; -*-
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
          (let* ((new-name (f-swap-ext buffer-file-name selected-ext))
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
      (save-excursion (yank))
      (save-buffer)
      (message "Pasted from kill-ring"))))

(defun my/scratch-new-temp (&optional paste-from-kill-ring)
  (interactive)
  (switch-to-buffer (generate-new-buffer "*my-temp*"))
  (when paste-from-kill-ring
    (save-excursion (yank))
    (message "Pasted from kill-ring")))

(defun my/scratch-new-temp-from-clipboard ()
  (interactive)
  (my/scratch-new-temp t))

(defun my/scratch-new-from-clipboard ()
  "Create a new persistent scratch buffer with initial contents
yanked from the kill-ring."
  (interactive)
  (my/scratch-new t))

(defvar my/scratch-browse-previous-window-configuration nil)
(defvar my/scratch-browse-preview-window nil)
(defvar my/scratch-browse-buffer-name "*scratch files*")
(defvar-local my/scratch-browse-sort-kind 'created)

(defvar my/scratch-browse-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "q") #'my/scratch-browse-quit)
    (define-key map (kbd "n") #'my/scratch-browse-next)
    (define-key map (kbd "p") #'my/scratch-browse-previous)
    (define-key map (kbd "N") #'my/scratch-browse-next)
    (define-key map (kbd "P") #'my/scratch-browse-previous)
    (define-key map (kbd "s") #'my/scratch-browse-toggle-sort)
    (define-key map (kbd "RET") #'my/scratch-browse-edit-file)
    map))

(define-derived-mode my/scratch-browse-mode special-mode "Scratch-Browse"
  "Major mode for browsing persistent scratch files."
  (setq-local truncate-lines t)
  (setq-local default-directory my/scratch-directory)
  (setq-local my/scratch-browse-sort-kind 'created))

(defun my/scratch-browse--file-time (file sort-kind)
  (let* ((attrs (file-attributes file))
         (modified (file-attribute-modification-time attrs))
         (created (and (fboundp 'file-attribute-creation-time)
                       (ignore-errors
                         (file-attribute-creation-time attrs)))))
    (pcase sort-kind
      ('modified modified)
      (_ (or created modified)))))

(defun my/scratch-browse--sorted-files (&optional sort-kind)
  (let* ((kind (or sort-kind my/scratch-browse-sort-kind 'created))
         (files (directory-files-recursively my/scratch-directory ".*" nil))
         (files-with-times (mapcar (lambda (file)
                                     (cons file (my/scratch-browse--file-time file kind)))
                                   files)))
    (mapcar #'car
            (sort files-with-times
                  (lambda (a b)
                    (time-less-p (cdr b) (cdr a)))))))

(defun my/scratch-browse--relative-time (time)
  (let* ((seconds (max 0 (floor (float-time (time-subtract (current-time) time)))))
         (minutes (max 1 (floor (/ seconds 60.0))))
         (hours (max 1 (floor (/ seconds 3600.0))))
         (days (max 1 (floor (/ seconds 86400.0))))
         (weeks (max 1 (floor (/ days 7.0))))
         (months (max 1 (floor (/ days 30.0))))
         (years (max 1 (floor (/ days 365.0)))))
    (cond
     ((< minutes 60) (format "%d %s ago" minutes (if (= minutes 1) "min" "mins")))
     ((< hours 24) (format "%d %s ago" hours (if (= hours 1) "hour" "hours")))
     ((< days 7) (format "%d %s ago" days (if (= days 1) "day" "days")))
     ((< days 30) (format "%d %s ago" weeks (if (= weeks 1) "week" "weeks")))
     ((< days 365) (format "%d %s ago" months (if (= months 1) "month" "months")))
     (t (format "%d %s ago" years (if (= years 1) "year" "years"))))))

(defun my/scratch-browse--file-at-point ()
  (get-text-property (point) 'my/scratch-file))

(defun my/scratch-browse--render (&optional keep-file)
  (let* ((inhibit-read-only t)
         (files (my/scratch-browse--sorted-files my/scratch-browse-sort-kind))
         (entries (mapcar (lambda (file)
                            (let* ((time (my/scratch-browse--file-time file my/scratch-browse-sort-kind))
                                   (relative-time (my/scratch-browse--relative-time time)))
                              (list :file file
                                    :path (file-relative-name file my/scratch-directory)
                                    :relative-time relative-time)))
                          files))
         (path-width (if entries
                         (apply #'max (mapcar (lambda (it) (length (plist-get it :path))) entries))
                       0))
         (relative-width (if entries
                             (apply #'max (mapcar (lambda (it) (length (plist-get it :relative-time))) entries))
                           0))
         (line-format (format "%%-%ds - %%-%ds" path-width relative-width)))
    (erase-buffer)
    (dolist (entry entries)
      (let ((line-start (point))
            (file (plist-get entry :file)))
        (insert (format line-format
                        (plist-get entry :path)
                        (plist-get entry :relative-time))
                "\n")
        (add-text-properties line-start
                             (1- (point))
                             `(my/scratch-file ,file))))
    (goto-char (point-min))
    (when keep-file
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (if (equal (my/scratch-browse--file-at-point) keep-file)
              (setq found t)
            (forward-line 1)))
        (unless found
          (goto-char (point-min)))))
    files))

(defun my/scratch-browse-view-file (file &optional select-window)
  (when file
    (let ((origin-window (selected-window)))
      (unless (window-live-p my/scratch-browse-preview-window)
        (setq my/scratch-browse-preview-window (split-window origin-window nil 'below)))
      (set-window-buffer my/scratch-browse-preview-window (find-file-noselect file))
      (when select-window
        (select-window my/scratch-browse-preview-window)))))

(defun my/scratch-browse--preview-at-point ()
  (if-let ((file (my/scratch-browse--file-at-point)))
      (my/scratch-browse-view-file file)
    (message "No file on this line")))

(defun my/scratch-browse-next ()
  (interactive)
  (forward-line 1)
  (when (eobp)
    (forward-line -1))
  (my/scratch-browse--preview-at-point))

(defun my/scratch-browse-previous ()
  (interactive)
  (forward-line -1)
  (my/scratch-browse--preview-at-point))

(defun my/scratch-browse-toggle-sort ()
  (interactive)
  (setq-local my/scratch-browse-sort-kind
              (if (eq my/scratch-browse-sort-kind 'modified)
                  'created
                'modified))
  (let ((current-file (my/scratch-browse--file-at-point)))
    (my/scratch-browse--render current-file)
    (my/scratch-browse--preview-at-point))
  (message "%s"
           (if (eq my/scratch-browse-sort-kind 'modified)
               "Sorted by last modified time (newest first)"
             "Sorted by last created time (newest first)")))

(defun my/scratch-browse-edit-file ()
  (interactive)
  (if-let ((file (my/scratch-browse--file-at-point)))
      (my/scratch-browse-view-file file t)
    (message "No file on this line")))

(defun my/scratch-browse-quit ()
  (interactive)
  (when my/scratch-browse-previous-window-configuration
    (set-window-configuration my/scratch-browse-previous-window-configuration)
    (setq my/scratch-browse-previous-window-configuration nil)
    (setq my/scratch-browse-preview-window nil)))

(defun my/scratch-browse ()
  (interactive)
  (f-mkdir-full-path my/scratch-directory)
  (let* ((files-buffer (get-buffer-create my/scratch-browse-buffer-name))
         files)
    (setq my/scratch-browse-previous-window-configuration
          (current-window-configuration))
    (delete-other-windows)
    (setq my/scratch-browse-preview-window (split-window-below))
    (with-current-buffer files-buffer
      (my/scratch-browse-mode)
      (setq files (my/scratch-browse--render)))
    (switch-to-buffer files-buffer)
    (if files
        (my/scratch-browse--preview-at-point)
      (set-window-buffer my/scratch-browse-preview-window
                         (get-buffer-create "*scratch preview*"))
      (message "No scratch files found in %s" my/scratch-directory))))
