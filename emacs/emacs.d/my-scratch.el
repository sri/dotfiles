(defvar my-scratch-filename
  (expand-file-name "~/.emacs.d/scratch"))

(make-variable-buffer-local 'auto-save-visited-file-name)

(define-derived-mode my-scratch-mode text-mode "Scratch"
  "My scratch pad."
  (setq auto-save-visited-file-name t)
  (setq buffer-save-without-query t)
  (setq buffer-file-name my-scratch-filename)
  (auto-save-mode 1))

(defun my-scratch-setup ()
  (when (file-exists-p my-scratch-filename)
    (insert-file-contents my-scratch-filename)
    (set-buffer-modified-p nil))
  ;; After Emacs has started up, if *scratch* is still in
  ;; fundamental-mode, then initial-major-mode is funcall'ed which
  ;; will kill all local variables.  So any variables we set below
  ;; won't have any effect.  So change the mode here. See startup.el.
  (my-scratch-mode))

(with-current-buffer (get-buffer-create "*scratch*")
  (my-scratch-setup))
