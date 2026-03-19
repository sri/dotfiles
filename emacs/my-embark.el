;; -*- lexical-binding: t; -*-
(require 'ace-window)

(defun my/ace-window-switch-to-selected-window ()
  (let ((aw-dispatch-always t))
    (when-let* ((selected-win (aw-select nil)))
      ;; If selected-win is nil, probably a dispatch action (via key '?')
      ;; occurred and point will be in the selected window already.
      (aw-switch-to-window selected-win))))

;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(defun my/embark-ace-action-find-file ()
  "Open file and position it using ace-window."
  (interactive)
  (my/ace-window-switch-to-selected-window)
  (call-interactively 'find-file))

(define-key embark-file-map (kbd "o") 'my/embark-ace-action-find-file)


(defun my/embark-ace-action-switch-to-buffer ()
  "View buffer and position it using ace-window."
  (interactive)
  (my/ace-window-switch-to-selected-window)
  (call-interactively 'switch-to-buffer))

(define-key embark-buffer-map (kbd "o") 'my/embark-ace-action-switch-to-buffer)

;; Helpers for Embark multi-file actions
(defun my/embark--unique-files (files)
  (let* ((files (if (listp files) files (list files)))
         (files (mapcar (lambda (f) (expand-file-name f default-directory)) files)))
    (delete-dups files)))

(defun my/embark-open-files-in-new-tabs (files)
  "Open FILES, one per new tab."
  (interactive)
  (let ((files (my/embark--unique-files files))
        (orig-tab (1+ (tab-bar--current-tab-index (funcall tab-bar-tabs-function)))))
    (unless files (user-error "No files selected"))
    (dolist (file files)
      (tab-bar-new-tab)
      (find-file file))
    (tab-bar-select-tab (1+ orig-tab))))

(defun my/embark-open-files-in-side-by-side-windows (files)
  "Open FILES in current frame, one file per side-by-side window."
  (interactive)
  (let ((files (my/embark--unique-files files)))
    (unless files (user-error "No files selected"))
    (delete-other-windows)
    (find-file (car files))
    (dolist (file (cdr files))
      (split-window-below)
      (other-window 1)
      (find-file file))
    (balance-windows)))

(with-eval-after-load 'embark
  ;; No "Run ... on N files?" prompt
  (setq embark-confirm-act-all nil)

  ;; Make RET default to find-file for file candidates
  (add-to-list 'embark-default-action-overrides '(file . find-file))

  ;; Tell embark-act-all to pass the whole selected list to these commands
  (dolist (fn '(my/embark-open-files-in-new-tabs
                my/embark-open-files-in-side-by-side-windows))
    (add-to-list 'embark-multitarget-actions fn))

  ;; Actions in file action map
  (bind-keys :map embark-file-map
             ("T" . my/embark-open-files-in-new-tabs)
             ("W" . my/embark-open-files-in-side-by-side-windows)))

(defun my/embark-select-and-next ()
  "Select current Vertico candidate, then move down one (no wrap)."
  (interactive)
  (embark-select)
  (when (and (boundp 'vertico--total)
             (> vertico--total 0)
             (< vertico--index (1- vertico--total)))
    (vertico-next 1)))

(bind-keys :map vertico-map
           ("C-;" . embark-act-all)
           ("C-SPC" . my/embark-select-and-next))
