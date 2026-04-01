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

;; Helpers for Embark multi-target actions
(defun my/embark--as-list (items)
  (if (listp items) items (list items)))

(defun my/embark--open-in-new-tabs (items open-fn empty-msg)
  (unless items (user-error empty-msg))
  (let ((orig-tab (1+ (tab-bar--current-tab-index (funcall tab-bar-tabs-function)))))
    (dolist (item items)
      (tab-bar-new-tab)
      (funcall open-fn item))
    (tab-bar-select-tab (1+ orig-tab))))

(defun my/embark--open-in-side-by-side-windows (items open-fn empty-msg)
  (unless items (user-error empty-msg))
  (delete-other-windows)
  (funcall open-fn (car items))
  (dolist (item (cdr items))
    (split-window-below)
    (other-window 1)
    (funcall open-fn item))
  (balance-windows))

(defun my/embark--unique-files (files)
  (let* ((files (my/embark--as-list files))
         (files (mapcar (lambda (f) (expand-file-name f default-directory)) files)))
    (delete-dups files)))

(defun my/embark--live-unique-buffers (buffers)
  (let* ((buffers (my/embark--as-list buffers))
         (buffers (mapcar (lambda (b) (if (bufferp b) b (get-buffer b))) buffers))
         (buffers (delq nil buffers)))
    (delete-dups buffers)))

(defun my/embark--existing-bookmarks (bookmarks)
  (let (result)
    (dolist (bookmark (my/embark--as-list bookmarks))
      (let ((name (if (stringp bookmark) bookmark (format "%s" bookmark))))
        (when (bookmark-get-bookmark name 'noerror)
          (push name result))))
    (delete-dups (nreverse result))))

(defun my/embark-open-files-in-new-tabs (files)
  "Open FILES, one per new tab."
  (interactive)
  (my/embark--open-in-new-tabs
   (my/embark--unique-files files)
   #'find-file
   "No files selected"))

(defun my/embark-open-files-in-side-by-side-windows (files)
  "Open FILES in current frame, one file per side-by-side window."
  (interactive)
  (my/embark--open-in-side-by-side-windows
   (my/embark--unique-files files)
   #'find-file
   "No files selected"))

(defun my/embark-open-buffers-in-new-tabs (buffers)
  "Open BUFFERS, one per new tab."
  (interactive)
  (my/embark--open-in-new-tabs
   (my/embark--live-unique-buffers buffers)
   #'switch-to-buffer
   "No buffers selected"))

(defun my/embark-open-buffers-in-side-by-side-windows (buffers)
  "Open BUFFERS in current frame, one buffer per side-by-side window."
  (interactive)
  (my/embark--open-in-side-by-side-windows
   (my/embark--live-unique-buffers buffers)
   #'switch-to-buffer
   "No buffers selected"))

(defun my/embark-open-bookmarks-in-new-tabs (bookmarks)
  "Open BOOKMARKS, one per new tab."
  (interactive)
  (my/embark--open-in-new-tabs
   (my/embark--existing-bookmarks bookmarks)
   #'bookmark-jump
   "No bookmarks selected"))

(defun my/embark-open-bookmarks-in-side-by-side-windows (bookmarks)
  "Open BOOKMARKS in current frame, one bookmark per side-by-side window."
  (interactive)
  (my/embark--open-in-side-by-side-windows
   (my/embark--existing-bookmarks bookmarks)
   #'bookmark-jump
   "No bookmarks selected"))

(defun my/embark--bind-open-actions (keymap open-tabs-fn open-windows-fn)
  (define-key keymap (kbd "T") open-tabs-fn)
  (define-key keymap (kbd "W") open-windows-fn))

(with-eval-after-load 'embark
  ;; No "Run ... on N files?" prompt
  (setq embark-confirm-act-all nil)

  ;; Make RET default to find-file for file candidates
  (add-to-list 'embark-default-action-overrides '(file . find-file))

  ;; Tell embark-act-all to pass the whole selected list to these commands
  (dolist (fn '(my/embark-open-files-in-new-tabs
                my/embark-open-files-in-side-by-side-windows
                my/embark-open-buffers-in-new-tabs
                my/embark-open-buffers-in-side-by-side-windows
                my/embark-open-bookmarks-in-new-tabs
                my/embark-open-bookmarks-in-side-by-side-windows))
    (add-to-list 'embark-multitarget-actions fn))

  (bind-keys :map embark-variable-map
             ("k" . describe-keymap))

  ;; Actions in file, buffer and bookmark action maps
  (my/embark--bind-open-actions embark-file-map
                                #'my/embark-open-files-in-new-tabs
                                #'my/embark-open-files-in-side-by-side-windows)
  (my/embark--bind-open-actions embark-buffer-map
                                #'my/embark-open-buffers-in-new-tabs
                                #'my/embark-open-buffers-in-side-by-side-windows)
  (my/embark--bind-open-actions embark-bookmark-map
                                #'my/embark-open-bookmarks-in-new-tabs
                                #'my/embark-open-bookmarks-in-side-by-side-windows))

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
