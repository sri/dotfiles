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
