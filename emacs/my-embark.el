(require 'ace-window)

;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(defun my/embark-ace-action-find-file ()
  "Open file and position it using ace-window."
  (interactive)
  (let ((aw-dispatch-always t))
    (when-let* ((selected-win (aw-select nil)))
      ;; If selected-win is nil, probably a dispatch action (via key '?')
      ;; occurred and point will be in the selected window already.
      (aw-switch-to-window selected-win))
    (call-interactively 'find-file)))

(define-key embark-file-map (kbd "o") 'my/embark-ace-action-find-file)
