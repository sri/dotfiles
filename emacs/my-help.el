(require 'help-mode)

(defun my/help-push-next-button ()
  (interactive)
  (forward-button 1 t)
  (push-button))

(defun my/eval-expression ()
  (interactive)
  (goto-char (point-min))
  (let* ((sym (buffer-substring-no-properties
               (point)
               (save-excursion
                 (forward-sexp)
                 (point))))
         (default-input (format "(setq %s )" sym)))
    (eval-expr (eval-expr-read-lisp-object-minibuffer "Eval: " default-input))))


(bind-keys :map help-mode-map
           (":" . my/eval-expression)
           ("b" . help-go-back)
           ("f" . help-go-forward)
           ("n" . forward-button)
           ("p" . backward-button)
           ("x" . delete-window)
           ("g" . my/help-push-next-button))
