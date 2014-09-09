;; -*- lexical-binding: t -*-

;; Maps mode -> alist of (key . new-fn)
(defvar my-override-keys-hash
  (make-hash-table :test 'equal))

;; Runs in the context of a hook.
(defun my-override-keys-hook ()
  (let ((new-bindings
         (gethash major-mode my-override-keys-hash)))
    (dolist (new new-bindings)
      ;; Bind key to the new fn.
      ;; Print out a message the first time the
      ;; key is invoked.
      ;; WARNING: these let* vars are lexically bound.
      (let* ((fn (cdr new))
             (key-raw (car new))
             (key (if (stringp (car new))
                      (kbd (car new))
                    (car new)))
             (current-binding (key-binding key t)))
        (unless (eq fn current-binding)
          (define-key (current-local-map) key
            ;; TODO: it would be nice if this lambda
            ;; had the same docstring as fn.
            (lambda ()
              (interactive)
              (when (symbolp current-binding)
                (message "%s previously: `%s'; now: `%s'"
                         key-raw current-binding fn))
              (call-interactively fn)
              (define-key (current-local-map) key fn))))))))

(defun my-override-keys (key new-fn modes)
  (dolist (mode modes)
    (let ((hook (intern (format "%s-hook" mode))))
      (add-hook hook 'my-override-keys-hook))
    (puthash mode
             (cons (cons key new-fn) (gethash mode my-override-keys-hash))
             my-override-keys-hash)))
