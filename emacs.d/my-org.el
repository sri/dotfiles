(require 'org)

(set-register ?t '(file . "~/Dropbox/Notes/todo.org"))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((awk . t)
;;    (calc . t)
;;    (clojure . t)
;;    (emacs-lisp . t)
;;    (gnuplot . t)
;;    (haskell . t)
;;    (ocaml . t)
;;    (org . t)
;;    (python . t)
;;    (ruby . t)
;;    (sh . t)
;;    (sql . t)
;;    (sqlite . t)))

(setq org-special-ctrl-a/e t)
(setq org-return-follows-link t)
(setq org-use-speed-commands t)
(setq org-hide-leading-stars nil)
(setq org-fontify-done-headline t)
(setq org-closed-keep-when-no-todo t)
(setq org-log-done 'time)
(setq org-completion-use-ido t)

(my-overwrite-key-bindings-in-mode "C-j" 'other-window '(org-mode))
(my-overwrite-key-bindings-in-mode "C-," 'beginning-of-buffer '(org-mode))

(custom-set-faces
  '(org-done ((t (:strike-through t))))
  '(org-headline-done ((t (:strike-through t)))))

(defun my-org-insert-chrome-link ()
  (interactive)
  (let ((subject (do-applescript "tell application \"Google Chrome\"
                                  title of active tab of front window
                                  end tell"))
        (url (do-applescript "tell application \"Google Chrome\"
                              URL of active tab of front window
                              end tell")))
    (insert (org-make-link-string url subject))))
