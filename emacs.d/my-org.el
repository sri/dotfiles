(require 'org)

(set-register ?t '(file . "~/Dropbox/Notes/todo.org"))

(setq org-agenda-files '("~/Dropbox/Notes"))

(add-hook 'org-mode-hook
          (lambda ()
            ;; 3rd arg T says to modify the buffer-local hook
            (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
            (setq cursor-type 'bar)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   ;; (awk . t)
   ;; (calc . t)
   ;; (clojure . t)
   (emacs-lisp . t)
   ;; (gnuplot . t)
   ;; (haskell . t)
   ;; (ocaml . t)
   ;; (org . t)
   (python . t)
   (ruby . t)
   ;; (sh . t)
   ;; (sql . t)
   ;; (sqlite . t)
   ))

(setq org-hide-leading-stars t)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k nil)
(setq org-return-follows-link t)
(setq org-use-speed-commands t)
(setq org-hide-leading-stars nil)
(setq org-fontify-done-headline t)
(setq org-closed-keep-when-no-todo t)
(setq org-log-done 'time)
(setq org-completion-use-ido t)

(my-override-keys "C-j" 'other-window '(org-mode))
(my-override-keys "C-," 'beginning-of-buffer '(org-mode))
(my-override-keys "<S-return>" 'my-dired '(org-mode))

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
