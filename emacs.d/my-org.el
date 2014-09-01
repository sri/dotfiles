(require 'org)

(set-register ?t '(file . "~/Dropbox/Notes/todo.org"))

(setq org-return-follows-link t)
(setq org-use-speed-commands t)
(setq org-hide-leading-stars nil)
(setq org-fontify-done-headline t)
(setq org-closed-keep-when-no-todo t)
(setq org-log-done 'time)

(my-overwrite-key-bindings-in-mode "C-j" 'other-window '(org-mode))

(custom-set-faces
  '(org-done ((t (:strike-through t))))
  '(org-headline-done ((t (:strike-through t)))))
