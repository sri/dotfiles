(require 'org)
(require 'org-bullets)

(setq org-bullets-bullet-list '("○"))

;; Fix inserting a new plain list item:
;; don't insert a newline before the new plain list item. This only
;; occurs when I'm on the last plain list item and hit
;; Alt-Shift-Enter.
(setq org-blank-before-new-entry
      (assq-delete-all 'plain-list-item org-blank-before-new-entry))

(setq org-agenda-files '("~/Dropbox/Notes"))

(add-hook 'org-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (font-lock-mode 1)
            (org-bullets-mode 1)
            (diminish 'org-indent-mode)
            (setq cursor-type 'bar)))

(add-to-list 'org-modules 'habits)

(setq org-babel-python-command "python3")

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   ;; (awk . t)
   (calc . t)
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

;; Show lists as collapsed
(setq org-cycle-include-plain-lists 'integrate)

;; M-RET while in the middle of a header will now create a new header
;; (at the same level) below the current one. Old behavior: split the
;; line and start the next header with the fragment after point of
;; this line.
(push '(headline) org-M-RET-may-split-line)

(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k nil)
(setq org-return-follows-link t)
(setq org-use-speed-commands t)
(setq org-fontify-done-headline t)
(setq org-closed-keep-when-no-todo t)
(setq org-log-done nil)
(setq org-confirm-babel-evaluate nil)

(setq org-todo-keyword-faces
      '(("TODO" :foreground "#4b4f89")
        ("INPROGRESS" :foreground "#4b4f89" :box t)
        ("APPT" :foreground "medium blue" :weight bold)
        ("NOTE" :foreground "brown" :weight bold)
        ("STARTED" :foreground "dark orange" :weight bold)
        ("WAITING" :foreground "red" :weight bold)
        ("DELEGATED" :foreground "dark violet" :weight bold)
        ("DEFERRED" :foreground "dark blue" :weight bold)
        ("SOMEDAY" :foreground "#088e8e" :weight bold)
        ("PROJECT" :foreground "#088e8e" :weight bold)))

(setq org-todo-keywords
      '((sequence "TODO" "APPT" "NOTE" "IN-PROGRESS" "WAITING"
                  "DELEGATED" "DEFERRED"
                  "SOMEDAY" "PROJECT"
                  "|"
                  "DONE" "CANCELED")))

(defun my/org-insert-chrome-link ()
  (interactive)
  (let ((subject (do-applescript "tell application \"Google Chrome\"
                                  title of active tab of front window
                                  end tell"))
        (url (do-applescript "tell application \"Google Chrome\"
                              URL of active tab of front window
                              end tell")))
    (insert (org-make-link-string url subject))))
