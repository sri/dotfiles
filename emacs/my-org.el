;;; -*- lexical-binding: t -*-
(require 'org)
(require 'org-mouse)

(require 'org-bullets)
(setq org-bullets-bullet-list '("○"))

(setq org-capture-templates
      '(
        ("t" "Inbox" entry (file "~/my/notes/inbox.org") "* TODO %?\n")
        ))

(setq org-cycle-include-plain-lists 'integrate)
(setq org-blank-before-new-entry nil)
(setq org-M-RET-may-split-line '((default . nil)))
(setq org-insert-heading-respect-content t)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k nil)
(setq org-return-follows-link nil)
(setq org-use-speed-commands t)
(setq org-fontify-done-headline t)
(setq org-closed-keep-when-no-todo t)

(setq org-log-done 'time)
;; This only works if org-todo-keywords are annotated with `!'
;; (for timestamp) and `@' (for note), like:
;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "WAIT(w!)" "|" "DONE(d!)" "CANCELED(c!)")))
(setq org-log-into-drawer t)
(setq org-tag-alist
      '(("ADMIN" . ?a)
        ("SOMEDAY" . ?s)
        ))

(setq org-confirm-babel-evaluate nil)
(setq org-agenda-files '("~/my/notes"))
(setq org-babel-python-command "python3")

(setq org-refile-targets
      '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
;;(setq org-refile-allow-creating-parent-nodes 'confirm)


;; TODO: do later
;; NEXT: do now
;; DONE: done
;; WAIT: waiting for some external change (event)
;; STOP: stopped waiting, decided not to work on it
;; CANCELED: discarded
'(setq org-todo-keyword-faces
    '(("NOTE" :foreground "#4b4f89")
      ("IDEA" :foreground "#4b4f89" :box t)
      ("TODO" :foreground "medium blue" :weight bold)
      ("SOON" :foreground "brown" :weight bold)
      ("WAIT" :foreground "dark orange" :weight bold)
      ("HOLD" :foreground "red" :weight bold)
      ("HACK" :foreground "dark violet" :weight bold)
      ("NEXT" :foreground "dark blue" :weight bold)
      ("DONE" :foreground "#088e8e" :weight bold)
      ("STOP" :foreground "#088e8e" :weight bold)))

'(setq org-todo-keyword-faces
      '(("TODO"       . (:foreground "tomato" :weight bold))
        ("IN-PROGRESS". (:foreground "gold" :weight bold))
        ("WAITING"    . (:foreground "orange" :weight bold))
        ("BLOCKED"    . (:foreground "red" :weight bold))
        ("REVIEW"     . (:foreground "deep sky blue" :weight bold))
        ("FOLLOWUP"   . (:foreground "cyan" :weight bold))
        ("DONE"       . (:foreground "spring green" :weight bold))
        ("CANCELLED"  . (:foreground "gray" :weight bold))
        ("DELEGATED"  . (:foreground "light salmon" :weight bold))
        ("DUPLICATE"  . (:foreground "gray70" :weight bold))))


(setq org-todo-keywords
      '((sequence "TODO(t)" "IDEA(i)" "NEXT(n)" "WAIT(w)"
                  "|"
                  "DONE(d)" "CANCELLED(c)")))

(setq org-src-preserve-indentation t)

(add-hook 'org-mode-hook
          (lambda ()
            ;; (turn-on-auto-fill)
            (org-bullets-mode 1)
            (setq cursor-type 'bar)))

(add-to-list 'org-modules 'habits)


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
   (shell . t)
   (sql . t)
   ;; (sqlite . t)
   ))

;; Show lists as collapsed




(defun my/org-insert-chrome-link ()
  (interactive)
  (let ((subject (do-applescript "tell application \"Google Chrome\"
                                  title of active tab of front window
                                  end tell"))
        (url (do-applescript "tell application \"Google Chrome\"
                              URL of active tab of front window
                              end tell")))
    (insert (org-make-link-string url subject))))


;; From https://list.orgmode.org/orgmode/CAOBv0PdCgqP1oZrhTmxyt1paKAotfH3LDPv5vYeXzekgZ1U0Ow@mail.gmail.com/
(require 'button-lock)
(require 'thingatpt)

(add-hook 'org-mode-hook (lambda () (button-lock-mode 1)))

(defface my/org-ticket-face
  '((t :foreground "#268bd2" :underline t :inherit unspecified))
  "Face for Org dynamic links.")

(defun my/org-create-dynamic-link (regex url-template)
  (let ((url-template url-template))
    (button-lock-set-button
     regex
     (lambda ()
       (interactive)
       (let* ((matched-string (thing-at-point 'symbol))
              (url (if (save-match-data (string-match "%s" url-template))
                       (format url-template matched-string)
                     url-template)))
         (browse-url url)))
     :face 'my/org-ticket-face)))

(defun my/org-mode-get-subtree-count (&optional debug)
  (interactive "P")
  (let ((sub-heading-level (1+ (org-current-level)))
        (count 0))
    (save-excursion
      (org-map-entries
       (lambda ()
         (when debug
           (message "%s: current=%d, sub-heading-level=%d"
                    (buffer-substring-no-properties (point-at-bol)
                                                    (point-at-eol))
                    (org-current-level)
                    sub-heading-level))
         (when (= (org-current-level) sub-heading-level)
           (incf count)))
       nil
       'tree))
    (message "Number of subheadings below this: %d" count)))
;; Example usage in your personal ~/.emacs.private.el file.
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (my/org-create-dynamic-link
;;              "\\([[:alpha:]]\\{2,5\\}-[[:digit:]]+\\)"
;;              "https://www.example.com/%s")))

(bind-keys :map org-mode-map
           ("M-<down>" . outline-backward-same-level)
           ("M-<up>" . outline-forward-same-level)
           ("C-c t" . org-toggle-link-display))
