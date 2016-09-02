(require 'org)
(require 'org-bullets)

(setq org-bullets-bullet-list '("◉" "○"))

(set-register ?t '(file . "~/Dropbox/Notes/todo.org"))

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
            (org-bullets-mode 1)
            (linum-mode -1)
            (setq cursor-type 'hbar)))

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
(setq org-log-done 'time)

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING"
                  "|"
                  "DONE" "CANCELED")))

(defun my-org-insert-chrome-link ()
  (interactive)
  (let ((subject (do-applescript "tell application \"Google Chrome\"
                                  title of active tab of front window
                                  end tell"))
        (url (do-applescript "tell application \"Google Chrome\"
                              URL of active tab of front window
                              end tell")))
    (insert (org-make-link-string url subject))))

;;;
(defun my-org-insert-week-template (&optional arg)
  "Insert org headline template for the week.

Example:

** 2016-07-04 (week 27)
*** <2016-07-04 Mon - day 186>
*** <2016-07-05 Tue - day 187>
*** <2016-07-06 Wed - day 188>
*** <2016-07-07 Thu - day 189>
*** <2016-07-08 Fri - day 190>
*** <2016-07-09 Sat - day 191>
*** <2016-07-10 Sun - day 192>"

  (interactive "P")
  (lexical-let ((current nil)
                (oneday (seconds-to-time 86400)))

    (labels ((find-week-start-or-end (spec cur)
               (let ((stop (if (eq spec 'start) "1" "7"))
                     (fn (if (eq spec 'start) 'time-subtract 'time-add)))
                 (loop (if (string= (format-time-string "%u" cur) stop)
                           (return cur)
                         (setq cur (funcall fn cur oneday))))))

             (current-week ()
               (let* ((week-end (find-week-start-or-end 'end current))
                      ;; Tue Jul 19, 2016
                      (day "%a %b %d, %Y")
                      (week (format-time-string "%U" current))
                      (start (format-time-string day current))
                      (end (format-time-string day week-end)))
                 (format "Week %s (%s - %s)" week start end)))

             (display-current-in-minibuffer ()
               (delete-region (point-at-bol) (point-at-eol))
               (insert (current-week)))

             (prev-week ()
               (interactive)
               (when (minibufferp)
                 (let ((week-start (find-week-start-or-end 'start current)))
                   (setq week-start (time-subtract week-start oneday))
                   (setq current (find-week-start-or-end 'start week-start))
                   (display-current-in-minibuffer))))

             (next-week ()
               (interactive)
               (when (minibufferp)
                 (let ((week-end (find-week-start-or-end 'end current)))
                   (setq current (time-add week-end oneday))
                   (display-current-in-minibuffer)))))

      (setq current (find-week-start-or-end 'start (current-time)))

      (let* ((minibuf-map (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "<left>")
                              (lambda ()
                                (interactive)
                                (prev-week)))
                            (define-key map (kbd "<right>")
                              (lambda ()
                                (interactive)
                                (next-week)))
                            (define-key map (kbd "<return>") 'exit-minibuffer)
                            (define-key map (kbd "C-g") 'exit-minibuffer)
                            map))
             (week-start (if (not arg)
                             (find-week-start-or-end 'start (current-time))
                           (read-from-minibuffer "Insert week: "
                                                 (current-week) minibuf-map)
                           current))
             (week-header "%Y-%m-%d (week %U)")
             (day-header "<%Y-%m-%d %a - day %j>"))

        (save-excursion
          (insert "** " (format-time-string week-header week-start) "\n")
          (dotimes (i 7)
            (insert "*** " (format-time-string day-header week-start) "\n")
            (setq week-start (time-add week-start oneday))))))))

;; Links:
(org-add-link-type "gitsha" 'my-org-show-git-sha)

(require 'magit)
(defun my-org-show-git-sha (sha)
  (magit-show-commit sha))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'button-lock)
(require 'thingatpt)

(add-hook 'org-mode-hook
          (lambda () (button-lock-mode 1)))

(defface my-org-ticket-face
  '((t :foreground "#268bd2" :underline t :inherit unspecified))
  "Face for Org dynamic links.")

(defun my-org-create-dynamic-link (regex url-template)
  (lexical-let ((url-template url-template))
    (button-lock-set-button
     regex
     (lambda ()
       (interactive)
       (let* ((matched-string (thing-at-point 'symbol))
              (url (if (save-match-data (string-match "%s" url-template))
                       (format url-template matched-string)
                     url-template)))
         (browse-url url)))
     :face 'link)))
