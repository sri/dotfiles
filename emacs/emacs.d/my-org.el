(require 'org)
(require 'org-bullets)

(setq org-bullets-bullet-list '("◉" "○" "✸"))

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
            ;; 3rd arg T says to modify the buffer-local hook
            (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
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

(defun my-org-insert-week-template ()
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

  (interactive)
  (let ((week-start (current-time))
        (oneday (seconds-to-time 86400)))
    (loop
      (if (string= (format-time-string "%w" week-start) "1")
          (return)
        (setq week-start (time-subtract week-start oneday))))
    (let ((week-hdr "%Y-%m-%d (week %U)")
          (day-hdr "<%Y-%m-%d %a - day %j>"))
    (save-excursion
      (insert "** " (format-time-string week-hdr week-start) "\n")
      (dotimes (i 7)
        (insert "*** " (format-time-string day-hdr week-start) "\n")
        (setq week-start (time-add week-start oneday)))))))


;; Links:
(org-add-link-type "gitsha" 'my-org-show-git-sha)

(require 'magit)
(defun my-org-show-git-sha (sha)
  (magit-show-commit sha))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Dynamic links

;; In an Org buffer, this converts words such as `JIRA-123' into links
;; to a JIRA ticket without converting it into a Org link. See
;; `my-org-dynamic-link-url-prefix' for the URL prefix prepended to
;; the word before opening the link. Dynamic link can only be clicked
;; on by the mouse. Hitting Return on them does nothing.
;;
;; See `org-activate-plain-links' and `org-set-font-lock-defaults'
;; for an example of how this is done in Org mode.
;; Another way to achieve this is with `goto-address-mode'.

(defface my-org-dynamic-link-face
  '((t :foreground "#268bd2" :box 1 :weight bold :inherit unspecified))
  "Face for Org dynamic links.")

(defvar my-org-dynamic-links-matcher
  '()
  "Matcher for dynamic links.
Each element must be a 2-element list of the format:
\(REGEX URL-PREFIX)

REGEX should match whichever word you want to convert
to a link. For JIRA tickets, this should be a regexp
that matches the format `JIRA-123'.

URL-PREFIX should be the URL to open when the link is clicked. If
it contains a \"%s\", then it will be replaced with the matched
word. If that isn't present, then the URL-PREFIX is visited.")

(defun my-org-activate-dynamic-links (limit)
  (let ((matchers my-org-dynamic-links-matcher)
        (result nil)
        (regex)
        (link-template))
    (while (and matchers
                (null result))

      (setq regex (caar matchers)
            link-template (cadar matchers)
            matchers (cdr matchers))

      ;; Below is mostly copied from `org-activate-plain-links'.
      (when (and (re-search-forward regex limit t)
                 (not (org-in-src-block-p)))
        (let ((face
               (get-text-property (max (1- (match-beginning 0)) (point-min))
                                  'face))
              (link
               (if (save-match-data (string-match "%s" link-template))
                   (format link-template (org-match-string-no-properties 0))
                 link-template)))
          (unless (if (consp face) (memq 'org-tag face) (eq 'org-tag face))
            (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
            (add-text-properties (match-beginning 0) (match-end 0)
                                 (list 'mouse-face 'highlight
                                       'face 'my-org-dynamic-link-face
                                       'htmlize-link `(:uri ,link)
                                       'keymap org-mouse-map))
            (org-rear-nonsticky-at (match-end 0))
            (setq result t)))))
    result))

;; Puts our function into the `font-lock-defaults'.
(add-hook 'org-font-lock-set-keywords-hook
          (lambda ()
            (nconc org-font-lock-extra-keywords
                   (list '(my-org-activate-dynamic-links (0 'my-org-dynamic-link-face t))))))

;; Open the link when it is clicked on.
(add-hook 'org-open-at-point-functions
          (lambda ()
            (let ((link (get-text-property (point) 'htmlize-link)))
              (when (cadr link)
                (browse-url (cadr link))
                t))))
