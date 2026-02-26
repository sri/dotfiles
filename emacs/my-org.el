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
(setq org-fontify-done-headline nil)
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

(setq org-hierarchical-todo-statistics nil)
(setq org-checkbox-hierarchical-statistics nil)
(setq org-provide-todo-statistics 'all-headlines)

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


;; - `TODO` → `!` +  📌
;; - `IDEA` → `?` + 💡
;; - `NOW` → `.` + 🔥
;; - `NEXT` → `>` + ➜
;; - `WAIT` → `~` + ⏳
;; - `DONE` → `+` + ✅
;; - `CANCELLED` → `x` + ❌


(setq org-todo-keywords
     '((sequence "TODO(t)" "IDEA(i)" "NOW(n)" "NEXT(e)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; Theme-specific `org-todo-keyword-faces` are applied from
;; `emacs/theme-overrides/my-solarized-dark-faces.el`.

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following is to make grandparent nodes of checkboxes have the
;; totals. Useful to highlight the stats.
;;
;; Example:
;;
;; * grandparent [2/3]
;; ** parent [2/3]
;; - [X] test 1
;; - [ ] test 2
;; - [X] test 3
;;
;; Another example:
;;
;; * grandparent [5/8]
;; ** parent [3/4]
;; - [X] test 1
;; - [X] test 2
;; - [X] test 3
;; - [ ]
;; ** parent 2 [2/4]
;;  - [X] another test 1
;; *** sub parent [1/3]
;; - [ ] test 1
;; - [X] test 2
;; - [ ] test 3
;;
;; By Codex mostly; had to step into edebug and tell it where it
;; got stuck after its first attempt.
;; Original here: https://github.com/emacs-mirror/emacs/blob/master/lisp/org/org-list.el
;;
;; Explanation from Codex of the changes:
;; Here’s exactly what changed in org-list.el to make it work:
;;
;; Added an internal recursion guard arg
;; org-update-checkbox-count is now (&optional all no-ancestor) so ancestor-triggered calls don’t recursively re-walk ancestors.
;; org-list.el (line 2528)
;; org-list.el (line 2537)
;;
;; Added explicit ancestor updates
;; After updating the current section, it now goes to the current heading and walks upward with org-up-heading-safe, calling org-update-checkbox-count on each ancestor.
;; org-list.el (line 2682)
;;
;; Fixed heading-cookie scope detection
;; While evaluating each cookie, the code now detects whether the cookie is on a heading (headline) vs inside an item/block (container).
;; org-list.el (line 2605)
;;
;; Critical fix for COOKIE_DATA: recursive on headings
;; For recursive heading cookies, search/count scope is now the whole heading subtree (org-element-contents-end headline) instead of stopping at the next heading.
;; org-list.el (line 2618)
;; org-list.el (line 2621)
;;
;; Non-recursive behavior preserved
;; For non-recursive heading cookies, scope still ends at outline-next-heading (same local-section behavior as before).
;; org-list.el (line 2626)
;;
;; Why your example now works: ancestor traversal reaches * grandparent, and for that recursive cookie, counting now includes descendants like *** parent’s checkbox list.
;;
;; How to optimize this by Codex
;; ==============================
;; loop is at org-list.el (line 2682)
;; it uses org-up-heading-safe, so it walks ancestor headings to the top heading chain.
;; It does not scan unrelated branches, but with COOKIE_DATA: recursive it can rescan large subtrees for each ancestor cookie, which is the expensive part.
;;
;; Most useful efficiency controls:
;;
;; Keep recursive only where you really need it.
;; Skip ancestor updates when the ancestor heading has no stats cookie (cheap micro-optimization).
;; Add a cap, e.g. org-checkbox-ancestor-update-levels:
;; nil = no ancestor updates (current subtree/local only)
;; 1 = parent only
;; 2 = parent + grandparent
;; all = current behavior

(require 'org-list)

(defun org-update-checkbox-count (&optional all no-ancestor)
  "Update the checkbox statistics in the current section.

This will find all statistic cookies like [57%] and [6/12] and
update them with the current numbers.

With optional prefix argument ALL, do this for the whole buffer.
When ALL is symbol `narrow', update statistics only in the accessible
portion of the buffer.
Optional argument NO-ANCESTOR is for internal use and inhibits
ancestor cookie updates."
  (interactive "P")
  (save-excursion
    (save-restriction
      (unless (eq all 'narrow) (widen))
      (let* ((cookie-re "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)")
	     (box-re "^[ \t]*\\([-+*]\\|\\([0-9]+\\|[A-Za-z]\\)[.)]\\)[ \t]+\
\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\][ \t]*\\)?\\(\\[[- X]\\]\\)")
             (cookie-data (or (org-entry-get nil "COOKIE_DATA") ""))
	     (recursivep
	      (or (not org-checkbox-hierarchical-statistics)
	          (string-match-p "\\<recursive\\>" cookie-data)))
	     (within-inlinetask (and (not all)
				     (featurep 'org-inlinetask)
				     (org-inlinetask-in-task-p)))
	     (end (cond (all (point-max))
		        (within-inlinetask
		         (save-excursion (outline-next-heading) (point)))
		        (t (save-excursion
			     (org-with-limited-levels (outline-next-heading))
			     (point)))))
	     (count-boxes
	      (lambda (item structs recursivep)
	        ;; Return number of checked boxes and boxes of all types
	        ;; in all structures in STRUCTS.  If RECURSIVEP is
	        ;; non-nil, also count boxes in sub-lists.  If ITEM is
	        ;; nil, count across the whole structure, else count only
	        ;; across subtree whose ancestor is ITEM.
	        (let ((c-on 0) (c-all 0))
	          (dolist (s structs (list c-on c-all))
		    (let* ((pre (org-list-prevs-alist s))
			   (par (org-list-parents-alist s))
			   (items
			    (cond
			     ((and recursivep item) (org-list-get-subtree item s))
			     (recursivep (mapcar #'car s))
			     (item (org-list-get-children item s par))
			     (t (org-list-get-all-items
			         (org-list-get-top-point s) s pre))))
			   (cookies (delq nil (mapcar
					     (lambda (e)
					       (org-list-get-checkbox e s))
					     items))))
		      (cl-incf c-all (length cookies))
		      (cl-incf c-on (cl-count "[X]" cookies :test #'equal)))))))
	     cookies-list cache)
        ;; Move to start.
        (cond (all (goto-char (point-min)))
	      (within-inlinetask (org-back-to-heading t))
	      (t (org-with-limited-levels (outline-previous-heading))))
        ;; Build an alist for each cookie found.  The key is the position
        ;; at beginning of cookie and values ending position, format of
        ;; cookie, number of checked boxes to report and total number of
        ;; boxes.
	        (while (re-search-forward cookie-re end t)
	          (let ((context (save-excursion (backward-char)
					         (save-match-data (org-element-context)))))
		    (when (and (org-element-type-p context 'statistics-cookie)
		               (not (string-match-p "\\<todo\\>" cookie-data)))
		      (push
		       (append
		        (list (match-beginning 1) (match-end 1) (match-end 2))
		        (let* ((container
			        (org-element-lineage
			         context
			         '(drawer center-block dynamic-block inlinetask item
				          quote-block special-block verse-block)))
			       (headline
			        (and (not container)
				     (org-element-lineage context '(headline))))
			       (beg
			        (cond
			         (container (org-element-contents-begin container))
			         (headline (or (org-element-begin headline)
				               (org-element-end headline)))
			         (t
			          (save-excursion
			            (org-with-limited-levels
			             (outline-previous-heading))
			            (point)))))
			       (scope-end
			        (cond
			         (container (org-element-contents-end container))
			         ((and recursivep headline)
			          ;; Recursive heading cookies should account for
			          ;; checkbox states across the whole subtree.
			          (or (org-element-contents-end headline)
			              (org-element-end headline)))
			         (headline
			          (save-excursion
			            (goto-char (org-element-begin headline))
			            (org-with-limited-levels (outline-next-heading))
			            (point)))
			         (t
			          (save-excursion
			            (org-with-limited-levels (outline-next-heading))
			            (point))))))
		          (or (cdr (assq beg cache))
			      (save-excursion
			        (goto-char beg)
			        (let ((end scope-end)
				      structs)
			          (while (re-search-forward box-re end t)
			            (let ((element (org-element-at-point)))
			              (when (org-element-type-p element 'item)
			                (push (org-element-property :structure element)
				              structs)
			                ;; Skip whole list since we have its
			                ;; structure anyway.
			                (while (setq element (org-element-lineage
						              element 'plain-list))
			                  (goto-char
				           (min (org-element-end element)
				                end))))))
			          ;; Cache count for cookies applying to the same
			          ;; area.  Then return it.
			          (let ((count
				         (funcall count-boxes
				                  (and (org-element-type-p
					                container 'item)
					               (org-element-property
					                :begin container))
				                  structs
				                  recursivep)))
			            (push (cons beg count) cache)
			            count))))))
		       cookies-list))))
        ;; Apply alist to buffer.
        (dolist (cookie cookies-list)
          (let* ((beg (car cookie))
	         (end (nth 1 cookie))
	         (percent (nth 2 cookie))
	         (checked (nth 3 cookie))
	         (total (nth 4 cookie)))
	    (goto-char beg)
            (org-fold-core-ignore-modifications
	      (insert-and-inherit
	       (if percent (format "[%d%%]" (floor (* 100.0 checked)
					           (max 1 total)))
	         (format "[%d/%d]" checked total)))
	      (delete-region (point) (+ (point) (- end beg))))
	    (when org-auto-align-tags (org-fix-tags-on-the-fly)))))))
  ;; A checkbox can live below more than one heading, and each ancestor
  ;; can hold its own statistics cookie.
  (when (and (not all) (not no-ancestor))
    (save-excursion
      (when (ignore-errors (org-back-to-heading t))
        (while (org-up-heading-safe)
          (org-update-checkbox-count nil t))))))
