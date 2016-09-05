;; My Activity Log
(require 'org)
(require 'org-clock) ; for org-clock-special-range

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activity Log -- Date Functions

(defvar my-activity-log-oneday (seconds-to-time 86400))

(defun my-activity-log-week-range (&optional time)
  (let ((week-start-day 1)) ; Monday
    (org-clock-special-range 'thisweek time nil week-start-day)))

(defun my-activity-log-next-week (time)
  (let ((range (my-activity-log-week-range time)))
    (time-add (cadr range) my-activity-log-oneday)))

(defun my-activity-log-prev-week (time)
  (let ((range (my-activity-log-week-range time)))
    (time-subtract (car range) my-activity-log-oneday)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activity Log -- Minibuffer
(defvar my-activity-log-current-day nil)

(defun my-activity-log-current-week-string ()
  (let ((range (my-activity-log-week-range my-activity-log-current-day))
        (day-template "%a %b %d, %Y"))
    (format "Week %s (%s - %s)"
            (format-time-string "%W" (car range))
            (format-time-string day-template (car range))
            (format-time-string day-template (cadr range)))))

(defun my-activity-log-display-in-minibuffer ()
  (when (minibufferp)
    (delete-region (point-at-bol) (point-at-eol))
    (insert (my-activity-log-current-week-string))))

(defvar my-activity-log-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>")
      (lambda ()
        (interactive)
        (setq my-activity-log-current-day
              (my-activity-log-prev-week my-activity-log-current-day))
        (my-activity-log-display-in-minibuffer)))

    (define-key map (kbd "<right>")
      (lambda ()
        (interactive)
        (setq my-activity-log-current-day
              (my-activity-log-next-week my-activity-log-current-day))
        (my-activity-log-display-in-minibuffer)))

    ;; RET works both in terminal and GUI, but also binds C-m
    ;; <return> only works in GUI
    (define-key map (kbd "RET") 'exit-minibuffer)
    (define-key map (kbd "C-g") 'abort-recursive-edit)
    map))

(defun my-activity-log-read-from-minibuffer (spec)
  (read-from-minibuffer (format "%s week: " (if (eq spec 'start) "Start" "End"))
                        (my-activity-log-current-week-string)
                        my-activity-log-minibuffer-map)
  my-activity-log-current-day)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer functions

(defun my-activity-log-latest-in-current-buffer ()
  (save-excursion
    (goto-char (point-max))
    (let ((header-regex "^[*][*] \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\).*")
          (result nil))
      (while (and (null result)
                  (re-search-backward header-regex nil t))
        ;; Date-to-time needs to have all the components present.
        (setq result (date-to-time (format "%s 00:00:00" (match-string 1)))))
      result)))

(defun my-activity-log-insert-for-week (&optional time)
  (let* ((range (my-activity-log-week-range time))
         (start (car range))
         (header (format-time-string "%Y-%m-%d (week %U)" start)))
    (insert "** " header "\n")
    (dotimes (i 7)
      (let ((day (format-time-string "<%Y-%m-%d %a - day %j>" start)))
        (insert "*** " day "\n")
        (setq start (time-add start my-activity-log-oneday))))))

(defun my-activity-log-insert-template (&optional arg)
  "Inserts activity log template for the week.

With a prefix arg, inserts for a user selected range of weeks at
the current point.

Without the prefix arg, appends to the current buffer, either for
the current date or the week after the latest template present in
the current buffer.

Example template:

** 2016-09-05 (week 36)
*** <2016-09-05 Mon - day 249>
*** <2016-09-06 Tue - day 250>
*** <2016-09-07 Wed - day 251>
*** <2016-09-08 Thu - day 252>
*** <2016-09-09 Fri - day 253>
*** <2016-09-10 Sat - day 254>
*** <2016-09-11 Sun - day 255>"
  (interactive "P")
  (if arg
      (let (start end)
        (setq my-activity-log-current-day nil)
        (setq start (my-activity-log-read-from-minibuffer 'start))
        (setq my-activity-log-current-day (my-activity-log-next-week start))
        (setq end (my-activity-log-read-from-minibuffer 'end))

        (while (time-less-p start end)
          (my-activity-log-insert-for-week start)
          (setq start (my-activity-log-next-week start)))
        (my-activity-log-insert-for-week end))

    ;; Without arg, append to the current buffer the week's template,
    ;; which depends on the latest that is already there or the
    ;; current time now.
    (let* ((latest (my-activity-log-latest-in-current-buffer))
           (time (if latest
                     (my-activity-log-next-week latest)
                   (current-time))))
      (goto-char (point-max))
      (my-activity-log-insert-for-week time))))
