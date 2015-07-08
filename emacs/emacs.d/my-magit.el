(require 'magit)
(require 'advice)

(defadvice magit-show-level-1-all (after my-magit-show-level-1-all)
  (goto-char (point-max))
  (next-line -1)
  (magit-toggle-section))
(ad-activate 'magit-show-level-1-all)

(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-diff-highlight-hunk-body nil)
(defun my-git-grep ()
  (interactive)
  (let* ((search (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                   (completing-read "git grep: "
                                    nil nil nil (current-word))))
         (cmd (concat "cd \"%s\" && "
                      "git --no-pager grep -P -n \"%s\" "
                      "`git rev-parse --show-toplevel`"))
         (buffer-name (format "*git grep: %s*" search))
         (compilation-buffer-name-function
          ;; Fix me: should return unique name
          (lambda (ignore) buffer-name)))
    (with-current-buffer (get-buffer-create buffer-name)
      (setq truncate-lines t))
    (grep-find (format cmd (expand-file-name default-directory)
                       search))))

(defun my-magit-view-diff ()
  "View each file diff.
Find the first diff section (after point) and opens it up for
viewing.  Subsequent calls to this command will either scroll
thru the diff until the end is visible on the window. After that,
it'll close the current section and open the next one."
  (interactive)
  (let ((continue (eq last-command 'my-magit-view-diff))
        (current (magit-current-section))
        (done nil))
    ;; Find a diff or hunk section after point
    (while (and (not done)
                (not (member (magit-section-type current) '(diff hunk))))
      (if (null (magit-find-section-after (point)))
          (setq done t)
        (magit-goto-next-section)
        (setq current (magit-current-section))
        ;; magit-diff ends with a "back" button and it seems to be
        ;; considered a magit section.
        (when (eq (magit-section-type current) 'button)
          (setq done t))))
    (cond (done
           (message "Done"))
          ((or continue
               (eq (magit-section-type current) 'hunk))
           (let ((parent (if (eq (magit-section-type current) 'diff)
                             current
                           (magit-section-parent current))))
             (unless (eq (magit-section-type parent) 'diff)
               (error "Parent of hunk is not a diff but is %s"
                      (magit-section-type parent)))
             ;; If the current diff has more content that is visible
             ;; in the current window, scroll up and let the user view
             ;; it.  Otherwise, close this diff section and open the
             ;; next one.
             (cond ((pos-visible-in-window-p (magit-section-end parent))
                    (goto-char (magit-section-beginning parent))
                    (magit-hide-section)
                    (recenter 0)
                    (if (null (magit-find-section-after (point)))
                        (message "Done")
                      (magit-goto-next-section)
                      (if (eq (magit-section-type (magit-current-section)) 'diff)
                          (magit-show-section)
                        (message "Done"))))
                   (t (scroll-up)))))
          ((eq (magit-section-type current) 'diff)
           ;; Initial viewing
           (goto-char (magit-section-beginning current))
           (magit-show-section)
           (recenter 0)))))

(mapc (lambda (mode-map)
        (define-key mode-map (kbd ",")
          'my-magit-view-diff))
      (list magit-status-mode-map
            magit-diff-mode-map))

;; Don't highlight sections.
(defun magit-highlight-section ()
  nil)

(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)

(setq magit-status-buffer-switch-function 'switch-to-buffer)

;; (defun my-magit-close-current-open-next ()
;;   (interactive)
;;   (magit-goto-parent-section)
;;   (magit-toggle-section)
;;   (magit-goto-next-section)
;;   (recenter 0))

;; (defun my-magit-click ()
;;   (cond ((memq major-mode '(magit-log-mode magit-branch-manager-mode))
;;          (magit-show-item-or-scroll-up))
;;         ((eq major-mode 'magit-status-mode)
;;          (let* ((current (magit-current-section))
;;                 (current-title (format "%s" (magit-section-title current)))
;;                 (parent (magit-section-parent current)))
;;            (if (and parent
;;                     (eq (magit-section-title parent) 'stashes))
;;                (magit-show-item-or-scroll-up)
;;              (unless (string-prefix-p "@@" current-title)
;;                (magit-toggle-section)))))
;;         ((memq major-mode '(magit-wazzup-mode
;;                             magit-commit-mode
;;                             magit-log-edit-mode
;;                             magit-stash-mode
;;                             magit-reflog-mode
;;                             magit-diff-mode))
;;          (magit-toggle-section))))
