;; -*- lexical-binding: t; -*-
(require 'ace-window)
(require 'browse-url)
(require 'embark)
(require 'seq)
(require 'subr-x)
(require 'thingatpt)
(require 'url-util)
(require 'vc-git)

(defvar my/ticket-regexp "\\b[A-Z]\\{3,5\\}-[0-9]\\{1,5\\}\\b")

(defvar my/ticket-url-prefix nil
  "URL prefix or format string used to open tickets.
If the value contains `%s', format it with the ticket id.
Otherwise concatenate the value and the ticket id.")

(defvar my/embark-ticket-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'my/embark-ticket-open)
    (define-key map (kbd "b") #'my/embark-ticket-open-branch-on-github)
    (define-key map (kbd "p") #'my/embark-ticket-open-pr-on-github)
    (define-key map (kbd "m") #'my/embark-ticket-diff-against-main)
    (define-key map (kbd "d") #'my/embark-ticket-diff-against-develop)
    (define-key map (kbd "g") #'my/embark-ticket-grep-repo)
    (define-key map (kbd "G") #'my/embark-ticket-log-commits)
    map))

(defun my/embark-ticket--repo-root ()
  (or (vc-git-root default-directory)
      (error "Not in a Git repository")))

(defun my/embark-ticket--all-branches ()
  (let ((default-directory (my/embark-ticket--repo-root)))
    (process-lines "git" "for-each-ref" "--format=%(refname:short)"
                   "refs/heads" "refs/remotes")))

(defun my/embark-ticket--dedupe-branches (branches)
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (branch branches)
      (let ((display (string-remove-prefix "origin/" branch)))
        (unless (gethash display seen)
          (puthash display t seen)
          (push branch result))))
    (nreverse result)))

(defun my/embark-ticket--matching-branches (ticket)
  (my/embark-ticket--dedupe-branches
   (seq-filter (lambda (branch)
                 (string-match-p (regexp-quote ticket) branch))
               (my/embark-ticket--all-branches))))

(defun my/embark-ticket--read-branch (ticket)
  (let* ((matching (my/embark-ticket--matching-branches ticket))
         (branches (or matching
                       (my/embark-ticket--dedupe-branches
                        (my/embark-ticket--all-branches)))))
    (cond
     ((= (length matching) 1)
      (car matching))
     ((> (length matching) 1)
      (completing-read (format "Branch for %s: " ticket) matching nil t))
     (t
      (completing-read (format "No branch match for %s; branch: " ticket)
                       branches nil t)))))

(defun my/embark-ticket--branch-web-name (branch)
  (string-remove-prefix "origin/" branch))

(defun my/embark-ticket--repo-url ()
  (let ((default-directory (my/embark-ticket--repo-root)))
    (car (process-lines "gh" "repo" "view" "--json" "url" "-q" ".url"))))

(defun my/embark-ticket--pr-url (branch)
  (let* ((default-directory (my/embark-ticket--repo-root))
         (branch (my/embark-ticket--branch-web-name branch)))
    (condition-case nil
        (car (process-lines "gh" "pr" "view" branch "--json" "url" "-q" ".url"))
      (error
       (let* ((repo-url (my/embark-ticket--repo-url))
              (repo-path (replace-regexp-in-string "\\`https://github\\.com/" ""
                                                   repo-url))
              (owner (car (split-string repo-path "/")))
              (query (url-hexify-string
                      (format "is:pr head:%s:%s" owner branch))))
         (format "%s/pulls?q=%s" repo-url query))))))

(defun my/embark-ticket--ticket-url (ticket)
  (unless (and my/ticket-url-prefix
               (not (string-empty-p my/ticket-url-prefix)))
    (error "`my/ticket-url-prefix' is not configured"))
  (if (string-match-p "%s" my/ticket-url-prefix)
      (format my/ticket-url-prefix ticket)
    (concat my/ticket-url-prefix ticket)))

(defun my/embark-ticket--target-at-point ()
  (save-match-data
    (when (thing-at-point-looking-at my/ticket-regexp)
      (cons 'ticket
            (cons (match-string-no-properties 0)
                  (cons (match-beginning 0) (match-end 0)))))))

(defun my/embark-ticket-open (ticket)
  (browse-url (my/embark-ticket--ticket-url ticket)))

(defun my/embark-ticket-open-branch-on-github (ticket)
  (let* ((branch (my/embark-ticket--read-branch ticket))
         (url (format "%s/tree/%s"
                      (my/embark-ticket--repo-url)
                      (url-hexify-string
                       (my/embark-ticket--branch-web-name branch)))))
    (browse-url url)))

(defun my/embark-ticket-open-pr-on-github (ticket)
  (browse-url
   (my/embark-ticket--pr-url
    (my/embark-ticket--read-branch ticket))))

(defun my/embark-ticket-diff-against-main (ticket)
  (require 'magit)
  (let ((branch (my/embark-ticket--read-branch ticket)))
    (magit-diff-range (format "origin/main...%s" branch))))

(defun my/embark-ticket-diff-against-develop (ticket)
  (require 'magit)
  (let ((branch (my/embark-ticket--read-branch ticket)))
    (magit-diff-range (format "origin/develop...%s" branch))))

(defun my/embark-ticket-grep-repo (ticket)
  (require 'consult)
  (let ((default-directory (my/embark-ticket--repo-root)))
    (consult-git-grep default-directory ticket)))

(defun my/embark-ticket-log-commits (ticket)
  (require 'magit)
  (let ((default-directory (my/embark-ticket--repo-root)))
    (magit-log-all (list (format "--grep=%s" ticket)))))

(defun my/ace-window-switch-to-selected-window ()
  (let ((aw-dispatch-always t))
    (when-let* ((selected-win (aw-select nil)))
      ;; If selected-win is nil, probably a dispatch action (via key '?')
      ;; occurred and point will be in the selected window already.
      (aw-switch-to-window selected-win))))

;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(defun my/embark-ace-action-find-file ()
  "Open file and position it using ace-window."
  (interactive)
  (my/ace-window-switch-to-selected-window)
  (call-interactively 'find-file))

(define-key embark-file-map (kbd "o") 'my/embark-ace-action-find-file)


(defun my/embark-ace-action-switch-to-buffer ()
  "View buffer and position it using ace-window."
  (interactive)
  (my/ace-window-switch-to-selected-window)
  (call-interactively 'switch-to-buffer))

(define-key embark-buffer-map (kbd "o") 'my/embark-ace-action-switch-to-buffer)

;; Helpers for Embark multi-target actions
(defun my/embark--as-list (items)
  (if (listp items) items (list items)))

(defun my/embark--open-in-new-tabs (items open-fn empty-msg)
  (unless items (user-error empty-msg))
  (let ((orig-tab (1+ (tab-bar--current-tab-index (funcall tab-bar-tabs-function)))))
    (dolist (item items)
      (tab-bar-new-tab)
      (funcall open-fn item))
    (tab-bar-select-tab (1+ orig-tab))))

(defun my/embark--open-in-side-by-side-windows (items open-fn empty-msg)
  (unless items (user-error empty-msg))
  (delete-other-windows)
  (funcall open-fn (car items))
  (dolist (item (cdr items))
    (split-window-below)
    (other-window 1)
    (funcall open-fn item))
  (balance-windows))

(defun my/embark--unique-files (files)
  (let* ((files (my/embark--as-list files))
         (files (mapcar (lambda (f) (expand-file-name f default-directory)) files)))
    (delete-dups files)))

(defun my/embark--live-unique-buffers (buffers)
  (let* ((buffers (my/embark--as-list buffers))
         (buffers (mapcar (lambda (b) (if (bufferp b) b (get-buffer b))) buffers))
         (buffers (delq nil buffers)))
    (delete-dups buffers)))

(defun my/embark--existing-bookmarks (bookmarks)
  (let (result)
    (dolist (bookmark (my/embark--as-list bookmarks))
      (let ((name (if (stringp bookmark) bookmark (format "%s" bookmark))))
        (when (bookmark-get-bookmark name 'noerror)
          (push name result))))
    (delete-dups (nreverse result))))

(defun my/embark-open-files-in-new-tabs (files)
  "Open FILES, one per new tab."
  (interactive)
  (my/embark--open-in-new-tabs
   (my/embark--unique-files files)
   #'find-file
   "No files selected"))

(defun my/embark-open-files-in-side-by-side-windows (files)
  "Open FILES in current frame, one file per side-by-side window."
  (interactive)
  (my/embark--open-in-side-by-side-windows
   (my/embark--unique-files files)
   #'find-file
   "No files selected"))

(defun my/embark-open-buffers-in-new-tabs (buffers)
  "Open BUFFERS, one per new tab."
  (interactive)
  (my/embark--open-in-new-tabs
   (my/embark--live-unique-buffers buffers)
   #'switch-to-buffer
   "No buffers selected"))

(defun my/embark-open-buffers-in-side-by-side-windows (buffers)
  "Open BUFFERS in current frame, one buffer per side-by-side window."
  (interactive)
  (my/embark--open-in-side-by-side-windows
   (my/embark--live-unique-buffers buffers)
   #'switch-to-buffer
   "No buffers selected"))

(defun my/embark-open-bookmarks-in-new-tabs (bookmarks)
  "Open BOOKMARKS, one per new tab."
  (interactive)
  (my/embark--open-in-new-tabs
   (my/embark--existing-bookmarks bookmarks)
   #'bookmark-jump
   "No bookmarks selected"))

(defun my/embark-open-bookmarks-in-side-by-side-windows (bookmarks)
  "Open BOOKMARKS in current frame, one bookmark per side-by-side window."
  (interactive)
  (my/embark--open-in-side-by-side-windows
   (my/embark--existing-bookmarks bookmarks)
   #'bookmark-jump
   "No bookmarks selected"))

(defun my/embark--bind-open-actions (keymap open-tabs-fn open-windows-fn)
  (define-key keymap (kbd "T") open-tabs-fn)
  (define-key keymap (kbd "W") open-windows-fn))

(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist '(ticket my/embark-ticket-map))
  (add-hook 'embark-target-finders #'my/embark-ticket--target-at-point)

  ;; No "Run ... on N files?" prompt
  (setq embark-confirm-act-all nil)

  ;; Make RET default to find-file for file candidates
  (add-to-list 'embark-default-action-overrides '(file . find-file))

  ;; Tell embark-act-all to pass the whole selected list to these commands
  (dolist (fn '(my/embark-open-files-in-new-tabs
                my/embark-open-files-in-side-by-side-windows
                my/embark-open-buffers-in-new-tabs
                my/embark-open-buffers-in-side-by-side-windows
                my/embark-open-bookmarks-in-new-tabs
                my/embark-open-bookmarks-in-side-by-side-windows))
    (add-to-list 'embark-multitarget-actions fn))

  (bind-keys :map embark-variable-map
             ("k" . describe-keymap))

  ;; Actions in file, buffer and bookmark action maps
  (my/embark--bind-open-actions embark-file-map
                                #'my/embark-open-files-in-new-tabs
                                #'my/embark-open-files-in-side-by-side-windows)
  (my/embark--bind-open-actions embark-buffer-map
                                #'my/embark-open-buffers-in-new-tabs
                                #'my/embark-open-buffers-in-side-by-side-windows)
  (my/embark--bind-open-actions embark-bookmark-map
                                #'my/embark-open-bookmarks-in-new-tabs
                                #'my/embark-open-bookmarks-in-side-by-side-windows))

(defun my/embark-select-and-next ()
  "Select current Vertico candidate, then move down one (no wrap)."
  (interactive)
  (embark-select)
  (when (and (boundp 'vertico--total)
             (> vertico--total 0)
             (< vertico--index (1- vertico--total)))
    (vertico-next 1)))

(bind-keys :map vertico-map
           ("C-;" . embark-act-all)
           ("C-SPC" . my/embark-select-and-next))
