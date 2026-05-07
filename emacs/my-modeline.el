;; -*- lexical-binding: t; -*-
(require 's)
(require 'url-util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
(defmacro def-with-selected-window (name &rest body)
  `(defun ,name (&optional event)
     (interactive (list last-input-event))
     (with-selected-window (if (mouse-event-p (my/effective-mouse-event event))
                               (posn-window (event-start (my/effective-mouse-event event)))
                             (selected-window))
       ,@body)))

(defun my/effective-mouse-event (&optional event)
  (if (mouse-event-p event)
      event
    last-nonmenu-event))

(defmacro def-modeline-var (name &rest body)
  `(progn
     (defvar ,name)
     (setq-default ,name (progn ,@body))
     (make-variable-buffer-local ',name)
     (put ',name 'risky-local-variable t)
     ',name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer modified
(def-with-selected-window my/save-buffer ()
  (save-buffer))

(defvar my/mode-line-buffer-modified-p-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'my/save-buffer)
    (define-key map [mode-line down-mouse-1] 'ignore)
    (define-key map [mode-line S-mouse-1] 'my/unsaved-changes)
    map))

(defun my/buffer-modified-p ()
  (let ((ignore-in-modes '(shell-mode dired-mode)))
    (cond ((memq major-mode ignore-in-modes) " ")
          ((buffer-modified-p) "*")
          (t " "))))

(def-modeline-var my/mode-line-modified
  `(:propertize (:eval (my/buffer-modified-p))
                help-echo "mouse-1: Save buffer\nS-mouse-1: show unsaved changes"
                mouse-face mode-line-highlight
                local-map ,my/mode-line-buffer-modified-p-keymap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer name:
(require 'easymenu)

(defvar my/mode-line-buffer-name-menu-map
  (easy-menu-create-menu
   ""
   '(["Copy filename" my/mode-line-copy-file-name t]
     ["Copy directory" my/mode-line-copy-directory t]
     ["Copy absolute path" my/mode-line-copy-full-path t]
     ["Copy path relative to repo" my/mode-line-copy-file-name-relative-to-repo t]
     ["Copy path with repo name & line num" my/mode-line-copy-file-name-in-repo t]
     ["Copy path in remote web (current branch)" my/mode-line-copy-file-name-in-remote t]
     ["Open path in remote web (current branch)" my/mode-line-open-file-name-in-remote t]
     "---"
     ["Open in Finder" my/mode-line-open-folder]
     ["Open in Sublime" my/mode-line-open-in-sublime]
     ["Copy region for JIRA" my/mode-line-copy-for-jira])))

(def-with-selected-window my/mode-line-copy-for-jira ()
  (if (use-region-p)
    (let ((path (save-excursion
                  (goto-char (region-beginning))
                  (my/mode-line-get-file-name-in-repo)))
          (region (format "{noformat}\n%s\n{noformat}\n"
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end)))))
      (kill-new (format "*%s*:\n%s" path region))
      (message "Copied `%s' with the region for JIRA" path))
    (message "No region selected")))

;; Buffer name: click to copy
(make-face 'my/mode-line-buffer-name-face)
(set-face-attribute 'my/mode-line-buffer-name-face nil
    :inherit 'mode-line-faced
    ;:foreground "#4271ae"
    ;;:height 90
    ;:box '(:line-width 2 :color "#4271ae")
    )

(defun my/mode-line-buffer-identification-help-echo (window object point)
  ;; Don't put a docstring as it'll display and hover over a menu item
  )

(def-with-selected-window my/mode-line-copy-directory ()
  (let ((path (and buffer-file-name
                   (file-name-directory buffer-file-name))))
    (when path
      (kill-new path)
      (message "Copied: `%s'" path))))

(def-with-selected-window my/mode-line-copy-full-path ()
  (let ((path buffer-file-name))
    (when path
      (kill-new path)
      (message "Copied: `%s'" path))))

(def-with-selected-window my/mode-line-copy-file-name ()
  (let ((path (and buffer-file-name
                   (file-name-nondirectory buffer-file-name))))
    (when path
      (kill-new path)
      (message "Copied: `%s'" path))))

(defun my/vc-mode-line--repo-root ()
  (let ((path (or buffer-file-name default-directory)))
    (and path (vc-git-root path))))

(defun my/vc-mode-line--current-branch ()
  (or (magit-get-current-branch)
      (and buffer-file-name
           (vc-git--symbolic-ref buffer-file-name))
      (error "Can't determine current branch")))

(defun my/vc-mode-line--all-branches ()
  (let ((default-directory (or (my/vc-mode-line--repo-root) default-directory)))
    (delete-dups
     (process-lines "git" "for-each-ref" "--format=%(refname:short)"
                    "refs/heads" "refs/remotes"))))

(defun my/vc-mode-line--read-branch ()
  (let* ((default-branch (my/vc-mode-line--current-branch))
         (branch (completing-read "Branch: "
                                  (my/vc-mode-line--all-branches)
                                  nil t nil nil default-branch)))
    (string-remove-prefix "origin/" branch)))

(defun my/vc-mode-line--github-repo-base ()
  (let ((url (magit-get "remote" "origin" "url")))
    (unless url
      (error "No origin remote configured"))
    (setq url (s-trim url))
    (cond
     ((string-match "\\`git@github\\.com:\\(.+?\\)\\(?:\\.git\\)?/?\\'" url)
      (format "https://github.com/%s" (match-string 1 url)))
     ((string-match "\\`ssh://git@github\\.com[:/]\\(.+?\\)\\(?:\\.git\\)?/?\\'" url)
      (format "https://github.com/%s" (match-string 1 url)))
     ((string-match "\\`https?://github\\.com/\\(.+?\\)\\(?:\\.git\\)?/?\\'" url)
      (format "https://github.com/%s" (match-string 1 url)))
     (t
      (error "Origin is not a github.com remote: %s" url)))))

(defun my/vc-mode-line--github-repo ()
  (replace-regexp-in-string
   "\\`https://github\\.com/" ""
   (my/vc-mode-line--github-repo-base)))

(defun my/vc-mode-line--relative-file-name ()
  (unless buffer-file-name
    (error "Current buffer is not visiting a file"))
  (let ((repo-root (my/vc-mode-line--repo-root)))
    (unless repo-root
      (error "Current buffer is not in a Git repo"))
    (file-relative-name buffer-file-name repo-root)))

(defun my/vc-mode-line--github-path (path)
  (mapconcat #'url-hexify-string (split-string path "/" t) "/"))

(defun my/vc-mode-line--github-ref (ref)
  (url-hexify-string ref))

(defun my/vc-mode-line--selected-line-range ()
  (when (use-region-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (line-start (line-number-at-pos start))
           ;; Region end is exclusive, so step back one char to avoid
           ;; anchoring to the next line when the region ends at BOL.
           (line-end (line-number-at-pos (max start (1- end)))))
      (cons line-start line-end))))

(defun my/vc-mode-line--current-line-web-url ()
  (if-let ((line-range (my/vc-mode-line--selected-line-range)))
      (my/vc-mode-line--file-web-url nil (car line-range) (cdr line-range))
    (my/vc-mode-line--file-web-url nil (line-number-at-pos))))

(defun my/vc-mode-line--file-web-url (&optional branch line-start line-end)
  (concat
   (format "%s/blob/%s/%s"
           (my/vc-mode-line--github-repo-base)
           (my/vc-mode-line--github-ref
            (or branch (my/vc-mode-line--current-branch)))
           (my/vc-mode-line--github-path
            (my/vc-mode-line--relative-file-name)))
   (if line-start
       (if (and line-end (/= line-start line-end))
           (format "#L%d-L%d" line-start line-end)
         (format "#L%d" line-start))
     "")))

(defun my/vc-mode-line--raw-file-web-url (&optional branch)
  (format "https://raw.githubusercontent.com/%s/%s/%s"
          (my/vc-mode-line--github-repo)
          (my/vc-mode-line--github-ref
           (or branch (my/vc-mode-line--current-branch)))
          (my/vc-mode-line--github-path
           (my/vc-mode-line--relative-file-name))))

(defun my/vc-mode-line--pull-request-url ()
  (let ((default-directory (or (my/vc-mode-line--repo-root) default-directory)))
    (condition-case nil
        (car (process-lines "gh" "pr" "view" "--json" "url" "--jq" ".url"))
      (error
       (let* ((repo (my/vc-mode-line--github-repo))
              (owner (car (split-string repo "/")))
              (branch (my/vc-mode-line--current-branch))
              (query (url-hexify-string
                      (format "is:pr head:%s:%s" owner branch))))
         (format "https://github.com/%s/pulls?q=%s" repo query))))))

(defun my/vc-mode-line-copy-url (url)
  (kill-new url)
  (message "Copied %s" url))

(defun my/mode-line-get-file-name-in-repo (&optional exclude-repo-name exclude-line-num)
  (let* ((path buffer-file-name)
         (git-root (my/git-repo-root)))
    (message "path %s" path)
    (setq git-root (s-trim git-root))
    (if (or (string= git-root "")
            (not (string-prefix-p git-root path)))
        (message "Not in git repo")
      (setq path
            (format "%s%s%s"
                    (if exclude-repo-name "" (file-name-nondirectory git-root))
                    (if exclude-repo-name
                        (s-chop-prefix "/" (s-chop-prefix git-root path))
                        (s-chop-prefix git-root path))
                    (if exclude-line-num "" (format "#%d" (line-number-at-pos))))))))

(def-with-selected-window my/mode-line-copy-file-name-in-repo ()
  (let ((path (my/mode-line-get-file-name-in-repo)))
    (kill-new path)
    (message "Copied: `%s'" path)))

(defun my/file-in-remote ()
  (my/vc-mode-line--file-web-url))

(def-with-selected-window my/mode-line-copy-file-name-in-remote ()
  (let ((url (my/file-in-remote)))
    (kill-new url)
    (message "Copied %s" url)))

(def-with-selected-window my/mode-line-open-file-name-in-remote ()
  (let ((url (my/file-in-remote)))
    (browse-url url)
    (message "Opening %s" url)))

(def-with-selected-window my/mode-line-copy-file-name-relative-to-repo ()
  (let ((path (my/mode-line-get-file-name-in-repo t t)))
    (kill-new path)
    (message "Copied: `%s'" path)))

(def-with-selected-window my/mode-line-open-folder ()
  (apply 'call-process "open" nil nil nil
         (if-let (name (buffer-file-name))
           (list "-R" name)
           (list default-directory))))

(def-with-selected-window my/mode-line-open-in-sublime ()
  (let ((path (or buffer-file-name
                  default-directory)))
    (call-process "open" nil nil nil "-a" "Sublime Text" path)))

(defvar my/mode-line-buffer-identification-keymap
  (let ((map (make-sparse-keymap)))
    (bindings--define-key map [mode-line down-mouse-1]
      `(menu-item "Menu Bar" ignore
                  :filter ,(lambda (_)  my/mode-line-buffer-name-menu-map)))
    map))

(setq-default mode-line-buffer-identification
              `(:propertize "%12b"
                            face my/mode-line-buffer-name-face
                            help-echo my/mode-line-buffer-identification-help-echo
                            mouse-face mode-line-highlight
                            local-map ,my/mode-line-buffer-identification-keymap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def-with-selected-window my/mode-line-goto-line
  (call-interactively 'consult-goto-line))

(defvar my/mode-line-column-line-number-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'my/mode-line-goto-line)
    (define-key map [mode-line down-mouse-1] 'ignore)
    map))

(def-modeline-var my/mode-line-position
  `((line-number-mode
     ((column-number-mode
       (10 ,(propertize
	     " %l %C"
	     'local-map my/mode-line-column-line-number-mode-map
	     'mouse-face 'mode-line-highlight
	     'help-echo "mouse-1: goto line"))
       (6 ,(propertize
	    " L%l"
	    'local-map my/mode-line-column-line-number-mode-map
	    'mouse-face 'mode-line-highlight
	    'help-echo "mouse-1: goto line"))))
     ((column-number-mode
       (5 ,(propertize
	    " C%C"
	    'local-map my/mode-line-column-line-number-mode-map
	    'mouse-face 'mode-line-highlight
	    'help-echo "mouse-1: goto line")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vc mode
(require 'vc-hooks)
(require 'vc-git)

(def-with-selected-window my/magit-status ()
  (magit-status))

(def-with-selected-window my/magit-blame ()
  (if magit-blame-mode
      (magit-blame-quit)
    (magit-blame-addition '("-w"))))

(def-with-selected-window my/magit-log-buffer ()
  (magit-log-buffer-file))

(def-with-selected-window my/vc-mode-line-view-pr ()
  (browse-url
   (my/vc-mode-line--pull-request-url)))

(def-with-selected-window my/vc-mode-line-copy-pr ()
  (my/vc-mode-line-copy-url
   (my/vc-mode-line--pull-request-url)))

(def-with-selected-window my/vc-mode-line-view-file-on-web ()
  (browse-url
   (my/vc-mode-line--file-web-url)))

(def-with-selected-window my/vc-mode-line-copy-file-on-web ()
  (my/vc-mode-line-copy-url
   (my/vc-mode-line--file-web-url)))

(def-with-selected-window my/vc-mode-line-view-raw-file-on-web ()
  (browse-url
   (my/vc-mode-line--raw-file-web-url)))

(def-with-selected-window my/vc-mode-line-copy-raw-file-on-web ()
  (my/vc-mode-line-copy-url
   (my/vc-mode-line--raw-file-web-url)))

(def-with-selected-window my/vc-mode-line-view-file-at-current-line ()
  (browse-url
   (my/vc-mode-line--current-line-web-url)))

(def-with-selected-window my/vc-mode-line-copy-file-at-current-line ()
  (my/vc-mode-line-copy-url
   (my/vc-mode-line--current-line-web-url)))

(def-with-selected-window my/vc-mode-line-view-file-different-branch ()
  (browse-url
   (my/vc-mode-line--file-web-url
    (my/vc-mode-line--read-branch))))

(def-with-selected-window my/vc-mode-line-copy-file-different-branch ()
  (my/vc-mode-line-copy-url
   (my/vc-mode-line--file-web-url
    (my/vc-mode-line--read-branch))))

(def-with-selected-window my/vc-mode-line-view-repo-on-web ()
  (browse-url
   (my/vc-mode-line--github-repo-base)))

(def-with-selected-window my/vc-mode-line-copy-repo-on-web ()
  (my/vc-mode-line-copy-url
   (my/vc-mode-line--github-repo-base)))

(defvar my/vc-mode-line-menu-map
  (easy-menu-create-menu
   ""
   '(["Magit blame" my/magit-blame t]
     ["Magit log buffer file" my/magit-log-buffer t]
     "---"
     ["View PR" my/vc-mode-line-view-pr t]
     ["(copy)" my/vc-mode-line-copy-pr t]
     "---"
     ["View file on Web" my/vc-mode-line-view-file-on-web buffer-file-name]
     ["(copy)" my/vc-mode-line-copy-file-on-web buffer-file-name]
     "---"
     ["View Raw File on Web" my/vc-mode-line-view-raw-file-on-web buffer-file-name]
     ["(copy)" my/vc-mode-line-copy-raw-file-on-web buffer-file-name]
     "---"
     ["View File at Current Line" my/vc-mode-line-view-file-at-current-line buffer-file-name]
     ["(copy)" my/vc-mode-line-copy-file-at-current-line buffer-file-name]
     "---"
     ["View File Different Branch" my/vc-mode-line-view-file-different-branch buffer-file-name]
     ["(copy)" my/vc-mode-line-copy-file-different-branch buffer-file-name]
     "---"
     ["View repo on Web" my/vc-mode-line-view-repo-on-web t]
     ["(copy)" my/vc-mode-line-copy-repo-on-web t])))

(defvar my/vc-mode-line-keymap
  (let ((map (make-sparse-keymap)))
    (bindings--define-key map [mode-line down-mouse-1]
      `(menu-item "Menu Bar" ignore
                  :filter ,(lambda (_) my/vc-mode-line-menu-map)))
    (define-key map [mode-line mouse-1] 'ignore)
    (define-key map [mode-line S-mouse-1] 'my/vc-mode-line-copy-file-at-current-line)
    map))

;; Let's only care about Git for now.
(defun vc-mode-line (file &optional backend)
  (interactive (list buffer-file-name))
  (when (eq backend 'Git)
    (setq vc-mode
          (propertize (format "(%s)" (vc-git--symbolic-ref file))
                      'mouse-face 'mode-line-highlight
                      'help-echo "mouse-1: branch menu\nS-mouse-1: copy file URL"
                      'local-map my/vc-mode-line-keymap))
    (force-mode-line-update)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my/buffer-mods-git-blame
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'my/magit-blame)
    map))

(def-modeline-var my/buffer-mods
  `((:propertize (:eval (if buffer-file-name "ß" " "))
                 help-echo "Git blame current file"
                 mouse-face mode-line-highlight
                 local-map ,my/buffer-mods-git-blame)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put it all  together
(setq-default mode-line-format
              '(" "
                my/mode-line-modified " "
                my/mode-line-position " "
                mode-line-buffer-identification " "
                (vc-mode vc-mode) " "
                (project-mode-line project-mode-line-format)
                ;; (which-func-mode which-func-format)
                mode-line-modes))
                ;; my/buffer-mods
