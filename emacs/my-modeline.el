;; -*- lexical-binding: t; -*-
(require 's)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
(defmacro def-with-selected-window (name &rest body)
  `(defun ,name (event)
     (interactive "e")
     (with-selected-window (posn-window (event-start event))
       ,@body)))

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
  (let* ((filename (my/mode-line-get-file-name-in-repo t t))
         (url (magit-get "remote" "origin" "url")))
    (unless (string-match "^http" url)
      (setq url
            (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                                      "https://\\2/\\3"
                                      url)))
    (cond ((or (string-match "github" url)
               (string-match "gitlab" url))
           (setq url (format "%s/blob/%s/%s" url
                             (magit-get-current-branch)
                             filename))))
    url))

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

(defvar my/vc-mode-line-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'my/magit-blame)
    (define-key map [mode-line S-mouse-1] 'my/magit-log-buffer)
    map))

;; Let's only care about Git for now.
(defun vc-mode-line (file &optional backend)
  (interactive (list buffer-file-name))
  (when (eq backend 'Git)
    (setq vc-mode
          (propertize (format "(%s)" (vc-git--symbolic-ref file))
                      'mouse-face 'mode-line-highlight
                      'help-echo "Click for Magit blame\nS-mouse-1: Magit log buffer"
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
