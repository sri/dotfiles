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

(def-modeline-var my/mode-line-modified
  `(:propertize (:eval (if (buffer-modified-p) "*" " "))
                help-echo "mouse-1: Save buffer\nS-mouse-1: show unsaved changes"
                mouse-face mode-line-highlight
                local-map ,my/mode-line-buffer-modified-p-keymap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer name:
(require 'easymenu)

(defvar my/mode-line-buffer-name-menu-map
  (easy-menu-create-menu
   "Path actions"
   '(["Copy absolute path" my/mode-line-copy-full-path t]
     ["Copy filename" my/mode-line-copy-file-name t]
     ["Copy filename in repo w/ line num" my/mode-line-copy-file-name-in-repo t]
     "---"
     ["Open in Finder" my/mode-line-open-folder]
     ["Open in Sublime" my/mode-line-open-in-sublime]
     ["Copy region for JIRA" my/mode-line-copy-for-jira])))

(def-with-selected-window my/mode-line-copy-for-jira ()
  (let ((path (my/mode-line-get-file-name-in-repo))
        (region (and (use-region-p)
                     (format "{noformat}\n%s\n{noformat}\n"
                             (buffer-substring-no-properties
                              (region-beginning)
                              (region-end))))))
    (kill-new (if region
                  (format "*%s*:\n%s" path region)
                path))
    (message "Copied `%s' with the region for JIRA" path)))


;; Buffer name: click to copy
(make-face 'my/mode-line-buffer-name-face)
(set-face-attribute 'my/mode-line-buffer-name-face nil
    :inherit 'mode-line-faced
    ;:foreground "#4271ae"
    ;;:height 90
    ;:box '(:line-width 2 :color "#4271ae")
    )

(defun my/mode-line-buffer-identification-help-echo (window object point)
  "Click to open Path actions menu")

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

(defun my/mode-line-get-file-name-in-repo ()
  (let* ((path buffer-file-name)
         (git-root (shell-command-to-string "git rev-parse --show-toplevel 2> /dev/null")))
    (setq git-root (s-trim git-root))
    (if (or (string= git-root "")
            (not (string-prefix-p git-root path)))
        (message "Not in git repo")
      (setq path
            (format "%s%s#%d"
                    (file-name-nondirectory git-root)
                    (s-chop-prefix git-root path)
                    (line-number-at-pos))))))

(def-with-selected-window my/mode-line-copy-file-name-in-repo ()
  (let ((path (my/mode-line-get-file-name-in-repo)))
    (kill-new path)
    (message "Copied: `%s'" path)))

(def-with-selected-window my/mode-line-open-folder ()
  (let* ((name (buffer-file-name))
         (dir (if name (file-name-directory name) default-directory)))
    (call-process "open" nil nil nil dir)))

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
  (let ((n (read-number "Goto line: ")))
    (goto-line n)))

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
    (magit-blame)))

(def-with-selected-window my/magit-log-buffer ()
  (magit-log-buffer-file))

(defvar my/vc-mode-line-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'my/magit-status)
    (define-key map [mode-line S-mouse-1] 'my/magit-blame)
    (define-key map [mode-line M-mouse-1] 'my/magit-log-buffer)
    map))

;; Let's only care about Git for now.
(defun vc-mode-line (file &optional backend)
  (interactive (list buffer-file-name))
  (when (eq backend 'Git)
    (setq vc-mode
          (propertize (format "(%s)" (vc-git--symbolic-ref file))
                      'mouse-face 'mode-line-highlight
                      'help-echo "Click for Magit status\nS-mouse-1: Magit blame\nAlt-mouse-1: Magit log buffer"
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
                mode-line-modes
                ;; my/buffer-mods
                (defining-kbd-macro " def")))
