;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
(defmacro def-with-selected-window (name &rest body)
  `(defun ,name (event)
     (interactive "e")
     (with-selected-window (posn-window (event-start event))
       ,@body)))

(defun my-make-mode-line-mouse-map (&rest args)
  (let ((map (make-sparse-keymap)))
    (while args
      (define-key map (vector 'mode-line (pop args)) (pop args)))
    map))

(defmacro def-modeline-var (name &rest body)
  `(progn
     (defvar ,name)
     (setq-default ,name (progn ,@body))
     (make-variable-buffer-local ',name)
     (put ',name 'risky-local-variable t)
     ',name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer modified
(defvar my-mode-line-buffer-modified-p-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'save-buffer)
    (define-key map [mode-line down-mouse-1] 'ignore)
    (define-key map [mode-line S-mouse-1] 'my-unsaved-changes)
    map))

(def-modeline-var my-mode-line-modified
  `(:propertize (:eval (if (buffer-modified-p) "*" " "))
                help-echo "mouse-1: Save buffer\nS-mouse-1: show unsaved changes"
                mouse-face mode-line-highlight
                local-map ,my-mode-line-buffer-modified-p-keymap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer name: click to copy
(make-face 'my-mode-line-buffer-name-face)
(set-face-attribute 'my-mode-line-buffer-name-face nil
    :inherit 'mode-line-faced
    ;:foreground "#4271ae"
    ;;:height 90
    ;:box '(:line-width 2 :color "#4271ae")
    )

(defun my-mode-line-buffer-identification-help-echo (window object point)
  "mouse-1: Copy file path to kill ring\nS-mouse-1: Open directory")

(def-with-selected-window my-mode-line-copy-full-path ()
  (let ((full (buffer-file-name)))
    (when full
      (kill-new full)
      (message "Copied: `%s'" full))))

(def-with-selected-window my-mode-line-open-folder ()
  (let* ((name (buffer-file-name))
         (dir (if name (file-name-directory name) default-directory)))
    (call-process "open" nil nil nil dir)))

(defvar my-mode-line-buffer-identification-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'my-mode-line-copy-full-path)
    (define-key map [header-line down-mouse-1] 'ignore)
    ;; (define-key map [header-line mouse-1] 'my-mode-line-copy-full-path)
    ;; (define-key map [mode-line mouse-3] 'ignore)
    (define-key map [mode-line S-mouse-1] 'my-mode-line-open-folder)
    ;; (define-key map [header-line down-mouse-3] 'ignore)
    ;; (define-key map [header-line mouse-3] 'ignore))
    map))

(setq-default mode-line-buffer-identification
              `(:propertize "%12b"
                            face my-mode-line-buffer-name-face
                            help-echo my-mode-line-buffer-identification-help-echo
                            mouse-face mode-line-highlight
                            local-map ,my-mode-line-buffer-identification-keymap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def-with-selected-window my-mode-line-goto-line
  (let ((n (read-number "Goto line: ")))
    (goto-line n)))

(defvar my-mode-line-column-line-number-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'my-mode-line-goto-line)
    (define-key map [mode-line down-mouse-1] 'ignore)
    map))

(def-modeline-var my-mode-line-position
  `((line-number-mode
     ((column-number-mode
       (10 ,(propertize
	     " %l,%C"
	     'local-map my-mode-line-column-line-number-mode-map
	     'mouse-face 'mode-line-highlight
	     'help-echo "mouse-1: goto line"))
       (6 ,(propertize
	    " L%l"
	    'local-map my-mode-line-column-line-number-mode-map
	    'mouse-face 'mode-line-highlight
	    'help-echo "mouse-1: goto line"))))
     ((column-number-mode
       (5 ,(propertize
	    " C%C"
	    'local-map my-mode-line-column-line-number-mode-map
	    'mouse-face 'mode-line-highlight
	    'help-echo "mouse-1: goto line")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put it all  together
(setq-default mode-line-format
              '(" "
                my-mode-line-modified " "
                my-mode-line-position " "
                mode-line-buffer-identification " "
                mode-line-modes " "
                (defining-kbd-macro " Def")
                (vc-mode vc-mode)
                ))
