(defun my-mode-line-copy-full-path ()
  "Copies the buffer name to the kill ring.
If that is nil, then it is mode specific as to what gets copied:
 - shell or magit modes, copies the default directory."
  (interactive)
  (let ((name (or (buffer-file-name)
                  (cond ((eq major-mode 'shell-mode)
                         default-directory)
                        ((string-prefix-p "Magit" mode-name)
                         default-directory)))))
    (when name
      (kill-new name)
      (message "Copied `%s'" name))))

(defun my-mode-line-buffer-identification-help-echo (window object point)
  (let ((buffer (window-buffer window)))
    (with-current-buffer buffer
      (format "%s\n%s\nmouse-1: Copy file path to kill ring"
              (buffer-name buffer)
              (buffer-file-name buffer)))))

(make-face 'my-mode-line-buffer-name-face)
(set-face-attribute 'my-mode-line-buffer-name-face nil
    :inherit 'mode-line-faced
    ;:foreground "#4271ae"
    ;;:height 90
    ;:box '(:line-width 2 :color "#4271ae")
    )

(defun my-mode-line-open-folder ()
  (interactive)
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
    ;; (define-key map [header-line mouse-3] 'ignore)
    map))

(setq-default mode-line-buffer-identification
              `(:propertize "%12b"
                face my-mode-line-buffer-name-face
                help-echo my-mode-line-buffer-identification-help-echo
                mouse-face mode-line-highlight
                local-map ,my-mode-line-buffer-identification-keymap))

(require 'powerline)

;; Copied from powerline.el
(defun powerline-buffer-id (&optional face pad)
  (powerline-raw
   (format-mode-line
    (concat " " (propertize
		 (format-mode-line mode-line-buffer-identification)
		 'face face
		 'mouse-face 'mode-line-highlight
		 'help-echo 'my-mode-line-buffer-identification-help-echo
		 'local-map my-mode-line-buffer-identification-keymap)))
   face pad))

(setq powerline-default-separator nil)
(powerline-center-theme)


;; (defmacro def-with-selected-window (name &rest body)
;;   `(defun ,name (event)
;;      (interactive "e")
;;      (with-selected-window (posn-window (event-start event))
;;        ,@body)))

;; (def-with-selected-window my-header-line-shell-erase-buffer-and-run-prev-cmd
;;   (my-shell-erase-buffer)
;;   (comint-previous-input 1)
;;   (comint-send-input))

;; (def-with-selected-window my-header-line-shell-erase-buffer
;;   (my-shell-erase-buffer))


;; (def-with-selected-window my-mode-line-kill-file-full-path
;;   (let ((full (buffer-file-name)))
;;     (when full
;;       (kill-new full)
;;       (message "Copied: `%s'" full))))



;; (defun my-mode-line-buffer-name ()
;;   (let* ((boxsize 25)
;;          (bn (buffer-name))
;;          (len (length bn)))
;;     (if (> len boxsize)
;;         (concat (substring bn 0 (- boxsize 3)) "...")
;;       (concat bn (make-string (- boxsize len) ? )))))

;; (defvar my-mode-line-buffer-modified-p-keymap
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [mode-line mouse-1] 'save-buffer)
;;     (define-key map [mode-line down-mouse-1] 'ignore)
;;     map))

;; (setq-default mode-line-modified
;;               `(:propertize (:eval (if (buffer-modified-p) "*" " "))
;;                             help-echo "mouse-1: Save buffer"
;;                             mouse-face mode-line-highlight
;;                             local-map ,my-mode-line-buffer-modified-p-keymap))

;; (def-with-selected-window my-mode-line-beginning-or-end-of-buffer
;;   (if (bobp)
;;       (goto-char (point-max))
;;     (goto-char (point-min))))

;; (def-with-selected-window my-mode-line-scroll-up
;;   (scroll-up))

;; (def-with-selected-window my-mode-line-scroll-down
;;   (scroll-down))

;; (setq my-mode-line-buffer-percentage-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [mode-line mouse-1] 'my-mode-line-scroll-up)
;;     (define-key map [mode-line S-mouse-1] 'my-mode-line-scroll-down)
;;     (define-key map [mode-line C-mouse-1] 'my-mode-line-beginning-or-end-of-buffer)
;;     (define-key map [mode-line down-mouse-1] 'ignore)
;;     map))

;; (def-with-selected-window my-mode-line-goto-line
;;   (let ((n (read-number "Goto line: ")))
;;     (goto-line n)))

;; (setq my-mode-line-column-line-number-mode-map
;;       (let ((map (make-sparse-keymap)))
;;         (define-key map [mode-line mouse-1] 'my-mode-line-goto-line)
;;         (define-key map [mode-line down-mouse-1] 'ignore)
;;         map))

;; (setq-default mode-line-position
;;   `((-3 ,(propertize
;;           "%p"
;;           'local-map my-mode-line-buffer-percentage-mode-map
;;           'mouse-face 'mode-line-highlight
;;           'help-echo "mouse-1: scroll up\nShift mouse-1: scroll down
;; Ctrl mouse-1: toggle between Beginning & End of buffer"))
;;     (line-number-mode
;;      ((column-number-mode
;;        (10 ,(propertize
;; 	     " (L%l,C%c)"
;; 	     'local-map my-mode-line-column-line-number-mode-map
;; 	     'mouse-face 'mode-line-highlight
;; 	     'help-echo "mouse-1: goto line"))
;;        (6 ,(propertize
;; 	    " L%l"
;; 	    'local-map my-mode-line-column-line-number-mode-map
;; 	    'mouse-face 'mode-line-highlight
;; 	    'help-echo "mouse-1: goto line"))))
;;      ((column-number-mode
;;        (5 ,(propertize
;; 	    " C%c"
;; 	    'local-map my-mode-line-column-line-number-mode-map
;; 	    'mouse-face 'mode-line-highlight
;; 	    'help-echo "mouse-1: goto line")))))))

;; (make-variable-buffer-local 'mode-line-position)



;; (def-with-selected-window my-mode-line-window-split-right
;;   (split-window-right))

;; (def-with-selected-window my-mode-line-window-split-below
;;   (split-window-below))

;; (def-with-selected-window my-mode-line-window-delete
;;   (delete-window))

;; (def-with-selected-window my-mode-line-window-delete-other-windows
;;   (delete-other-windows))

;; (defun my-make-mode-line-mouse-map (&rest args)
;;   (let ((map (make-sparse-keymap)))
;;     (while args
;;       (define-key map (vector 'mode-line (pop args)) (pop args)))
;;     map))

;; (defun my-make-header-line-mouse-map (&rest args)
;;   (let ((map (make-sparse-keymap)))
;;     (while args
;;       (define-key map (vector 'header-line (pop args)) (pop args)))
;;     map))

;; (def-with-selected-window my-mode-line-goto-dired-fn
;;   (my-dired))

;; (setq-default my-mode-line-goto-dired
;;   (list (propertize "(.)"
;;                     'mouse-face 'mode-line-highlight
;;                     'help-echo "dired current directory"
;;                     'local-map (my-make-mode-line-mouse-map
;;                                 'down-mouse-1 #'ignore
;;                                 'mouse-1 #'my-mode-line-goto-dired-fn))))

;; (setq-default my-mode-line-window-manipulation
;;   (list "("
;;         (propertize "⇨"
;;                     'mouse-face 'mode-line-highlight
;;                     'help-echo "split window right"
;;                     'local-map (my-make-mode-line-mouse-map
;;                                 'down-mouse-1 #'ignore
;;                                 'mouse-1 #'my-mode-line-window-split-right))
;;         (propertize "⇩"
;;                     'mouse-face 'mode-line-highlight
;;                     'help-echo "split window below"
;;                     'local-map (my-make-mode-line-mouse-map
;;                                 'down-mouse-1 #'ignore
;;                                 'mouse-1 #'my-mode-line-window-split-below))
;;         (propertize "X"
;;                     'mouse-face 'mode-line-highlight
;;                     'help-echo "delete window"
;;                    'local-map (my-make-mode-line-mouse-map
;;                                 'down-mouse-1 #'ignore
;;                                 'mouse-1 #'my-mode-line-window-delete))
;;         (propertize "1"
;;                     'mouse-face 'mode-line-highlight
;;                     'help-echo "delete other windows"
;;                     'local-map (my-make-mode-line-mouse-map
;;                                 'down-mouse-1 #'ignore
;;                                 'mouse-1 #'my-mode-line-window-delete-other-windows))
;;          ")"))

;; (make-variable-buffer-local 'my-mode-line-window-manipulation)
;; (put 'my-mode-line-window-manipulation 'risky-local-variable t)

;; (make-variable-buffer-local 'my-mode-line-goto-dired)
;; (put 'my-mode-line-goto-dired 'risky-local-variable t)

;; (defvar my-original-mode-line-format mode-line-format)

;; (defun my-mode-line-toggle ()
;;   (interactive)
;;   (setq-default mode-line-format
;;                 (if (eq mode-line-format my-original-mode-line-format)
;;                     my-mode-line-format
;;                   my-original-mode-line-format)))

;; (progn
;;   (defvar my-mode-line-format)
;;   (setq my-mode-line-format
;;         '(" "
;;           mode-line-modified " "
;;           my-mode-line-window-manipulation " "
;;           my-mode-line-goto-dired " "
;;           mode-line-buffer-identification " "
;;           mode-line-modes " "
;;           mode-line-position
;;           (defining-kbd-macro " Def")
;;           (vc-mode vc-mode)
;;           ))
;;   (setq-default mode-line-format my-mode-line-format)
;;   )
