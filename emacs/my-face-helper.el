;; -*- lexical-binding: t; -*-
;;; Face helper - from ChatGPT 5.1 prompt
;;; Change the selected face to match another face.


(defvar-local my/copy-face-show-backgrounds nil
  "When non-nil, show only faces that specify a background.")

(defun my/copy-face-from-list ()
  "Pick a face, then show a clickable list of faces. Includes a toggle
button for filtering faces that specify a background."
  (interactive)
  ;; Step 1: select target face
  (let* ((face (read-face-name
                "Customize face"
                (or (face-at-point t t) "all faces") t)))
    (when (member face '(nil ""))
      (setq face (face-list)))
    (when (and (listp face) (null (cdr face)))
      (setq face (car face)))
    (when (listp face)
      (user-error "Please pick a single face."))

    (let ((target-face face))
      (my/render-copy-face-buffer target-face))))


;; --- Renderer ---
(defun my/render-copy-face-buffer (target-face)
  "Render the *Copy Face From* buffer for TARGET-FACE."
  (let ((buf (get-buffer-create "*Copy Face From*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)

      ;; Keep toggle local
      (unless (local-variable-p 'my/copy-face-show-backgrounds)
        (setq-local my/copy-face-show-backgrounds nil))

      ;; Toggle button
      (insert-text-button
       (format "With backgrounds: %s"
               (if my/copy-face-show-backgrounds "ON" "OFF"))
       'follow-link t
       'help-echo "Toggle showing only faces with a background"
       'action
       (lambda (_)
         (setq my/copy-face-show-backgrounds
               (not my/copy-face-show-backgrounds))
         (my/render-copy-face-buffer target-face)))
      (insert "\n\n")

      (insert (format "Click a face to copy its attributes INTO `%s'\n\n"
                      target-face))

      ;; Step 2: list faces (filtered if toggle enabled)
      (dolist (f (sort (face-list) #'string<))
        (when (if (null my/copy-face-show-backgrounds)
                  t
                (let ((bg (face-attribute f :background nil 'default)))
                  (and bg
                       (not (eq bg 'unspecified))
                       (not (string= bg (face-background 'default))))))
          (let ((face-name f))
            (insert-text-button
             (format "%s" face-name)
             'face face-name
             'follow-link t
             'help-echo (format "Copy `%s' into `%s` and save"
                                face-name target-face)
             'action
             (lambda (_btn)
               (my/copy-and-save-face face-name target-face))))
          (insert "\n")))

      (goto-char (point-min))
      (setq buffer-read-only t)
      (pop-to-buffer buf))))

;; --- Attribute copy helper ---
(defun my/copy-face-attributes (from-face to-face)
  "Copy all attributes FROM-FACE to TO-FACE."
  (dolist (attr '(:family :foundry :weight :slant :width
                  :height :underline :overline :strike-through
                  :box :inverse-video :foreground :background
                  :stipple :inherit :extend))
    (let ((val (face-attribute from-face attr nil 'default)))
      (when (not (eq val 'unspecified))
        (set-face-attribute to-face nil attr val)))))

;; --- Save helper ---
(defun my/save-face-to-custom (face)
  "Save FACE to `custom-file` exactly as Customize does."
  (let* ((spec (custom-face-attributes-get face nil))
         (entry `(,face ((t ,spec)))))
    ;; Update the saved face list
    (customize-save-customized
     (list (cons 'custom-set-faces (list entry))))
    (message "Saved %s to custom-file." face)))



;; --- Combined action ---
(defun my/copy-and-save-face (from-face to-face)
  "Copy FROM-FACE attributes into TO-FACE and save TO-FACE."
  (my/copy-face-attributes from-face to-face)
  (custom-save-all)
  '(my/save-face-to-custom to-face)
  (message "Copied `%s' → `%s' and saved to custom-file"
           from-face to-face))
