;; -*- lexical-binding: t; -*-

(defvar ghostel--input-mode)

(defconst my/ghostel-input-modes
  '(("Semi-char" semi-char ghostel-semi-char-mode "C-c C-j"
     "Most keys go to the terminal; Emacs keeps C-c, C-x, M-x, etc.")
    ("Char"      char      ghostel-char-mode      "C-c M-d"
     "Every key goes to the terminal; use M-RET to leave this mode.")
    ("Line"      line      ghostel-line-mode      "C-c C-l"
     "Edit input with Emacs and send the complete line with RET.")
    ("Copy"      copy      ghostel-copy-mode      "C-c C-t"
     "Freeze terminal output for Emacs navigation, selection, and copying.")
    ("Emacs"     emacs     ghostel-emacs-mode     "C-c C-e"
     "Use Emacs navigation and selection while terminal output stays live."))
  "Ghostel input modes: label, mode symbol, command, key, and description.")

(defun my/ghostel--input-mode-annotation (candidate current-mode)
  "Annotate input mode CANDIDATE, marking CURRENT-MODE."
  (when-let* ((spec (assoc-string candidate my/ghostel-input-modes)))
    (let* ((label-width (apply #'max
                               (mapcar (lambda (item)
                                         (string-width (car item)))
                                       my/ghostel-input-modes)))
           (current-p (eq (nth 1 spec) current-mode))
           (padding (make-string (- label-width (string-width candidate)) ?\s)))
      (format "%s%s  %-9s  %s"
              (if current-p "*" " ")
              padding
              (nth 3 spec)
              (nth 4 spec)))))

(defun my/ghostel-select-input-mode ()
  "Display Ghostel input modes with descriptions and switch to one."
  (interactive)
  (unless (derived-mode-p 'ghostel-mode)
    (user-error "This is not a Ghostel buffer"))
  (let* ((current-mode ghostel--input-mode)
         (current-spec
          (seq-find (lambda (spec) (eq (nth 1 spec) current-mode))
                    my/ghostel-input-modes))
         (completion-extra-properties
          `(:annotation-function
            ,(lambda (candidate)
               (my/ghostel--input-mode-annotation candidate current-mode))))
         (choice
          (completing-read
           "Ghostel input mode: "
           (mapcar #'car my/ghostel-input-modes)
           nil t nil nil (car current-spec)))
         (spec (assoc-string choice my/ghostel-input-modes))
         (mode (nth 1 spec))
         (command (nth 2 spec)))
    ;; Copy and Emacs commands toggle themselves, so selecting the current
    ;; mode must be a no-op rather than unexpectedly leaving it.
    (unless (eq mode current-mode)
      (call-interactively command))))

(use-package ghostel
  :ensure t
  :bind (:map ghostel-mode-map
              ("C-c C-i" . my/ghostel-select-input-mode)))
