;; Add a new type of register: `command'.
(defun my/jump-to-register (register &optional delete)
  (let ((val (get-register register)))
    (cond ((and (consp val) (eq (car val) 'command))
           (funcall (cdr val))
           ;; Don't proceed with the old function
           t))))

;; Before-until -- if the function returns `t', don't call the old function.
(add-function :before-until (symbol-function 'jump-to-register) #'my/jump-to-register)

(let ((registers '((?d . "~/Desktop")
                   (?e . "~/my/dotfiles/emacs/emacs.d")
                   (?s . "~/dev")
                   (?~ . "~"))))
  (dolist (reg registers)
    (set-register (car reg) (cons 'file (cdr reg)))))

;; Location of my work notes & work log files.
(defvar my/work-notes-file nil)
(defvar my/work-log-file nil)

(defun my/open-work-notes ()
  (interactive)
  (cond ((and my/work-notes-file my/work-log-file)
         (delete-other-windows)
         (find-file my/work-notes-file)
         (split-window-vertically)
         (other-window 1)
         (find-file my/work-log-file))
        ((or my/work-notes-file my/work-log-file)
         (find-file (or my/work-notes-file my/work-log-file)))))

(add-hook 'emacs-startup-hook 'my/open-work-notes)
(set-register ?t '(command . my/open-work-notes))
