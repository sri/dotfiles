;; -*- lexical-binding: t; -*-
(let ((registers `((?d . "~/Desktop")
                   (?D . "~/Downloads")
                   (?e . "~/my/dotfiles/emacs")
                   (?s . "~/dev/src")
                   (?S . ,my/scratch-directory)
                   (?n . "~/my/notes")
                   (?y . "~/my/dotfiles/emacs/snippets/fundamental-mode")
                   (?p . "~/.emacs.private.el")
                   (?~ . "~"))))
  (--each registers
    (set-register (car it) (cons 'file (cdr it)))))

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
