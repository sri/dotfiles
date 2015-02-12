;; Emulate Sublime Text's New File creation:
;;  - quickly create a buffer backed by the file system
;;  - auto saves before Emacs exits
;; Also (NOT YET DONE):
;;  - quickly scan previous list of buffers
;;  - when Emacs starts up, load only these sticky buffers
;;    created less than a week ago.

(defvar my-sticky-buf-directory
  (expand-file-name "~/Desktop/emacs-saves/buffers"))

;; Timestamp is of the format: yyyymmddhhMMss
(defun my-sticky-buf-humanize-timestamp (timestamp)
  (let* ((year (substring timestamp 0 4))
         (month (substring timestamp 4 6))
         (day (substring timestamp 6 8))
         (hour (substring timestamp 8 10))
         (min (substring timestamp 10 12))
         (sec (substring timestamp 12 14))
         (encoded-time
          (apply 'encode-time
                 (mapcar 'string-to-number
                         (list sec min hour day month year)))))
    (format-time-string "%l:%M%p %a %m/%d/%y" encoded-time)))

(defun my-sticky-buf-humanize-buffer-name ()
  (let ((name (split-string (buffer-name) "[-]")))
    (rename-buffer (generate-new-buffer-name
                    (my-sticky-buf-humanize-timestamp (car name))))))

(defun my-sticky-buf-new ()
  (interactive)
  (unless (file-exists-p my-sticky-buf-directory)
    (make-directory my-sticky-buf-directory t))
  (let (timestamp name)
    (while (or (null name) (file-exists-p name))
      (setq timestamp (format-time-string "%Y%m%d%H%M%S"))
      (setq name (concat my-sticky-buf-directory
                         "/" timestamp "-notes.txt")))
    (write-region 1 1 name nil 'no-message nil 'excl)
    (find-file name)
    (my-sticky-buf-humanize-buffer-name)
    (setq auto-save-visited-file-name t)
    (setq buffer-save-without-query t)))
