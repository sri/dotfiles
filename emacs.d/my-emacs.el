(require 'bytecomp)

(defun my-load (name)
  (let (source compiled)
    (unless (file-name-absolute-p name)
      ;; expand-file-name binds with default-directory
      (setq name (expand-file-name name)))
    (cond ((string-match "\\.el$" name)
           (setq source name)
           (setq compiled (concat name "c")))
          (t
           (setq source (concat name ".el"))
           (setq compiled (concat name ".elc"))))
    (when (file-newer-than-file-p source compiled)
      (let (byte-compile-verbose)
        ;; Binding byte-compile-verbose to nil stops
        ;; the "Compiling ..." message, but doesn't
        ;; stop the "Wrote <filename>" one.
        (byte-compile-file source)))
    (load compiled nil t t)))

(defun my-load-customization (package-name)
  (let ((my-customization (format "my-%s.el" package-name)))
    (when (file-exists-p my-customization)
      (my-load my-customization))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-files
  '("my-env" "my-fns" "my-keys" "my-dired" "my-help"
    "my-shell" "my-sublime" "my-modeline" "my-win"
    "my-desktop"
    "my-mouse-hacks" "my-packages"))

(defvar my-packages
  '(color-theme color-theme-solarized magit bm autopair go-mode org
                macrostep yasnippet ace-jump-mode expand-region
                dired-details))

(defvar my-private-dot-emacs
  (expand-file-name "~/.emacs.private.el"))

(defvar my-third-party-non-installable-files
  (directory-files "~/.emacs.d/third-party" 'full "\\.el$" t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((default-directory "~/.emacs.d"))

  (mapc 'my-load my-files)
  (mapc 'my-load-customization my-packages)

  (mapc 'my-load my-third-party-non-installable-files)
  (mapc 'my-load-customization my-third-party-non-installable-files))

(when (file-exists-p my-private-dot-emacs)
  (my-load my-private-dot-emacs))

(defun display-startup-echo-area-message ()
  (let ((elapsed (float-time (time-subtract (current-time)
                                            my-emacs-start-time))))
    (message "Finished loading in %.3fs " elapsed)))

;; Local Variables:
;; eval: (add-hook 'after-save-hook (lambda () (byte-compile-file "my-emacs.el")) nil t)
;; End:
