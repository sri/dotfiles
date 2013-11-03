(require 'bytecomp)

(defun my-load (filename-no-ext)
  "Compiles the file, if necessary, and then loads it.
Filename-no-ext should be the full name of the file with
the extension."
  (let ((source (concat filename-no-ext ".el"))
        (compiled (concat filename-no-ext ".elc")))
    (when (file-newer-than-file-p source compiled)
      (let (byte-compile-verbose)
        ;; Binding byte-compile-verbose to nil stops
        ;; the "Compiling ..." message, but doesn't
        ;; stop the "Wrote <filename>" one.
        (byte-compile-file source)))
    (load compiled nil t t)))

(let ((my-files '("my-env" "my-fns" "my-keys" "my-dired" "my-help"
                  "my-shell" "my-sublime" "my-modeline" "my-win"
                  "my-mouse-hacks"
                  ;; Packages
                  "my-packages"
                  "my-bm" "my-magit" "my-autopair"
                  "my-yasnippet" "my-ace-jump-mode"
                  "my-expand-region"))
      (load-directory (file-name-directory load-file-name)))
  (dolist (f my-files)
    (my-load (expand-file-name f load-directory))))

;; Some third party stuff that isn't packaged via ELPA or MELPA.
;; So I maintain a copy of them. Customizations to those files will
;; be under ~/.emacs.d/my-<third-party-package-name>.el
(let* ((third-party-dir (expand-file-name "third-party" "~/.emacs.d"))
       (third-party-files (condition-case nil
                              (directory-files third-party-dir 'full)
                            (error '()))))
  (dolist (f third-party-files)
    (when (string-match "\\.el$" f)
      (my-load (file-name-sans-extension f))
      (let ((my-customization
             (expand-file-name (concat "my-" (file-name-base f))
                               (file-name-directory load-file-name))))
        (when (file-exists-p (concat my-customization ".el"))
          (my-load my-customization))))))

(let ((private (expand-file-name "~/.emacs.private")))
  (when (file-exists-p (concat private ".el"))
    (my-load private)))

(message "")
