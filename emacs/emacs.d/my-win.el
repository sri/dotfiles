(if window-system
    ;; Terminal
    (progn
      (load-theme 'tango-dark))

  ;; GUI
  (load-theme 'solarized-dark t)
  (custom-theme-set-faces
   'solarized-dark
   '(dired-header ((t (:foreground "#268bd2" :underline t :background nil)))))

  (add-hook 'focus-out-hook
            (lambda ()
              (when (and buffer-file-name (buffer-modified-p))
                (save-buffer))))

  (global-hl-line-mode 1)

  (let ((shell-path (shell-command-to-string "$SHELL -c 'echo -n $PATH'")))
    (setenv "PATH" shell-path)
    (setq exec-path (split-string shell-path path-separator)))

  ;; Center Emacs's position on screen
  (let* ((height 40)
         (width 80)
         (screen-height (x-display-pixel-height))
         (screen-width (x-display-pixel-width))
         (top (/ (- screen-height (frame-pixel-height)) 2))
         (left (/ (- screen-width (frame-pixel-width)) 2)))
    (add-to-list 'default-frame-alist (cons 'height 40))
    (add-to-list 'default-frame-alist (cons 'width 80))
    (add-to-list 'default-frame-alist (cons 'top top))
    (add-to-list 'default-frame-alist (cons 'left left))
    (add-to-list 'default-frame-alist '(font . "Monaco 18")))

  (set-frame-parameter nil 'alpha '(100 100))

  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

  )
