(defvar my-theme 'zenburn ;leuven
  "Can be defined in ~/.emacs.private.el.")

(defvar my-themes
  '(
    doom-zenburn

    ef-melissa-dark
    ef-cyprus
    gruvbox-dark-medium
    kaolin-dark

    leuven

    doom-tokyo-night
    solarized-dark
    jetbrains-darcula
    modus-vivendi
    modus-operandi
    zenburn
    solarized-light
    spacemacs-dark
    ))

(defun my-try-theme (theme)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme :no-confirm)
  (message "trying theme: %s" theme))

(let ((theme
       (or my-theme
           (nth (random (length my-themes)) my-themes))))
  (when theme
    (message "using theme %s" theme)
    (load-theme theme t)))

;; (add-hook 'focus-out-hook
;;           (lambda ()
;;             (when (and buffer-file-name (buffer-modified-p))
;;               (save-buffer))))

;; (let ((shell-path (shell-command-to-string "$SHELL -l -c 'echo -n $PATH'")))
  ;; (when (> (count "\n" shell-path) 0)
    ;; (warn "my-gui.el: shell path contains newlines:\n%s" shell-path))
  ;; (setenv "PATH" shell-path)
  ;; (setq exec-path (split-string shell-path path-separator)))


;; Center Emacs's position on screen
(let* ((height 40)
       (width 82)
       (top 0)
       (left (/ (- (display-pixel-width)
                   (frame-pixel-width))
                2)))

  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist (cons 'height height))
  (add-to-list 'default-frame-alist (cons 'width width))
  (add-to-list 'default-frame-alist (cons 'top top))
  (add-to-list 'default-frame-alist (cons 'left left)))

(set-frame-parameter nil 'alpha '(100 100))

(setq frame-title-format nil)
(setq ns-use-proxy-icon nil)
