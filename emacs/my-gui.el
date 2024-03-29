(defvar my-themes
  '(
    gruvbox-dark-medium
    doom-zenburn
    doom-tokyo-night
    leuven
    solarized-dark
    jetbrains-darcula
    modus-vivendi
    kaolin-dark
    modus-operandi
    zenburn
    solarized-light
    spacemacs-dark
    ))

(defun my-try-theme (theme)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme :no-confirm)
  (message "trying theme: %s" theme))

;; (my-try-theme 'zenburn)

(let ((theme (car my-themes)))
  (when theme
    (message "using theme %s" theme)
    (when (eq theme 'modus-vivendi)
      (require 'modus-vivendi-theme)
      (setq modus-vivendi-theme-override-colors-alist
            '(("fg-main" . "#dddddd")
              ("bg-main" . "#111111"))))

    (load-theme theme t)
    '(custom-theme-set-faces
     theme
     '(dired-header ((t (:foreground "#268bd2"
                                     :underline t
                                     :background nil)))))))

;; (add-hook 'focus-out-hook
;;           (lambda ()
;;             (when (and buffer-file-name (buffer-modified-p))
;;               (save-buffer))))

;; (let ((shell-path (shell-command-to-string "$SHELL -l -c 'echo -n $PATH'")))
  ;; (when (> (count "\n" shell-path) 0)
    ;; (warn "my-gui.el: shell path contains newlines:\n%s" shell-path))
  ;; (setenv "PATH" shell-path)
  ;; (setq exec-path (split-string shell-path path-separator)))



(require 'exec-path-from-shell)
(add-to-list 'exec-path-from-shell-variables "NPM_TOKEN")
(exec-path-from-shell-initialize)

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
