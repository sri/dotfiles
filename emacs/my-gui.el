;; -*- lexical-binding: t; -*-
(require 'bm)

(defvar my-theme
  (let* ((hour (nth 2 (decode-time (current-time))))
         (period (cond ((>= hour 17) 'evening)
                       ((>= hour 12) 'afternoon)
                       (t            'morning))))
    (cond (t 'solarized-zenburn)
          ((eq period 'evening)
           (set-face-attribute 'bm-persistent-face nil :extend t :background "#6e52b9")
           'ef-owl)
          (t
           (set-face-attribute 'bm-persistent-face nil :extend t :background "#60bd90")
           'modus-operandi-tinted)))
  "Can be defined in ~/.emacs.private.el.")

(defvar my-themes
  '(
    solarized-zenburn
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

(custom-theme-set-faces
 'solarized-zenburn
 '(region ((t (:background "#fbc050"))))
 '(bm-persistent-face ((t (:extend t :background "#6e52b9" :overline nil)))))

;; (add-hook 'focus-out-hook
;;           (lambda ()
;;             (when (and buffer-file-name (buffer-modified-p))
;;               (save-buffer))))

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
