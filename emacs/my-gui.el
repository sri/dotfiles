;; -*- lexical-binding: t; -*-
(require 'bm)

(defvar my-themes
  '(
    solarized-gruvbox
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


(defvar my-theme
  (let* ((hour (nth 2 (decode-time (current-time))))
         (period (cond ((or (>= hour 17) (<= hour 4)) 'evening)
                       ((>= hour 12) 'afternoon)
                       (t            'morning))))
    (cond ((eq period 'evening)
           'solarized-gruvbox-dark)
          (t
           'leuven
           'acme)))
  "Can be defined in ~/.emacs.private.el.")

(setq my-theme 'solarized-dark)

(defun my-try-theme (theme)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme :no-confirm)
  (message "trying theme: %s" theme))

(let ((theme
       (or my-theme
           (nth (random (length my-themes)) my-themes))))
  (when theme
    (message "using theme %s" theme)
    (load-theme theme t))

  (cond ((eq theme 'solarized-zenburn)
         (custom-set-faces
          '(region ((t (:background "#8E8E93" :foreground "black"))))
          '(bm-persistent-face ((t (:extend t :background "#6e52b9" :overline nil))))))
        ((eq theme 'solarized-dark)
         (custom-set-faces
          '(region ((t (:background "#735c00" :foreground "#002b36"))))
          '(bm-persistent-face ((t (:extend t :background "#6e52b9" :overline nil))))))))

(set-face-attribute 'bm-persistent-face nil :extend t
                    :background "#6e52b9")

;; Mac selector colors in hex.
;; Purple          : #AF52DE
;; Pink            : #FF2D55
;; Red             : #FF3B30
;; Orange          : #FF9500
;; Yellow          : #FFCC00
;; Green           : #34C759
;; Graphite (Gray) : #8E8E93
;; Blue            : #B0D0FF

(defun my/customize-theme (theme &rest args)
  (add-hook 'after-load-theme-hook
            (lambda ()
              (message "*** after-load-theme-hook called with theme %s" theme)
              (when (member theme custom-enabled-themes)
                (apply #'custom-theme-set-faces theme args)))))





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
