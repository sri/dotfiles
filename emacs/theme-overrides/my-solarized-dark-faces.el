;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'color)

(defconst my/org-complementary-themes
  '(solarized-gruvbox
    solarized-zenburn
    doom-zenburn
    ef-melissa-dark
    ef-cyprus
    gruvbox-dark-medium
    kaolin-dark
    doom-tokyo-night
    solarized-dark
    jetbrains-darcula
    modus-vivendi
    modus-operandi
    zenburn
    solarized-light
    spacemacs-dark)
  "Themes where Org faces are rebuilt from complementary colors.")

(defvar my/org-todo-keyword-faces-default
  (if (boundp 'org-todo-keyword-faces) org-todo-keyword-faces nil)
  "Default `org-todo-keyword-faces` to restore outside target themes.")

(defun my/normalize-color (value fallback)
  "Return VALUE as a #RRGGBB string or FALLBACK."
  (let* ((raw (cond
               ((stringp value) value)
               ((symbolp value) (symbol-name value))
               (t nil)))
         (rgb (and raw
                   (not (member raw '("unspecified" "nil")))
                   (ignore-errors (color-name-to-rgb raw)))))
    (if rgb
        (apply #'color-rgb-to-hex (append rgb '(2)))
      fallback)))

(defun my/face-color (face attr fallback)
  "Read FACE ATTR and normalize it, falling back to FALLBACK."
  (my/normalize-color (face-attribute face attr nil t) fallback))

(defun my/color-mix (fg bg alpha &optional fallback)
  "Blend FG into BG with ALPHA.
ALPHA=0 means BG, ALPHA=1 means FG."
  (let ((rgb-fg (ignore-errors (color-name-to-rgb fg)))
        (rgb-bg (ignore-errors (color-name-to-rgb bg))))
    (if (and rgb-fg rgb-bg)
        (apply #'color-rgb-to-hex
               (append
                (cl-mapcar (lambda (x y)
                             (+ (* alpha x) (* (- 1.0 alpha) y)))
                           rgb-fg rgb-bg)
                '(2)))
      (or fallback fg bg "#888888"))))

(defun my/color-complementary (c fallback)
  "Return complement of color C or FALLBACK."
  (my/normalize-color (ignore-errors (color-complement c)) fallback))

(defun my/palette-get (palette key fallback)
  (or (cdr (assq key palette)) fallback))

(defun my/build-org-complementary-palette ()
  "Build a palette from the current theme using complementary colors."
  (let* ((bg (my/face-color 'default :background "#1e1e1e"))
         (fg (my/face-color 'default :foreground "#d0d0d0"))
         (lvl1-src (my/face-color 'org-level-1 :foreground
                                  (my/face-color 'font-lock-keyword-face :foreground fg)))
         (lvl2-src (my/face-color 'org-level-2 :foreground
                                  (my/face-color 'font-lock-function-name-face :foreground fg)))
         (lvl3-src (my/face-color 'org-level-3 :foreground
                                  (my/face-color 'font-lock-type-face :foreground fg)))
         (kw-src (my/face-color 'font-lock-constant-face :foreground fg))
         (warn-src (my/face-color 'warning :foreground lvl1-src))
         (ok-src (my/face-color 'success :foreground lvl2-src))
         (err-src (my/face-color 'error :foreground lvl3-src))

         (accent-1 (my/color-complementary lvl1-src "#d7875f"))
         (accent-2 (my/color-complementary lvl2-src "#7fa2ff"))
         (accent-3 (my/color-complementary lvl3-src "#b48ead"))
         (accent-4 (my/color-complementary kw-src "#c0c060"))
         (todo (my/color-complementary warn-src accent-1))
         (done (my/color-complementary ok-src "#7fd7af"))
         (cancel (my/color-complementary err-src "#ff8080"))
         (wait (my/color-complementary (my/color-mix warn-src err-src 0.5 warn-src)
                                       "#d08a5f"))

         (level-1-bg (my/color-mix accent-1 bg 0.10 bg))
         (panel-bg (my/color-mix accent-2 bg 0.09 bg))
         (panel-line-bg (my/color-mix accent-3 bg 0.13 bg))
         (todo-bg (my/color-mix todo bg 0.16 bg))
         (done-bg (my/color-mix done bg 0.14 bg))
         (table-fg (my/color-mix fg accent-2 0.20 fg))
         (neutral (my/color-mix fg bg 0.72 fg)))
    `((bg . ,bg)
      (fg . ,fg)
      (accent-1 . ,accent-1)
      (accent-2 . ,accent-2)
      (accent-3 . ,accent-3)
      (accent-4 . ,accent-4)
      (todo . ,todo)
      (done . ,done)
      (cancel . ,cancel)
      (wait . ,wait)
      (level-1-bg . ,level-1-bg)
      (panel-bg . ,panel-bg)
      (panel-line-bg . ,panel-line-bg)
      (todo-bg . ,todo-bg)
      (done-bg . ,done-bg)
      (table-fg . ,table-fg)
      (neutral . ,neutral))))

(defun my/build-org-todo-keyword-faces (palette)
  "Build `org-todo-keyword-faces` from PALETTE."
  (let* ((todo (my/palette-get palette 'todo "#d7875f"))
         (idea (my/palette-get palette 'accent-4 "#c0c060"))
         (now (my/palette-get palette 'accent-2 "#7fa2ff"))
         (next (my/palette-get palette 'accent-3 "#b48ead"))
         (wait (my/palette-get palette 'wait "#d08a5f"))
         (done (my/palette-get palette 'done "#7fd7af"))
         (cancel (my/palette-get palette 'cancel "#ff8080")))
    `(("TODO"      . (:foreground ,todo :weight normal
                        :box (:line-width 1 :color ,todo :style released-button)))
      ("IDEA"      . (:foreground ,idea :slant italic :weight normal
                        :box (:line-width 1 :color ,idea :style released-button)))
      ("NOW"       . (:foreground ,now :weight normal
                        :box (:line-width 1 :color ,now :style released-button)))
      ("NEXT"      . (:foreground ,next :weight normal
                        :box (:line-width 1 :color ,next :style released-button)))
      ("WAIT"      . (:foreground ,wait :slant italic
                        :underline (:style wave :color ,wait)
                        :box (:line-width 1 :color ,wait :style released-button)))
      ("DONE"      . (:foreground ,done :weight normal
                        :box (:line-width 1 :color ,done :style released-button)))
      ("CANCELLED" . (:foreground ,cancel :strike-through t :weight normal
                        :box (:line-width 1 :color ,cancel :style released-button))))))

(defun my/apply-org-complementary-faces-for-theme (theme)
  "Apply Org face overrides for THEME, using complementary palette colors."
  (let* ((p (my/build-org-complementary-palette))
         (accent-1 (my/palette-get p 'accent-1 "#d7875f"))
         (accent-2 (my/palette-get p 'accent-2 "#7fa2ff"))
         (accent-3 (my/palette-get p 'accent-3 "#b48ead"))
         (neutral (my/palette-get p 'neutral "#b0b0b0"))
         (table-fg (my/palette-get p 'table-fg neutral))
         (panel-bg (my/palette-get p 'panel-bg "#2a2a2a"))
         (panel-line-bg (my/palette-get p 'panel-line-bg "#303030"))
         (level-1-bg (my/palette-get p 'level-1-bg panel-bg))
         (todo (my/palette-get p 'todo accent-1))
         (done (my/palette-get p 'done "#7fd7af"))
         (todo-bg (my/palette-get p 'todo-bg panel-bg))
         (done-bg (my/palette-get p 'done-bg panel-bg)))
    (custom-theme-set-faces
     theme
     `(org-todo ((((class color) (min-colors 89))
                  (:weight normal :foreground ,todo :background ,todo-bg
                   :box (:line-width 1 :color ,todo :style released-button)))))
     `(org-done ((((class color) (min-colors 89))
                  (:weight normal :foreground ,done :background ,done-bg
                   :box (:line-width 1 :color ,done :style released-button)))))
     `(org-date ((t (:foreground ,accent-1 :background unspecified :box nil :weight normal))))
     `(org-headline-done ((t (:foreground ,done))))
     `(org-level-1 ((t (:inherit variable-pitch :extend t :background ,level-1-bg
                           :foreground ,accent-1 :overline ,accent-1
                           :weight normal :height 1.3))))
     `(org-level-2 ((t (:inherit variable-pitch :extend nil :foreground ,accent-2
                           :weight normal :height 1.2))))
     `(org-level-3 ((t (:inherit variable-pitch :extend nil :foreground ,accent-3
                           :weight normal :height 1.15))))
     `(org-level-4 ((t (:inherit variable-pitch :extend nil :foreground ,neutral
                           :weight normal :height 1.1))))
     `(org-level-5 ((t (:inherit variable-pitch :extend nil :foreground ,neutral
                           :weight normal :height 1.08))))
     `(org-level-6 ((t (:inherit variable-pitch :extend nil :foreground ,neutral
                           :weight normal :height 1.06))))
     `(org-level-7 ((t (:inherit variable-pitch :extend nil :foreground ,neutral
                           :weight normal :height 1.04))))
     `(org-level-8 ((t (:inherit variable-pitch :extend nil :foreground ,neutral
                           :weight normal :height 1.02))))
     `(org-table ((t (:foreground ,table-fg :background ,panel-bg :weight normal))))
     `(org-block ((t (:background ,panel-bg))))
     `(org-block-begin-line ((t (:extend t :background ,panel-line-bg :underline ,panel-line-bg))))
     `(org-block-end-line ((t (:extend t :background ,panel-line-bg :overline ,panel-line-bg))))
     `(org-table-header ((t (:foreground ,table-fg :background ,panel-bg :weight normal))))
     `(org-table-row ((t (:foreground ,neutral :weight normal)))))))

(defun my/apply-org-todo-keyword-faces-for-current-theme ()
  "Apply theme-specific `org-todo-keyword-faces` for target themes."
  (let* ((theme (car custom-enabled-themes))
         (target (and theme (memq theme my/org-complementary-themes))))
    (setq org-todo-keyword-faces
          (if target
              (my/build-org-todo-keyword-faces
               (my/build-org-complementary-palette))
            my/org-todo-keyword-faces-default))
    (when (fboundp 'org-set-regexps-and-options)
      (org-set-regexps-and-options))
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (derived-mode-p 'org-mode)
          (font-lock-flush)
          (font-lock-ensure))))))

(defun my/apply-solarized-dark-face-overrides ()
  "Apply non-Org face overrides only to the `solarized-dark' theme."
  (custom-theme-set-faces
   'solarized-dark
   '(bm-persistent-face ((t (:foreground "#000000" :background "#916e00"))))
   '(vertico-current ((t (:foreground "#000000" :background "#916e00" :weight normal))))
   '(link ((t (:foreground "#997544"))))
   '(magit-diff-added ((t (:foreground "#5f8700" :background "#17351f" :weight normal))))
   '(magit-diff-added-highlight ((t (:foreground "#5f8700" :background "#1b3d24" :weight normal))))
   '(magit-diff-removed ((t (:foreground "#af3a32" :background "#3a2020" :weight normal))))
   '(magit-diff-removed-highlight ((t (:foreground "#af3a32" :background "#442626" :weight normal))))
   '(hl-line ((t (:extend t :background "#083e4a"))))
   '(region ((t (:extend t :background "#073642"))))
   '(tab-bar-tab ((t (:background unspecified :foreground "#93a1a1" :box nil :overline "#268bd2" :weight semi-bold))))
   '(tab-bar-tab-inactive ((t (:background "#073642" :foreground "#657b83" :box nil :weight normal))))
   '(tab-bar-tab-highlight ((t (:background "#144855" :weight normal))))))

(defun my/maybe-apply-solarized-dark-face-overrides (&rest _)
  (let ((theme (car custom-enabled-themes)))
    (when (and theme (memq theme my/org-complementary-themes))
      (my/apply-org-complementary-faces-for-theme theme)
      ;; Re-enable so merged face specs refresh immediately.
      (enable-theme theme)
      ;; Force realized face attr; theme spec alone sometimes doesn't set this.
      (set-face-attribute 'org-level-1 nil :extend t))

    (when (memq 'solarized-dark custom-enabled-themes)
      (my/apply-solarized-dark-face-overrides)
      ;; Re-enable so merged face specs refresh immediately.
      (enable-theme 'solarized-dark)
      ;; Make selected tab background match buffer/default background.
      (let ((bg (face-background 'default nil t)))
        (when (stringp bg)
          (set-face-attribute 'tab-bar-tab nil :background bg)))))

  (my/apply-org-todo-keyword-faces-for-current-theme))

(add-hook 'after-load-theme-hook #'my/maybe-apply-solarized-dark-face-overrides)
(my/maybe-apply-solarized-dark-face-overrides)
