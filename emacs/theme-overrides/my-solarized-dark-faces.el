;; -*- lexical-binding: t; -*-

(defvar my/org-todo-keyword-faces-default org-todo-keyword-faces
  "Default `org-todo-keyword-faces` to restore outside solarized-dark.")

(defconst my/solarized-dark-org-todo-keyword-faces
  '(("TODO"      . (:foreground "#A9552B" :weight normal
                     :box (:line-width 1 :color "#A9552B" :style released-button)))
    ("IDEA"      . (:foreground "#6c71c4" :slant italic :weight normal
                     :box (:line-width 1 :color "#6c71c4" :style released-button)))
    ("NOW"       . (:foreground "#268bd2" :weight normal
                     :box (:line-width 1 :color "#268bd2" :style released-button)))
    ("NEXT"      . (:foreground "#b58900" :weight normal
                     :box (:line-width 1 :color "#b58900" :style released-button)))
    ("WAIT"      . (:foreground "#cb4b16" :slant italic
                     :underline (:style wave :color "#cb4b16")
                     :box (:line-width 1 :color "#cb4b16" :style released-button)))
    ("DONE"      . (:foreground "#5E8B6F" :weight normal
                     :box (:line-width 1 :color "#5E8B6F" :style released-button)))
    ("CANCELLED" . (:foreground "#dc322f" :strike-through t :weight normal
                     :box (:line-width 1 :color "#dc322f" :style released-button))))
  "Org TODO keyword faces used only for `solarized-dark`.")

(defun my/apply-org-todo-keyword-faces-for-current-theme ()
  "Apply theme-specific `org-todo-keyword-faces`.
Uses solarized-dark mappings only when that theme is enabled."
  (setq org-todo-keyword-faces
        (if (memq 'solarized-dark custom-enabled-themes)
            my/solarized-dark-org-todo-keyword-faces
          my/org-todo-keyword-faces-default))
  (when (fboundp 'org-set-regexps-and-options)
    (org-set-regexps-and-options))
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (derived-mode-p 'org-mode)
        (font-lock-flush)
        (font-lock-ensure)))))

(defun my/apply-solarized-dark-face-overrides ()
  "Apply my face overrides only to the `solarized-dark' theme."
  (custom-theme-set-faces
   'solarized-dark
   '(bm-persistent-face ((t (:foreground "#000000" :background "#916e00"))))
   '(link ((t (:foreground "#997544"))))
   '(org-todo ((((class color) (min-colors 89))
                (:weight normal :foreground "#A9552B" :background "#1F3328" :box (:line-width 1 :color "#A9552B" :style released-button)))))
   '(org-done ((((class color) (min-colors 89))
                (:weight normal :foreground "#5E8B6F" :background "#1F3328" :box (:line-width 1 :color "#5E8B6F" :style released-button)))))
   '(org-date ((t (:foreground "#C26D3A" :background unspecified :box nil :weight normal))))
   '(org-headline-done ((t (:foreground "#5E8B6F"))))
   '(org-level-1 ((t (:inherit variable-pitch :extend t :background "#073642" :foreground "#C26D3A" :overline "#C26D3A" :weight normal :height 1.3))))
   '(org-level-2 ((t (:inherit variable-pitch :extend nil :foreground "#A9552B" :weight normal :height 1.2))))
   '(org-level-3 ((t (:inherit variable-pitch :extend nil :foreground "#6c71c4" :weight normal :height 1.15))))
   '(org-level-4 ((t (:inherit variable-pitch :extend nil :foreground "#839496" :weight normal :height 1.1))))
   '(org-level-5 ((t (:inherit variable-pitch :extend nil :foreground "#839496" :weight normal :height 1.08))))
   '(org-level-6 ((t (:inherit variable-pitch :extend nil :foreground "#839496" :weight normal :height 1.06))))
   '(org-level-7 ((t (:inherit variable-pitch :extend nil :foreground "#839496" :weight normal :height 1.04))))
   '(org-level-8 ((t (:inherit variable-pitch :extend nil :foreground "#839496" :weight normal :height 1.02))))
   '(org-table ((t (:foreground "#93a1a1" :background "#073642" :weight normal))))
   '(org-block ((t (:background "#073642"))))
   '(org-block-begin-line ((t (:extend t :background "#06303b" :underline "#06303b"))))
   '(org-block-end-line ((t (:extend t :background "#06303b" :overline "#06303b"))))
   '(org-table-header ((t (:foreground "#93a1a1" :background "#073642" :weight normal))))
   '(org-table-row ((t (:foreground "#839496" :weight normal))))
   '(magit-diff-added ((t (:foreground "#5f8700" :background "#17351f" :weight normal))))
   '(magit-diff-added-highlight ((t (:foreground "#5f8700" :background "#1b3d24" :weight normal))))
   '(magit-diff-removed ((t (:foreground "#af3a32" :background "#3a2020" :weight normal))))
   '(magit-diff-removed-highlight ((t (:foreground "#af3a32" :background "#442626" :weight normal))))
   '(hl-line ((t (:extend t :background "#083e4a"))))
   '(region ((t (:extend t :background "#073642"))))
   '(tab-bar-tab ((t (:background "#0b3a46" :foreground "#93a1a1" :box nil :overline "#268bd2" :weight semi-bold))))
   '(tab-bar-tab-inactive ((t (:background "#073642" :foreground "#657b83" :box nil :weight normal))))
   '(tab-bar-tab-highlight ((t (:background "#144855" :weight normal))))))

(defun my/maybe-apply-solarized-dark-face-overrides (&rest _)
  (if (memq 'solarized-dark custom-enabled-themes)
      (progn
        (my/apply-solarized-dark-face-overrides)
        ;; Re-enable so merged face specs refresh immediately.
        (enable-theme 'solarized-dark)
        ;; Force realized face attr; theme spec alone sometimes doesn't set this.
        (set-face-attribute 'org-level-1 nil :extend t)))
  (my/apply-org-todo-keyword-faces-for-current-theme))

(add-hook 'after-load-theme-hook #'my/maybe-apply-solarized-dark-face-overrides)
(my/maybe-apply-solarized-dark-face-overrides)
