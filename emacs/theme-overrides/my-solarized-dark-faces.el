;; -*- lexical-binding: t; -*-

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
   '(org-date ((t (:foreground "#268bd2" :background "#003f5e" :box (:line-width 1 :color "#268bd2" :style released-button)))))
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
  (when (memq 'solarized-dark custom-enabled-themes)
    (my/apply-solarized-dark-face-overrides)
    ;; Re-enable so merged face specs refresh immediately.
    (enable-theme 'solarized-dark)
    ;; Force realized face attr; theme spec alone sometimes doesn't set this.
    (set-face-attribute 'org-level-1 nil :extend t)))

(add-hook 'after-load-theme-hook #'my/maybe-apply-solarized-dark-face-overrides)
(my/maybe-apply-solarized-dark-face-overrides)
