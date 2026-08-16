;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defvar my-themes
  '(solarized-gruvbox
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
    spacemacs-dark))

(defvar my/theme-registry-file
  (expand-file-name "../theme/themes.json" my/dotfiles-dir)
  "Registry used by the ,theme command.")

(defun my/theme-registry-emacs-theme (&optional fallback)
  "Return the Emacs theme selected in `my/theme-registry-file'.
Return FALLBACK when the registry cannot be read or is incomplete."
  (condition-case err
      (let* ((registry
              (with-temp-buffer
                (insert-file-contents my/theme-registry-file)
                (json-parse-buffer :object-type 'alist)))
             (current (alist-get "current" registry nil nil #'string=))
             (themes (alist-get "themes" registry nil nil #'string=))
             (entry (and current
                         (alist-get current themes nil nil #'string=)))
             (name (and entry
                        (alist-get "emacs" entry nil nil #'string=))))
        (if (and (stringp name) (not (string-empty-p name)))
            (intern name)
          fallback))
    (error
     (message "Could not read theme registry: %s" (error-message-string err))
     fallback)))

(defvar my-theme
  (my/theme-registry-emacs-theme 'solarized-dark)
  "Current Emacs theme, selected by the ,theme command.")

(defun my/set-face-if-exists (face &rest attrs)
  "Set FACE attributes to ATTRS when FACE exists."
  (when (facep face)
    (apply #'set-face-attribute face nil attrs)))

(defun my/solarized-tty-menu-faces ()
  "Make TTY menus follow the currently enabled theme.
The historical function name is retained so existing hooks and advice keep
working when this file is reloaded."
  (unless (display-graphic-p)
    (let ((menu-bg (face-background 'default nil t))
          (menu-fg (face-foreground 'default nil t))
          (menu-bg-selected (face-background 'hl-line nil t))
          (menu-disabled (face-foreground 'shadow nil t))
          (menu-border (face-foreground 'font-lock-keyword-face nil t)))
      (my/set-face-if-exists
       'tty-menu-enabled-face
       :foreground menu-fg :background menu-bg)
      (my/set-face-if-exists
       'tty-menu-disabled-face
       :foreground menu-disabled :background menu-bg)
      (my/set-face-if-exists
       'tty-menu-selected-face
       :foreground menu-fg :background menu-bg-selected :weight 'bold
       :box `(:line-width -1 :color ,menu-border))
      ;; Exists only on some Emacs versions:
      (my/set-face-if-exists
       'tty-menu-header-face
       :foreground menu-border :background menu-bg :weight 'bold
       :box `(:line-width -1 :color ,menu-border)))))

(defun my/clear-legacy-theme-face-overrides ()
  "Remove old hard-coded TTY search colors when they are still present."
  (dolist (face-and-spec
           '((isearch ((t (:background "#22aa22" :foreground "black"))))
             (lazy-highlight ((t (:foreground "black" :background "green"))))))
    (let ((face (car face-and-spec))
          (legacy-spec (cadr face-and-spec)))
      (when (equal (get face 'face-override-spec) legacy-spec)
        (face-spec-set face nil 'reset)))))

(defun my/base16-truecolor-spec (spec)
  "Make base16 SPEC use its hex-color branch on truecolor terminals."
  (mapcar
   (lambda (clause)
     (if (equal (car-safe clause) '((type graphic)))
         (cons '((min-colors 16777216)) (cdr clause))
       clause))
   spec))

(defun my/prepare-base16-theme (theme)
  "Make base16 THEME work correctly on Emacs 32 truecolor terminals."
  (let ((empty-group-face-p
         (lambda (face)
           (string-match-p
            "\\`gnus-group-\\(?:mail\\|news\\)-[1-6]-empty\\'"
            (symbol-name face)))))
    (put theme 'theme-settings
         (cl-loop for entry in (get theme 'theme-settings)
                  unless (and (eq (car-safe entry) 'theme-face)
                              (funcall empty-group-face-p (cadr entry)))
                  do (when (eq (car-safe entry) 'theme-face)
                       (setf (nth 3 entry)
                             (my/base16-truecolor-spec (nth 3 entry))))
                  and collect entry))
    ;; `custom-theme-set-faces' records each spec both on the theme and face.
    (dolist (face (face-list))
      (if (funcall empty-group-face-p face)
          (put face 'theme-face
               (assq-delete-all theme (get face 'theme-face)))
        (when-let* ((theme-spec (assq theme (get face 'theme-face))))
          (setf (cadr theme-spec)
                (my/base16-truecolor-spec (cadr theme-spec))))))))

(defun my/switch-theme-cleanly (theme)
  "Disable old themes, clear stale face state, and enable THEME."
  (interactive
   (list (intern (completing-read "Theme: "
                                  (mapcar #'symbol-name
                                          (custom-available-themes))
                                  nil t))))
  (mapc #'disable-theme (copy-sequence custom-enabled-themes))
  (my/clear-legacy-theme-face-overrides)
  ;; Recalculate every face with no enabled theme first.  This clears direct
  ;; attributes left behind by an old theme whose bookkeeping became stale.
  (dolist (face (face-list))
    (ignore-errors (face-spec-recalc face)))
  (if (string-prefix-p "base16-" (symbol-name theme))
      (progn
        ;; Base16 20260802 and Emacs 32 disagree about the direction of
        ;; inheritance for Gnus's empty group faces.  Load without enabling,
        ;; remove those redundant specs, and then enable the corrected theme.
        (load-theme theme :no-confirm :no-enable)
        (my/prepare-base16-theme theme)
        (enable-theme theme)
        (run-hooks 'after-load-theme-hook))
    (load-theme theme :no-confirm))
  (set 'my-theme theme)
  (unless (equal custom-enabled-themes (list theme))
    (error "Theme switch left unexpected enabled themes: %S"
           custom-enabled-themes))
  (message "Using theme %s" theme)
  theme)

(defun my/load-current-theme ()
  "Load `my-theme' using a clean theme transition."
  (my/switch-theme-cleanly my-theme))

(defun my-try-theme (theme)
  "Temporarily switch to THEME without changing the theme registry."
  (interactive
   (list (intern (completing-read "Try theme: "
                                  (mapcar #'symbol-name my-themes)
                                  nil t))))
  (my/switch-theme-cleanly theme))
