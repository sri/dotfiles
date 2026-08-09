;; -*- lexical-binding: t; -*-
;(load-theme 'ample-flat)

(load-theme (or (and (boundp 'my-theme) my-theme)
                'solarized-dark)
            t)

(defun my/set-face-if-exists (face &rest attrs)
  (when (facep face)
    (apply #'set-face-attribute face nil attrs)))

(defun my/solarized-tty-menu-faces ()
  "Make TTY menus match Solarized Dark."
  (unless (display-graphic-p)
    ;; Slightly darker than base03 so menus stand out more.
    (let ((menu-bg "#001f27")
          (menu-bg-selected "#073642")
          (menu-border "#0f3b49"))
      (my/set-face-if-exists
       'tty-menu-enabled-face
       :foreground "#93a1a1" :background menu-bg)
      (my/set-face-if-exists
       'tty-menu-disabled-face
       :foreground "#586e75" :background menu-bg)
      (my/set-face-if-exists
       'tty-menu-selected-face
       :foreground "#eee8d5" :background menu-bg-selected :weight 'bold
       :box `(:line-width -1 :color ,menu-border))
      ;; Exists only on some Emacs versions:
      (my/set-face-if-exists
       'tty-menu-header-face
       :foreground "#268bd2" :background menu-bg :weight 'bold
       :box `(:line-width -1 :color ,menu-border)))))

(add-hook 'tty-setup-hook #'my/solarized-tty-menu-faces)

;; Emacs 29+:
(when (boundp 'after-load-theme-hook)
  (add-hook 'after-load-theme-hook #'my/solarized-tty-menu-faces))

;; Older Emacs fallback:
(advice-add 'load-theme :after (lambda (&rest _) (my/solarized-tty-menu-faces)))

(face-spec-set 'isearch '((t (:background "#22aa22" :foreground "black"))))
(face-spec-set 'lazy-highlight '((t (:foreground "black" :background "green"))))

;(set-face-background 'lazy-highlight "#29422d")
;(face-spec-set 'hl-line '((t (:background "color-236" :inherit nil))))

(xterm-mouse-mode)

(dolist (key '([M-mouse-1]
               [M-drag-mouse-1]
               [M-down-mouse-1]
               [M-mouse-2]
               [M-mouse-3]))
  (global-unset-key key))

(setq select-enable-clipboard t
      select-enable-primary nil)

;; Enable Emacs's built-in OSC 52 clipboard support for every TTY.  This must
;; run per terminal: when Emacs is a daemon, doing it only during startup sets
;; the parameter on `initial_terminal`, not on later emacsclient frames.
(require 'term/xterm)

(defun my/enable-tty-osc52-clipboard ()
  (unless (display-graphic-p)
    (xterm--init-activate-set-selection)))

(add-hook 'tty-setup-hook #'my/enable-tty-osc52-clipboard)

;; Also fix terminal frames that already exist when this file is reloaded.
(dolist (terminal (terminal-list))
  (set-terminal-parameter terminal 'xterm--set-selection t))
