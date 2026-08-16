;; -*- lexical-binding: t; -*-
(my/load-current-theme)

(add-hook 'tty-setup-hook #'my/solarized-tty-menu-faces)

;; Emacs 29+:
(when (boundp 'after-load-theme-hook)
  (add-hook 'after-load-theme-hook #'my/solarized-tty-menu-faces))

;; Older Emacs fallback:
(advice-add 'load-theme :after (lambda (&rest _) (my/solarized-tty-menu-faces)))

(my/solarized-tty-menu-faces)

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
