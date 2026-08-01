;; 2026-07-31 - trying out emacs's default completions
(defvar my/completions-style nil)

(setq completion-styles '(orderless basic substring initials flex))
(setq completion-category-overrides
      '((file (styles basic partial-completion))))
(setq completion-category-defaults nil)

(cond ((eq my/completions-style 'vertico+marginalia)
       ;; consult - previews, grouping, narrowing,
       ;; vertico,
       ;;  marginalia
       (require 'orderless)

       (require 'vertico)
       (vertico-mode 1)
       (setq vertico-cycle t)

       (require 'vertico-directory)
       (require 'vertico-quick)
       (require 'vertico-posframe)
       (vertico-posframe-mode -1)

       (require 'marginalia)
       (marginalia-mode 1)

       (require 'consult)
       ;; (vertico-multiform-mode -1)
       ;; (setq vertico-multiform-commands
       ;;       '((consult-grep buffer indexed)
       ;;         (consult-ripgrep buffer indexed)))
       ;; (setq vertico-multiform-categories
       ;;       '((consult-grep buffer)
       ;;         (consult-ripgrep buffer)))
)
      (t
       ;; For default
       (setq completion-show-help nil)
       (setq completion-show-inline-help nil)
       (setq completions-detailed t)
       (setq completions-format 'one-column)
       (setq completions-max-height 12)
       (setq completions-sort 'historical)
       (setq completion-auto-help t)
       ;; This selects the *Completions* window, not the first candidate.
       (setq completion-auto-select nil)
       (setq minibuffer-visible-completions t)
       (setq completion-eager-display t)
       (setq completion-eager-update t)

       (defun my/completion-select-first-completion ()
         (with-current-buffer (get-buffer "*Completions*")
           (minibuffer-next-line-completion 1)))
       (add-hook 'completion-setup-hook 'my/completion-select-first-completion)
       ))
