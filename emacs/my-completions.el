;; default new-
(defvar my/completions-style nil)





;; consult - previews, grouping, narrowing,
;; vertico,
;;  marginalia

(cond ((eq my/completions-style 'vertico+marginalia)
       (require 'orderless)
       (setq completion-styles '(orderless basic substring initials flex))
       (setq completion-category-overrides
             '((file (styles basic partial-completion))))
       (setq completion-category-defaults nil)

       (require 'vertico)
       (vertico-mode 1)
       (setq vertico-cycle t)

       (require 'vertico-directory)
       (require 'vertico-quick)
       (require 'vertico-posframe)
       (vertico-posframe-mode -1)
       ;; (vertico-multiform-mode -1)

       ;; (setq vertico-multiform-commands
       ;;       '((consult-grep buffer indexed)
       ;;         (consult-ripgrep buffer indexed)))

       ;; (setq vertico-multiform-categories
       ;;       '((consult-grep buffer)
       ;;         (consult-ripgrep buffer)))


       (require 'marginalia)
       (marginalia-mode 1)

       (require 'consult))
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

       ;; With `minibuffer-visible-completions', RET accepts the candidate at
       ;; point in *Completions*.  Emacs doesn't currently have a variable to
       ;; preselect the first candidate, so do it after the completions buffer
       ;; is shown/updated.
       (defun my/completions-select-first-candidate (&rest _)
         (when-let* ((win (get-buffer-window "*Completions*" 'visible)))
           (with-current-buffer (window-buffer win)
             (unless (get-text-property (window-point win) 'completion--string)
               (save-selected-window
                 (with-selected-window win
                   (first-completion)
                   (set-window-point win (point))))))))

       (advice-add 'minibuffer-completion-help
                   :after #'my/completions-select-first-candidate)
       (advice-add 'completion-help-at-point
                   :after #'my/completions-select-first-candidate)))
