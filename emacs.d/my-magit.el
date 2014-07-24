(require 'magit)

;; For when I am view the diff -- have "," close
;; the current diff and open the next one, if it
;; is there.
(defun my-magit-close-current-section ()
  (interactive)
  (let* ((current (magit-current-section))
         (type (magit-section-type current)))
    (when (eq type 'hunk)
      (setq current (magit-section-parent current))
      (setq type (magit-section-type current)))
    (unless (eq type 'diff)
      (error "unknow type %s" type))
    (goto-char (magit-section-beginning current))
    (magit-section-hideshow t)
    (when (magit-find-section-after (point))
      (magit-goto-next-section)
      (recenter 0)
      (magit-section-hideshow nil))))

(define-key magit-status-mode-map (kbd ",")
  'my-magit-close-current-section)

;; Don't highlight sections.
(defun magit-highlight-section ()
  nil)

(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)

(setq magit-status-buffer-switch-function 'switch-to-buffer)

;; (defun my-magit-close-current-open-next ()
;;   (interactive)
;;   (magit-goto-parent-section)
;;   (magit-toggle-section)
;;   (magit-goto-next-section)
;;   (recenter 0))

;; (defun my-magit-click ()
;;   (cond ((memq major-mode '(magit-log-mode magit-branch-manager-mode))
;;          (magit-show-item-or-scroll-up))
;;         ((eq major-mode 'magit-status-mode)
;;          (let* ((current (magit-current-section))
;;                 (current-title (format "%s" (magit-section-title current)))
;;                 (parent (magit-section-parent current)))
;;            (if (and parent
;;                     (eq (magit-section-title parent) 'stashes))
;;                (magit-show-item-or-scroll-up)
;;              (unless (string-prefix-p "@@" current-title)
;;                (magit-toggle-section)))))
;;         ((memq major-mode '(magit-wazzup-mode
;;                             magit-commit-mode
;;                             magit-log-edit-mode
;;                             magit-stash-mode
;;                             magit-reflog-mode
;;                             magit-diff-mode))
;;          (magit-toggle-section))))
