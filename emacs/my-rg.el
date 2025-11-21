;; -*- lexical-binding: t; -*-
(require 'rg)
(require 's)

;;; ChatGPT 5.1
(add-to-list 'display-buffer-alist
             '("^[*]rg[*].*"
               (display-buffer-reuse-window
                display-buffer-at-bottom)))

(defvar my/rg-rr-windows nil
  "List of windows participating in round-robin target selection.")

(defun my/rg-rr-build-window-list ()
  "Build list of all windows except the *rg* bottom window."
  (setq my/rg-rr-windows
        (seq-filter
         (lambda (w)
           (not (string-match-p "\\*rg\\*" (buffer-name (window-buffer w)))))
         (window-list))))

(defun my/rg-rr-next-window ()
  "Return next window in round-robin, rebuilding if needed."
  (when (or (null my/rg-rr-windows)
            ;; someone changed windows
            (not (cl-every #'window-live-p my/rg-rr-windows)))
    (my/rg-rr-build-window-list))
  (let ((win (car my/rg-rr-windows)))
    (setq my/rg-rr-windows (append (cdr my/rg-rr-windows)
                                   (list win)))
    win))

;; (defun my/rg-open-at-point-rr ()
;;   "Open rg result at point using real compilation data + round-robin window."
;;   (interactive)
;;   (let* ((msg (get-text-property (point) 'compilation-message)))
;;     (unless msg
;;       (user-error "No result on this line"))
;;     (let* ((loc (compilation--message->loc msg))
;;            (buf (marker-buffer (car (compilation--loc->markers loc))))
;;            (win (my/rg-rr-next-window))
;;            (display-buffer-overriding-action
;;             `((display-buffer-reuse-window
;;                display-buffer-use-some-window)
;;               (inhibit-same-window . t)
;;               (window . ,win))))
;;       (compilation-goto-locus loc msg))))

;; (define-key rg-mode-map (kbd "RET") #'my/rg-open-at-point-rr)
;; (define-key rg-mode-map [mouse-1] #'my/rg-open-at-point-rr)
;; (define-key rg-mode-map [mouse-2] #'my/rg-open-at-point-rr)

;; Advice compilation-goto-locus to use our round-robin window
(defun my/compilation-goto-locus-rr (orig fun-with info &rest args)
  (if (eq major-mode 'rg-mode)
      (let ((display-buffer-overriding-action
             `((display-buffer-reuse-window
                display-buffer-use-some-window)
               (inhibit-same-window . t)
               (window . ,(my/rg-rr-next-window)))))
        (apply orig fun-with info args))
    (apply orig fun-with info args)))

(advice-add 'compilation-goto-locus :around #'my/compilation-goto-locus-rr)
;;; End ChatGPT


;; Create a unique buffer -- from the rg.el docs
(defadvice rg-run (before rg-run-before activate)
  ;; TODO: buffer name should be
  ;; "repo ROOT <query>" or "repo query <dir>"
  (rg-save-search))

(defun my/rg (&optional use-git-repo-root)
  (interactive)
  (let* ((pattern (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning)
                                                      (region-end))
                    (rg-read-pattern nil)))
         ;; ChatGPT 5.1
         (looks-like-regexp (or (string-match-p "[.*+?[^]$(){}|\\]" pattern)
                                ;; smart case: if you have uppercase and lowercase,
                                ;; then its a literal; otherwise, its a pattern
                                (not (and (string-match-p "[A-Z]" pattern)
                                          (string-match-p "[a-z]" pattern)))))
         (dir (my/git-repo-root)))
    (rg-run pattern "everything" dir
            (if looks-like-regexp nil t))))

(defun my/redo-search-from-git-repo-root ()
  (interactive)
  (setf (rg-search-dir rg-cur-search) (my/git-repo-root))
  (rg-rerun))

(defun my/rg-menu ()
  (interactive)
  (let ((transient-default-level 7))
    (rg-menu)))

;; ChatGPT
(defun my/rg-tag-default-around (orig-fun &rest args)
  "Return nil in dired buffers, otherwise call ORIG-FUN."
  (if (derived-mode-p 'dired-mode)
      nil
    (apply orig-fun args)))

(advice-add 'rg-tag-default :around #'my/rg-tag-default-around)

(define-key rg-mode-map (kbd "R") 'my/redo-search-from-git-repo-root)
(define-key rg-mode-map (kbd "m") 'my/rg-menu)
(define-key rg-mode-map (kbd "N") 'rg-next-file)
(define-key rg-mode-map (kbd "P") 'rg-prev-file)
