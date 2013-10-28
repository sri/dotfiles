(setenv "PAGER" "cat")

(require 'dirtrack)
(setq-default dirtrack-list (list "^\\([^(]+\\)\\( .*?\\)?[$] $" 1))
;;
;; (let ((possibles '("/$ " "~$ " "~/my/test (master)$ "))
;;       (results '()))
;;   (dolist (p possibles)
;;     (push (cons p
;;                 (and (string-match (car dirtrack-list) p)
;;                      (match-string (cadr dirtrack-list) p)))
;;           results))
;;   results)

(defun my-setup-shell-header-line ()
  (setq header-line-format
        (list (propertize " [↩]"
                          'mouse-face 'mode-line-highlight
                          'help-echo "erase buffer"
                          'local-map (my-make-header-line-mouse-map
                                      'down-mouse-1 #'ignore
                                      'mouse-1 #'my-header-line-shell-erase-buffer))
              (propertize " [↩!]"
                          'mouse-face 'mode-line-highlight
                          'help-echo "erase buffer & run prev cmd"
                          'local-map (my-make-header-line-mouse-map
                                      'down-mouse-1 #'ignore
                                      'mouse-1 #'my-header-line-shell-erase-buffer-and-run-prev-cmd)))))

(defun my-shell-forward-char-or-previous-history (&optional arg)
  (interactive "p")
  (if (eobp)
      (comint-previous-input arg)
    (forward-char arg)))

(defun my-shell-next-line-or-next-history (&optional arg)
  (interactive "p")
  (if (eobp)
      (comint-next-input arg)
    (next-line arg)))

(defun my-shell-erase-buffer ()
  (interactive)
  (erase-buffer)
  (comint-send-input))

(add-hook 'shell-mode-hook
          (lambda ()
            (my-setup-shell-header-line)
            (linum-mode -1)
            (setq line-number-mode nil
                  column-number-mode nil)
            (setq comint-input-sender
                  'my-emacs-rspec-command)
            (toggle-truncate-lines 1)
            ;(buffer-disable-undo)
            (dirtrack-mode)
            (define-key shell-mode-map (kbd "C-<up>")
              'comint-previous-prompt)
            (define-key shell-mode-map (kbd "C-<down>")
              'comint-next-prompt)
            (define-key shell-mode-map (kbd "C-c e")
              'my-shell-erase-buffer)
            (define-key shell-mode-map (kbd "<right>")
              'my-shell-forward-char-or-previous-history)
            (define-key shell-mode-map (kbd "<down>")
              'my-shell-next-line-or-next-history)))
