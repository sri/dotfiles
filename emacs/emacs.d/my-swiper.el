(ivy-mode 1)

(setq ivy-extra-directories nil)

(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "<left>") 'ivy-backward-delete-char)
(define-key ivy-minibuffer-map (kbd "<right>") 'ivy-partial-or-done)
(define-key ivy-minibuffer-map (kbd "C-a") 'ivy-beginning-of-buffer)
(define-key ivy-minibuffer-map (kbd "C-e") 'ivy-end-of-buffer)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
