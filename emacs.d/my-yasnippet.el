(require 'advice)

(require 'yasnippet)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; I have yas working with hippie-expand.
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(defadvice yas--modes-to-activate (after my-yas-activate-global-mode ())
  (let ((modes ad-return-value))
    (setq ad-return-value (append modes (list 'global-mode)))))

(ad-activate 'yas--modes-to-activate)
