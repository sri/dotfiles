;; -*- mode: emacs-lisp -*-
(defvar my-emacs-start-time (current-time))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130))))
 '(bm-fringe-face ((t (:foreground "#859900")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((eval add-hook (quote after-save-hook) (lambda nil (byte-compile-file "my-emacs.el")) nil t)))))

(load (expand-file-name "~/.emacs.d/my-emacs.elc"))
