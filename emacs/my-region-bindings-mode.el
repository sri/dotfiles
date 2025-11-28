;; -*- lexical-binding: t; -*-
(require 'region-bindings-mode)
(require 'multiple-cursors)
(require 'which-key)
(require 'expand-region)


(add-hook 'magit-mode-hook 'region-bindings-mode-off)

(region-bindings-mode-enable)

(defun my/which-key-region-bindings ()
  (interactive)
  (which-key--show-keymap
   "Region Bindings"
   region-bindings-mode-map))

(bind-keys :map region-bindings-mode-map
           ("?" . my/which-key-region-bindings)

           ("A" . beginning-of-buffer)
           ("E" . end-of-buffer)

           ("a" . beginning-of-line)
           ("e" . end-of-line)

           ("B" . backward-sexp)
           ("F" . forward-sexp)

           ("b" . backward-word)
           ("f" . forward-word)

           ("w" . my/select-word)

           ("j" . next-line)
           ("k" . my/region-bindings-k)
           ("l" . my/select-line)
           ("c" . my/copy-line-or-region)
           ("d" . my/duplicate-line-or-region)
           ("C" . my/kill-line-or-region)

           (";" . my/comment-line-or-region)
           ("g" . my/rg)
           ("G" . my/google-search)
           ("J" . ace-jump-word-mode)
           ("m" . vr/mc-mark) ;; ?

           ("n" . mc/mark-next-like-this)
           ("N" . mc/skip-to-next-like-this)
           ("L" . mc/edit-lines)

           ("D" . er/mark-defun)
           ("u" . er/mark-url)
           ("q" . er/mark-inside-quotes)
           ("r" . er/expand-region)

           ("S" . sort-lines)
           ("SPC" . exchange-point-and-mark)

           ("i" . indent-rigidly)
           ("s" . my/region-bindings-s)
           ("/" . er/expand-region)
           ;; ("L" . ace-jump-line-mode)
           ;; By default, TAB does indent-region
           )


(defun my/magit-mode-p ()
  (memq major-mode '(magit-status-mode magit-diff-mode)))

(defun my/region-bindings-k ()
  (interactive)
  (if (my/magit-mode-p)
      (magit-discard)
    (previous-line)))

(defun my/region-bindings-s ()
  (interactive)
  (if (my/magit-mode-p)
      (magit-stage)
    (my/isearch-region)))

(add-to-list 'region-bindings-mode-disable-predicates
             'minibufferp)
