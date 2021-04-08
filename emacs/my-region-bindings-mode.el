(require 'region-bindings-mode)
(require 'multiple-cursors)
(require 'which-key)
(require 'expand-region)

(region-bindings-mode-enable)

(defun my/which-key-region-bindings ()
  (interactive)
  (which-key--show-keymap
   "Region Bindings"
   region-bindings-mode-map))

(bind-keys :map region-bindings-mode-map
           ("?" . my/which-key-region-bindings)

           (";" . my/comment-line-or-region)

           ("c" . my/copy-line-or-region)
           ("C" . my/kill-line-or-region)

           ("D" . er/mark-defun)
           ("q" . er/mark-inside-quotes)

           ("A" . beginning-of-buffer)
           ("E" . end-of-buffer)

           ("b" . backward-word)
           ("f" . forward-word)
           ("w" . my/select-word)

           ("B" . backward-sexp)
           ("F" . forward-sexp)

           ("J" . ace-jump-word-mode)
           ;; ("L" . ace-jump-line-mode)

           ("L" . mc/edit-lines)
           ("N" . mc/skip-to-next-like-this)
           ("n" . mc/mark-next-like-this)

           ("SPC" . exchange-point-and-mark)

           ("S" . sort-lines)

           ("s" . my/isearch-region)

           ("a" . beginning-of-line)
           ("e" . end-of-line)

           ("d" . my/duplicate-line-or-region)

           ;; By default, TAB does indent-region
           ("i" . indent-rigidly)

           ("j" . next-line)
           ("k" . my/region-bindings-k)
           ("l" . my/select-line)

           ("m" . vr/mc-mark)

           ("r" . er/expand-region)
           ("u" . er/mark-url)
           )

(defun my/region-bindings-k ()
  (interactive)
  (if (eq major-mode 'magit-status-mode)
      (magit-discard)
    (previous-line)))

(add-to-list 'region-bindings-mode-disable-predicates
             'minibufferp)
;; (global-set-key (kbd "C-i") 'calendar)
;; (global-set-key (kbd "<tab>") 'beginning-of-line)
