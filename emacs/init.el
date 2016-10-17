(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

(require 'cl)
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(require 'org)
(let ((compiled (expand-file-name "~/.emacs.d/emacs.elc"))
      (tangled (expand-file-name "~/.emacs.d/emacs.org")))
  (if (file-newer-than-file-p tangled compiled)
      (org-babel-load-file tangled 'compile)
    (load compiled nil 'nosuffix)))
