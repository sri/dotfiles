(defvar my-packages
  '(color-theme color-theme-solarized magit bm autopair go-mode org
                macrostep yasnippet))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(let ((missing '()))
  (dolist (p my-packages)
    (unless (package-installed-p p)
      (push p missing)))
  (when missing
    (package-refresh-contents)
    (dolist (p missing)
      (package-install p))))

(load-theme 'solarized-dark t)
