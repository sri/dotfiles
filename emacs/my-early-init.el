;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 50 1024 1024))))

(defvar my/dotfiles-dir "~/my/dotfiles/emacs")
(setq custom-file (expand-file-name "my-custom.el" my/dotfiles-dir))
(load custom-file)
(load (expand-file-name "my-solarized-dark-faces.el" my/dotfiles-dir) t)

(require 'package)
(package-initialize)

(message "early init done")
