;; -*- lexical-binding: t; -*-
(defvar my/disabled-eln-cache
  (expand-file-name "disabled-eln-cache/" user-emacs-directory))

(make-directory my/disabled-eln-cache t)

;; Disable native compilation as early as possible.
;; `package-native-compile' only affects package.el; the native-comp JIT can
;; still compile loaded .elc files unless these are also disabled.
(setq package-native-compile nil
      native-comp-jit-compilation nil
      native-comp-deferred-compilation nil
      ;; Disable on-demand native compilation of trampolines for primitives
      ;; like `sort'.  This can still happen even when package native-comp is
      ;; disabled.
      native-comp-enable-subr-trampolines nil
      comp-enable-subr-trampolines nil
      ;; Don't load previously generated native-code files from the eln cache.
      ;; This is stricter than just disabling new native compilation.
      native-comp-eln-load-path (list my/disabled-eln-cache)
      ;; Even if something explicitly invokes native compilation, keep functions
      ;; in bytecode form.  Emacs may still produce .eln files containing
      ;; bytecode, but not native code.
      native-comp-speed -1)

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 50 1024 1024))))

(defvar my/dotfiles-dir "~/my/dotfiles/emacs")
(setq custom-file (expand-file-name "my-custom.el" my/dotfiles-dir))
(load custom-file)

(setq package-install-upgrade-built-in t)

(require 'package)
(package-initialize)

(message "early init done")
