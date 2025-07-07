;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; In case of errors, I don't have to restart emacs
;; from the command-line with --debug-init passed in.
(setq debug-on-error t)

(defvar my-emacs-start-time (current-time))
(defvar my-emacs-elapsed-time)

(require 'cl-lib)
(require 'subr-x)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(defvar my/colors
  '(
    "#185e7f"
    "#2483b2"
    "#2eb624"
    "#2ecfdc"
    "#3dabc4"
    "#41e636"
    "#422f73"
    "#66cce2"
    "#6e52b9"
    "#9c3991"
    "#b653af"
    "#cc1228"
    "#e96921"
    "#f42b3d"
    "#fa8e4c"
    "#fbc050"
    "#fed98a"
    ))

(defun my/font (&optional font)
  (interactive "sFont name: ")
  (if font
      (mouse-select-font)
    (let ((fonts '("JetBrains Mono"
                   "Inconsolata")))
      (seq-find (lambda (f)
                  (if (find-font (font-spec :name f))
                      f
                    (message "my/font: cannot find %s" f)
                    nil))
                fonts))))

(defun my/font-size ()
  (cond ((eq system-type 'darwin) 140)
        (t 100)))

(defun my/load (path &optional ignore-if-missing)
  "Load the file specified by PATH.
Byte-compile the file, if necessary, before loading it.
On any errors, enter recursive edit, to fix errors, after which,
try to load the source again."
  (let* ((full-path-no-ext (file-name-sans-extension (expand-file-name path)))
         (source (concat full-path-no-ext ".el"))
         (compiled (concat full-path-no-ext ".elc")))
    (cond ((file-exists-p source)
           (let ((source-newer (file-newer-than-file-p source compiled))
                 (compile-errors)
                 (file-to-load))
             (when source-newer
               (if my/load-print-messages
                   (message "Source(%s) is newer, byte-compiling it..." source))
               (setq compile-errors (eq (byte-compile-file source) 'nil))
               (if (and compile-errors my/load-print-messages)
                   (message "...error byte-compiling it")))
             (setq file-to-load (if compile-errors source compiled))
             (if my/load-print-messages (message "Loading %s" file-to-load))
             (condition-case err
                 (load file-to-load nil t t)
               (error
                (message "Error while loading file %s\nFix the error and hit %s"
                         source
                         (key-description (car (where-is-internal 'exit-recursive-edit))))
                (sit-for 1.0)
                (switch-to-buffer (find-file source))
                (debug err)
                (recursive-edit)
                (my/load source ignore-if-missing)))))
          (t
           (message "%s does not exist" source)
           (if (file-exists-p compiled) (delete-file compiled))
           (if (null ignore-if-missing) (error "my/load: missing %s" source))))))

(defun my/load-all ()
  ;; Load packages and install them if necessary.
  (let* ((package--builtins '())
         (missing (cl-remove-if 'package-installed-p package-selected-packages)))
    (when missing
      (package-refresh-contents)
      (mapc 'package-install missing)))

  ;; Load my files
  (let* ((default-directory "~/my/dotfiles/emacs")
         (gui "my-gui")
         (non-gui "my-terminal")
         (this "my-dot-emacs")
         (base '("my-fns"
                 "my-scratch"
                 "my-register"
                 "my-env"
                 "my-keys"
                 "my-view"
                 "my-sublime"
                 "my-vscode"
                 "my-shell"
                 "my-occur"
                 "my-isearch"
                 "my-help"
                 "my-dired"
                 "my-modeline"))
         (all (append (list gui non-gui this) base))
         (pkg-customizations
          (cl-remove-if (lambda (el-file)
                       ;; remove autosaves
                       (or (string-prefix-p ".#" el-file)
                           (string-prefix-p "my-dot-emacs" el-file)
                           (cl-some (lambda (my) (string= (concat my ".el") el-file))
                                 all)))
                     (directory-files "." nil "\\.el$" t))))

    (when window-system
      '(require 'exec-path-from-shell)
      '(exec-path-from-shell-initialize))

    ;; Files that aren't on MELPA or any other package archive.
    (mapc 'my/load (directory-files "third-party" 'full "\\.el$" t))
    (mapc 'my/load base)
    (mapc 'my/load pkg-customizations)
    (my/load "~/.emacs.private.el" 'ignore-if-missing)
    (my/load (if window-system gui non-gui)))

  (setq my-emacs-elapsed-time
        (float-time (time-subtract (current-time) my-emacs-start-time)))

  (recentf-open-files))

(my/load-all)
(setq debug-on-error nil)
