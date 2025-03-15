;; -*- mode: emacs-lisp -*-

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

(defvar my/loading-errors '())

(add-hook 'after-init-hook
          #'(lambda ()
              (when my/loading-errors
                (with-current-buffer (get-buffer-create "*loading-errors*")
                  (insert "Loading errors:\n")
                  (save-excursion
                    (dolist (path my/loading-errors)
                      (insert path "\n")))
                  (pop-to-buffer (current-buffer)))))
          t)

;; Load the byte-compiled version of file.
(defun my/load (path &optional ignore-if-missing)
  ;; get full path without extension
  (setq path (file-name-sans-extension (expand-file-name path)))

  (let ((source (concat path ".el"))
        (compiled (concat path ".elc")))
    (cond ((file-exists-p source)
           (if (and (file-newer-than-file-p source compiled)
                    (null (byte-compile-file source)))
               ;; If we fail byte compiling the source, don't error
               ;; out. Just record an error message and try to load
               ;; the compiled file. This would be the case where I
               ;; recently edited a file and accidently left a syntax
               ;; error. Erroring out here would leave Emacs in a
               ;; broken state -- without my keybindings, etc.
               (push source my/loading-errors))
           (if (file-exists-p compiled)
               (condition-case err
                   (load compiled nil t t)
                 (error
                  (message "Error while loading file %s\nFix the error and hit %s"
                           source
                           (key-description (car (where-is-internal 'exit-recursive-edit))))
                  (sit-for 1.0)
                  (switch-to-buffer (find-file source))
                  (debug err)
                  (recursive-edit)
                  (my/load path ignore-if-missing)))))
          (t
           (if (file-exists-p compiled)
               (delete-file compiled))
           (if (null ignore-if-missing)
               (error "my/load: missing %s" source))))))

(defun my/load-all ()
  (setq my/loading-errors '())

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
         (diminish "my-diminish")
         (base '("my-fns"
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
         (all (cl-list* gui non-gui this diminish base))
         (pkg-customizations
          (cl-remove-if (lambda (el-file)
                       ;; remove autosaves
                       (or (string-prefix-p ".#" el-file)
                           (string-prefix-p "my-dot-emacs" el-file)
                           (cl-some (lambda (my) (string= (concat my ".el") el-file))
                                 all)))
                     (directory-files "." nil "\\.el$" t))))

    (when window-system
      (require 'exec-path-from-shell)
      (exec-path-from-shell-initialize))

    ;; Files that aren't on MELPA or any other package archive.
    (mapc 'my/load (directory-files "third-party" 'full "\\.el$" t))
    (mapc 'my/load base)
    (mapc 'my/load pkg-customizations)
    (my/load diminish)
    (my/load "~/.emacs.private.el" 'ignore-if-missing)
    (my/load (if window-system gui non-gui)))

  (setq my-emacs-elapsed-time
        (float-time (time-subtract (current-time) my-emacs-start-time)))

  (recentf-open-files))

(my/load-all)
