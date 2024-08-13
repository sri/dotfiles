;; -*- mode: emacs-lisp -*-
(defvar my-emacs-start-time (current-time))
(defvar my-emacs-elapsed-time)

(require 'cl)
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


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 170 :family "JetBrains Mono"))))
 '(bm-persistent-face ((t (:background "#6e52b9" :foreground "#ffffff"))))
 '(isearch ((t (:background "#ffffff" :foreground "#6e52b9"))))
 '(lazy-highlight ((t (:background "#6e52b9" :foreground "#ffffff")))))
 ;; '(tab-bar-tab ((t (:background "#fffff" :foreground "#777777")))))
'(
 '(anzu-match-3 ((t (:foreground "#3B84CC"))))
 '(anzu-mode-line ((t (:foreground "#3B84CC"))))
 '(anzu-mode-line-no-match ((t (:foreground "#3B84CC"))))

 '(button ((t (:foreground "#777777" :underline nil))))
 '(comint-highlight-prompt ((t nil)))
 '(cursor ((t (:background "#5e4900"))))
 '(custom-documentation ((t (:foreground "#777777"))))
 '(diff-refine-added ((t (:background "#22aa22" :foreground "black"))))
 '(diff-refine-removed ((t (:background "red" :foreground "black"))))
 '(dired-directory ((t (:foreground "#3B84CC"))))
 '(dired-marked ((t (:foreground "#cd5c60"))))
 '(font-atomberg mixer grinderlock-builtin-face ((t (:foreground "#777777"))))
 '(font-lock-constant-face ((t (:foreground "#777777"))))
 '(font-lock-function-name-face ((t (:foreground "#777777"))))
 '(font-lock-string-face ((t (:foreground "#777777"))))
 '(font-lock-type-face ((t (:foreground "#777777"))))
 '(fringe ((t (:foreground "#888888"))))
 '(highlight ((t (:foreground "#999999" :background "#222222"))))
 '(isearch ((t (:foreground "#3B84CC"))))
 '(lazy-highlight ((t (:foreground "#3B84CC"))))
 '(line-number-current-line ((t (:foreground "#545c5e"))))
 '(link ((t (:foreground "#777777" :underline nil))))
 '(magit-branch-local ((t (:foreground "#999999"))))
 '(magit-branch-remote ((t (:foreground "#999999"))))
 '(magit-diff-added ((t (:foreground "#156a15"))))
 '(magit-diff-added-highlight ((t (:foreground "#156a15" :background nil))))
 '(magit-diff-context ((t (:foreground "#999999"))))
 '(magit-diff-context-highlight ((t nil)))
 '(magit-diff-file-heading ((t (:weight normal :foreground "#999999"))))
 '(magit-diff-file-heading-highlight ((t nil)))
 '(magit-diff-hunk-heading ((t (:foreground "#999999" :background nil))))
 '(magit-diff-hunk-heading-highlight ((t nil)))
 '(magit-diff-removed ((t (:foreground "#881b1b"))))
 '(magit-diff-removed-highlight ((t (:foreground "#881b1b" :background nil))))
 '(magit-diffstat-added ((t (:foreground "#156a15"))))
 '(magit-hash ((t :inherit magit-branch-upstream)))
 '(magit-log-author ((t (:foreground "#999999"))))
 '(magit-log-date ((t (:foreground "#999999"))))
 '(magit-section-heading ((t (:foreground "#999999" :bold t))))
 '(magit-section-highlight ((t nil)))
 '(minibuffer-prompt ((t (:foreground "#777777"))))
 '(mode-line ((t (:foreground "#999999"))))
 '(mode-line-buffer-id ((t (:foreground "#999999"))))
 '(mode-line-emphasis ((t (:foreground "#3B84CC"))))
 '(mode-line-highlight ((t (:foreground "#3B84CC"))))
 '(neo-dir-link-face ((t (:foreground "#999999"))))
 '(neo-expand-btn-face ((t (:foreground "#999999"))))
 '(neo-file-link-face ((t (:foreground "#999999"))))
 '(neo-root-dir-face ((t (:foreground "#999999"))))
 '(org-checkbox ((t (:foreground "#4b4f89"))))
 '(org-date ((t (:foreground "#4b4f89"))))
 '(org-done ((t (:foreground "#333333" :strike-through t))))
 '(org-headline-done ((t (:strike-through t :slant italic))))
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#999999" :height 1.1 :weight bold))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#999999" :height 1.0))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#999999" :height 1.0))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#999999" :height 1.0))))
 '(org-level-5 ((t (:inherit variable-pitch :foreground "#999999" :height 1.0))))
 '(org-level-6 ((t (:inherit variable-pitch :foreground "#999999" :height 1.0))))
 '(org-level-7 ((t (:inherit variable-pitch :foreground "#999999" :height 1.0))))
 '(org-level-8 ((t (:inherit variable-pitch :foreground "#999999" :height 1.0))))
 '(org-link ((t (:foreground "#839496"))))
 '(org-special-keyword ((t (:foreground "#4b4f89"))))
 '(org-todo ((t (:foreground "#6c71c4" :weight normal))))
 '(region ((t (:background "#185e7f" :foreground "#111111" :distant-foreground nil))))
 '(show-paren-match ((t (:foreground "#3B84CC"))))
 '(show-paren-mismatch ((t (:foreground "#3B84CC")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7533e1fc8345739ea0ace60330ebffdf9da46398490b4c36c7e48775e5621052"
     "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163"
     "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045"
     "f8067b7d0dbffb29a79e0843797efabdf5e1cf326639874d8b407e9b034136a4"
     "97965ccdac20cae22c5658c282544892959dc541af3e9ef8857dbf22eb70e82b"
     "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476"
     "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279"
     "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482"
     "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e"
     "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f"
     default))
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe
              org-rmail org-w3m org-velocity))
 '(package-selected-packages
   '(ace-jump-mode ample-theme anzu bm button-lock casual-dired
                   centaur-tabs chatgpt-shell company company-go
                   consult csv-mode diff-hl diminish dired-sidebar
                   doom-themes dracula-theme ef-themes elisp-slime-nav
                   embark embark-consult exec-path-from-shell
                   expand-region flycheck flycheck-golangci-lint
                   git-link go-eldoc go-mode gotest gruvbox-theme
                   hydra jetbrains-darcula-theme kaolin-themes
                   leuven-theme macrostep magit marginalia
                   markdown-mode multiple-cursors orderless org
                   org-bullets poet-theme projectile protobuf-mode
                   rainbow-mode region-bindings-mode rg rjsx-mode
                   ruby-end s smart-mode-line solarized-theme
                   spacemacs-theme string-inflection swiper treemacs
                   typescript-mode undo-tree use-package vertico
                   vertico-posframe visual-regexp vterm web-mode
                   which-key xterm-color yaml-mode yasnippet
                   zenburn-theme zig-mode))
 '(typescript-indent-level 2))

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
               (load compiled nil t t)))
          (t
           (if (file-exists-p compiled)
               (delete-file compiled))
           (if (null ignore-if-missing)
               (error "my/load: missing %s" source))))))

(defun my/load-all ()
  (setq my/loading-errors '())

  ;; Load packages and install them if necessary.
  (let* ((package--builtins '())
         (missing (remove-if 'package-installed-p package-selected-packages)))
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
         (all (list* gui non-gui this diminish base))
         (pkg-customizations
          (remove-if (lambda (el-file)
                       ;; remove autosaves
                       (or (string-prefix-p ".#" el-file)
                           (some (lambda (my) (string= (concat my ".el") el-file))
                                 all)))
                     (directory-files "." nil "\\.el$" t))))

    (my/load (if window-system gui non-gui))
    ;; Files that aren't on MELPA or any other package archive.
    (mapc 'my/load (directory-files "third-party" 'full "\\.el$" t))
    (mapc 'my/load base)
    (mapc 'my/load pkg-customizations)
    (my/load diminish)
    (my/load "~/.emacs.private.el" 'ignore-if-missing))

  (setq my-emacs-elapsed-time
        (float-time (time-subtract (current-time) my-emacs-start-time)))

  (recentf-open-files))

(my/load-all)
(put 'magit-diff-edit-hunk-commit 'disabled nil)
(put 'upcase-region 'disabled nil)
