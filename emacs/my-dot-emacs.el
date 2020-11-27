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
      (helm-select-xfont)
    (let ((fonts '("JetBrains Mono"
                   "Inconsolata")))
      (seq-find (lambda (f)
                  (if (find-font (font-spec :name f))
                      f
                    (message "my/font: cannot find %s" f)
                    nil))
                fonts))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 110))))
 '(magit-diff-added ((t (:background nil :foreground "#5F7F5F"))))
 '(magit-diff-removed ((t (:background nil :foreground "#8C5353")))))

'(zzzzcustom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;
 ;; Inconsolata
 `(default ((t (:height 130 :family ,(my/font)  :foreground "#999999"))))
 '(anzu-match-3 ((t (:foreground "#3B84CC"))))
 '(anzu-mode-line ((t (:foreground "#3B84CC"))))
 '(anzu-mode-line-no-match ((t (:foreground "#3B84CC"))))
 '(bm-face ((t (:background "#422f73" :foreground "#111111"))))
 '(button ((t (:foreground "#777777" :underline nil))))
 '(comint-highlight-prompt ((t nil)))
 '(cursor ((t (:background "#5e4900"))))
 '(custom-documentation ((t (:foreground "#777777"))))
 '(diff-refine-added ((t (:background "#22aa22" :foreground "black"))))
 '(diff-refine-removed ((t (:background "red" :foreground "black"))))
 '(dired-directory ((t (:foreground "#3B84CC"))))
 '(dired-marked ((t (:foreground "#cd5c60"))))
 '(font-lock-builtin-face ((t (:foreground "#777777"))))
 '(font-lock-constant-face ((t (:foreground "#777777"))))
 '(font-lock-function-name-face ((t (:foreground "#777777"))))
 '(font-lock-string-face ((t (:foreground "#777777"))))
 '(font-lock-type-face ((t (:foreground "#777777"))))
 '(fringe ((t (:foreground "#888888"))))
 '(helm-buffer-directory ((t (:foreground "#777777"))))
 '(helm-buffer-file ((t (:foreground "#777777"))))
 '(helm-buffer-process ((t (:foreground "#777777"))))
 '(helm-buffer-size ((t (:foreground "#777777"))))
 '(helm-ff-directory ((t (:foreground "#777777"))))
 '(helm-ff-dirs ((t (:foreground "#777777"))))
 '(helm-ff-file ((t (:foreground "#777777"))))
 '(helm-header ((t (:foreground "#777777"))))
 '(helm-ls-git-added-copied-face ((t (:foreground "#777777"))))
 '(helm-ls-git-added-modified-face ((t (:foreground "#777777"))))
 '(helm-ls-git-conflict-face ((t (:foreground "#777777"))))
 '(helm-ls-git-deleted-and-staged-face ((t (:foreground "#777777"))))
 '(helm-ls-git-deleted-not-staged-face ((t (:foreground "#777777"))))
 '(helm-ls-git-modified-and-staged-face ((t (:foreground "#777777"))))
 '(helm-ls-git-modified-not-staged-face ((t (:foreground "#777777"))))
 '(helm-ls-git-renamed-modified-face ((t (:foreground "#777777"))))
 '(helm-ls-git-untracked-face ((t (:foreground "#777777"))))
 '(helm-match ((t (:foreground "#3B84CC"))))
 '(helm-selection ((t (:foreground "#3B84CC"))))
 '(helm-selection-line ((t (:foreground "#3B84CC"))))
 '(helm-source-header ((t (:foreground "#777777"))))
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
   (quote
    ("c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "f8067b7d0dbffb29a79e0843797efabdf5e1cf326639874d8b407e9b034136a4" "97965ccdac20cae22c5658c282544892959dc541af3e9ef8857dbf22eb70e82b" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-velocity)))
 '(package-selected-packages
   (quote
    (ivy-rich helm-posframe ivy-posframe which-key rg expand-region xterm-color go-eldoc git-link company-go company flycheck-golangci-lint gotest exec-path-from-shell treemacs helm-projectile markdown-mode csv-mode swiper counsel ivy poet-theme gruvbox-theme dracula-theme kaolin-themes centaur-tabs typescript-mode protobuf-mode s rjsx-mode ample-theme leuven-theme neotree use-package yaml-mode diminish anzu region-bindings-mode hydra multiple-cursors undo-tree powerline spacemacs-theme zenburn-theme org yasnippet web-mode visual-regexp solarized-theme smart-mode-line ruby-end rainbow-mode projectile org-bullets magit macrostep helm helm-ls-git go-mode flycheck elisp-slime-nav bm ace-jump-mode)))
 '(typescript-indent-level 2))

;; Load the byte-compiled version of file.
(defun my/load (arg &optional ignore-if-missing)
  (let* ((source (expand-file-name
                  (if (string-suffix-p ".el" arg) arg (concat arg ".el"))))
         (compiled (concat source "c")))
    (cond ((file-exists-p source)
           (if (file-newer-than-file-p source compiled)
               (let ((byte-compile-verbose nil))
                 (if (null (byte-compile-file source))
                     (error "my/load: ERROR byte compiling file %s" source))))
           (load compiled nil t t))
          (t
           (if (file-exists-p compiled)
               (delete-file compiled))
           (if (null ignore-if-missing)
               (error "my/load: missing %s" source))))))


(defun my/load-all ()
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
                 "my-update-dot-emacs"
                 "my-modeline"))
         (all (list* gui non-gui this diminish base))
         (pkg-customizations
          (remove-if (lambda (el-file)
                       ;; remove autosaves
                       (or (string-prefix-p ".#" el-file)
                           (some (lambda (my) (string-prefix-p my el-file))
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
