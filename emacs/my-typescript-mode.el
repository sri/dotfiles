;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst my/typescript--treesit-sources
  '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
    (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
  "Tree-sitter grammar sources for TypeScript and TSX.")

(defconst my/typescript-imenu--container-headings
  '("Function" "Class" "Interface" "Type" "Enum" "Variable" "Method" "Namespace" "Module")
  "Synthetic container headings produced by TS imenu that should not prefix candidates.")

(defun my/typescript--treesit-ready-p ()
  (and (fboundp 'treesit-ready-p)
       (treesit-ready-p 'typescript)
       (treesit-ready-p 'tsx)))

(defun my/typescript-install-treesit-grammars ()
  "Install tree-sitter grammars for TypeScript and TSX."
  (interactive)
  (unless (fboundp 'treesit-install-language-grammar)
    (user-error "This Emacs build does not support tree-sitter grammar installation"))
  (dolist (src my/typescript--treesit-sources)
    (add-to-list 'treesit-language-source-alist src))
  (treesit-install-language-grammar 'typescript)
  (treesit-install-language-grammar 'tsx)
  (message "Installed tree-sitter grammars for TypeScript + TSX"))

(defun my/typescript-refresh-open-buffers ()
  "Re-run major mode selection for already-open TS/TSX buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name
                 (string-match-p "\\.\\(ts\\|tsx\\|mts\\|cts\\)\\'" buffer-file-name))
        (normal-mode)))))

(defun my/typescript--blank-name-p (name)
  "Return non-nil when NAME is only whitespace."
  (string-match-p "\\`[[:space:]]*\\'" name))

(defun my/typescript--flatten-imenu-index (index &optional scope)
  "Flatten TS imenu INDEX.
Yields names like `fn' or `Class.method'.
Optional SCOPE is the class/module prefix accumulated while recursing."
  (cl-mapcan
   (lambda (item)
     (cond
      ;; Skip synthetic rescan entries.
      ((and (consp item) (stringp (car item)) (string-prefix-p "*" (car item)))
       nil)

      ;; Recurse into sub-alists.
      ((imenu--subalist-p item)
       (let ((name (car item))
             (children (cdr item)))
         (cond
          ;; Ignore top-level type buckets (Function/Class/etc).
          ((member name my/typescript-imenu--container-headings)
           (my/typescript--flatten-imenu-index children scope))
          ;; Preserve meaningful nesting (e.g. class name).
          (t
           (my/typescript--flatten-imenu-index
            children
            (if scope (concat scope "." name) name))))))

      ;; Leaf candidate.
      ((and (consp item)
            (stringp (car item))
            (or (markerp (cdr item))
                (integer-or-marker-p (cdr item))))
       (let ((name (car item))
             (pos (cdr item)))
         (cond
          ;; TS class headers often use a blank child name for class start.
          ((my/typescript--blank-name-p name)
           (when scope (list (cons scope pos))))
          (t
           (list (cons (if scope (concat scope "." name) name) pos))))))

      (t nil)))
   index))

(defun my/typescript-imenu-flat ()
  "Jump to TS symbol from a flat completion list.
Candidates are shaped as `fn' or `Class.method'.
Bound to `C-c m' in TS buffers."
  (interactive)
  (let* ((index (imenu--make-index-alist t))
         (items (my/typescript--flatten-imenu-index index))
         (default (thing-at-point 'symbol t))
         (choice (completing-read "TS symbol: " (mapcar #'car items)
                                  nil t nil 'imenu--history-list default))
         (item (assoc choice items)))
    (unless item
      (user-error "No symbol selected"))
    (imenu item)))

(defun my/imenu ()
  "Jump with imenu, using a flat list in TypeScript buffers."
  (interactive)
  (if (derived-mode-p 'typescript-ts-mode 'tsx-ts-mode 'typescript-mode)
      (my/typescript-imenu-flat)
    (imenu)))

(defun my/typescript-setup ()
  "Prefer tree-sitter TypeScript modes when grammars are available."
  (if (my/typescript--treesit-ready-p)
      (progn
        (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
        (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
        (add-to-list 'auto-mode-alist '("\\.\\(ts\\|mts\\|cts\\)\\'" . typescript-ts-mode)))
    ;; Fallback when grammars are missing.
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
    (add-to-list 'auto-mode-alist '("\\.\\(ts\\|mts\\|cts\\)\\'" . typescript-mode))))

(my/typescript-setup)
