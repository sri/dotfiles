;; -*- lexical-binding: t; -*-


(defconst my/typescript--treesit-sources
  '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
    (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
  "Tree-sitter grammar sources for TypeScript and TSX.")

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
