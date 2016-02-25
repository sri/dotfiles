(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)

(add-hook 'web-mode-hook
          (lambda ()
            (when (string= web-mode-content-type "javascript")
              (web-mode-set-content-type "jsx"))
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-sql-indent-offset 2)
            (setq web-mode-code-indent-offset 2)))
