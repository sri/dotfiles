(require 'go-mode)

(add-to-list 'exec-path (expand-file-name "~/go/bin"))

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
