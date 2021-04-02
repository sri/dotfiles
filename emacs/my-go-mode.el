(require 'go-mode)
(require 'company)
(require 'company-go)

(let ((path "/usr/local/go/bin"))
  (cond ((file-exists-p path)
         (push path exec-path)
         (setenv "PATH" (concat path ":" (getenv "PATH"))))
        (t
         (message "my-go-mode: path does not exist %s" path))))

(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 2)
            (setq indent-tabs-mode 1)
            (local-set-key (kbd "M-.") #'godef-jump)
            ;(company-mode)
            '(set (make-local-variable 'company-backends) '(company-go))
            '(go-eldoc-setup)
            '(flycheck-mode 1)))


(let ((cmd "goimports"))
  ;; Install: go get golang.org/x/tools/cmd/goimports
  (if (executable-find cmd)
      (setq gofmt-command "goimports")
    (message "unable to find %s" cmd)))
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup)
