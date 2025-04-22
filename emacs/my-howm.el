;; -*- lexical-binding: t; -*-
;; Notes:
;; https://leahneukirchen.org/blog/archive/2022/03/note-taking-in-emacs-with-howm.html
;; https://news.ycombinator.com/item?id=41438107

(use-package howm
      :after org
      :init
      (setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.org")
      (setq howm-view-title-header "*")

      ;; Use ripgrep for fast searching.
      (setq howm-view-use-grep t)
      (setq howm-view-grep-command "rg")
      (setq howm-view-grep-option "-nH --no-heading --color never")
      (setq howm-view-grep-extended-option nil)
      (setq howm-view-grep-fixed-option "-F")
      (setq howm-view-grep-expr-option nil)
      (setq howm-view-grep-file-stdin-option nil)

      (setq howm-template "%date %file\n* %title%cursor")

      (setq howm-keyword-case-fold-search t)
      (setq howm-view-summary-sep "|"))
