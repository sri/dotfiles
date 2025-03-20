(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     "18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3"
     "7533e1fc8345739ea0ace60330ebffdf9da46398490b4c36c7e48775e5621052"
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
   '(ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail
             ol-w3m))
 '(package-selected-packages
   '(ace-jump-mode bm button-lock casual
                   company company-go consult csv-mode
                   diff-hl diminish doom-themes
                   ef-themes elisp-slime-nav embark
                   embark-consult exec-path-from-shell expand-region
                   flycheck flycheck-golangci-lint git-link go-eldoc
                   go-mode gotest gruvbox-theme howm hydra
                   jetbrains-darcula-theme leuven-theme
                   macrostep magit marginalia markdown-mode
                   multiple-cursors orderless org org-bullets
                   projectile protobuf-mode
                   rainbow-mode region-bindings-mode rg rjsx-mode
                   ruby-end s smart-mode-line solarized-theme
                   spacemacs-theme string-inflection
                   typescript-mode undo-tree use-package vertico
                   vertico-posframe visual-regexp web-mode
                   which-key xterm-color yaml-mode yasnippet
                   zenburn-theme zig-mode))
 '(typescript-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 170 :family "JetBrains Mono")))))

(let ((skip-customizations
       (equal ?y
              (ignore-errors
                ;; Mouse clicks throws an error.
                (read-char "Skip my customizations?" nil 2.0)))))
  (if skip-customizations
      (dired "~/my/dotfiles/emacs")
    (load "~/my/dotfiles/emacs/my-dot-emacs-2")))
