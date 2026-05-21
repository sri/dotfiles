(use-package shell-command+
  :ensure t
  :bind
  (("M-!" . shell-command+)
   :map dired-mode-map
   ("M-!" . shell-command+)))
