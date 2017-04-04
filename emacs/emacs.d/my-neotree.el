(defun my-neotree (&optional arg)
  (interactive "P")
  (if arg
      (neotree-find)
    (neotree-toggle)))
