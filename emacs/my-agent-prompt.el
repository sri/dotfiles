;; -*- lexical-binding: t; -*-

(require 'server)

(defvar-local my/agent-prompt-action-file nil
  "Sidecar file used to tell the agent-editor wrapper what to do.")

(defvar my/agent-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s") #'my/agent-prompt-submit)
    (define-key map (kbd "C-c C-c") #'my/agent-prompt-submit)
    (define-key map (kbd "C-c C-e") #'my/agent-prompt-return)
    (define-key map (kbd "C-c C-k") #'my/agent-prompt-cancel)
    map)
  "Keymap for `my/agent-prompt-mode'.")

(define-minor-mode my/agent-prompt-mode
  "Edit a Pi or Claude prompt opened by `,agent-editor'."
  :lighter " Agent-Prompt"
  :keymap my/agent-prompt-mode-map
  (when my/agent-prompt-mode
    (setq-local header-line-format
                " Agent prompt: C-s send  C-c C-e return without sending  C-c C-k cancel")))

(defun my/agent-prompt--finish (action)
  "Finish the current agent prompt using ACTION."
  (unless (and my/agent-prompt-mode
               my/agent-prompt-action-file
               (file-exists-p my/agent-prompt-action-file))
    (user-error "This is not an agent prompt buffer"))
  (when (string= action "cancel")
    (erase-buffer))
  (save-buffer)
  (with-temp-file my/agent-prompt-action-file
    (insert action "\n"))
  (server-edit))

(defun my/agent-prompt-submit ()
  "Save the prompt, return to its agent pane, and submit it."
  (interactive)
  (my/agent-prompt--finish "submit"))

(defun my/agent-prompt-return ()
  "Save the prompt and return it to the agent without submitting."
  (interactive)
  (my/agent-prompt--finish "edit"))

(defun my/agent-prompt-cancel ()
  "Discard the prompt and return to an empty agent input."
  (interactive)
  (my/agent-prompt--finish "cancel"))

(defun my/save-or-send-agent-prompt ()
  "Submit an agent prompt, or save an ordinary buffer."
  (interactive)
  (if (bound-and-true-p my/agent-prompt-mode)
      (my/agent-prompt-submit)
    (save-buffer)))

(defun my/agent-prompt-maybe-enable ()
  "Enable `my/agent-prompt-mode' for files opened by `,agent-editor'."
  (when-let* ((file buffer-file-name)
              (action-file (concat file ".agent-editor-action"))
              ((file-exists-p action-file)))
    (setq-local my/agent-prompt-action-file action-file)
    (my/agent-prompt-mode 1)))

(add-hook 'server-visit-hook #'my/agent-prompt-maybe-enable)
