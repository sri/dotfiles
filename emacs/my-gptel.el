(require 'gptel)

(defun my/claude-api-key ()
  (auth-source-pick-first-password :host "api.anthropic.com"))

(setq gptel-model "claude-3-7-sonnet")
(setq gptel-backend
      (gptel-make-anthropic "Claude" :stream t :key #'my/claude-api-key))
