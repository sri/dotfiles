;; -*- lexical-binding: t; -*-

(with-eval-after-load 'pi-coding-agent
  ;; RET sends prompt; S-RET inserts newline in input mode.
  (bind-keys :map pi-coding-agent-input-mode-map
             ("RET" . pi-coding-agent-send)
             ("<return>" . pi-coding-agent-send)
             ("<S-return>" . newline)))
