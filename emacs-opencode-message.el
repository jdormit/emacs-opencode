;;; emacs-opencode-message.el --- OpenCode message model  -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct (opencode-message-part (:constructor opencode-message-part-create))
  "Structured representation of an OpenCode message part."
  id
  session-id
  message-id
  type
  text
  metadata
  time-start
  time-end
  snapshot
  reason
  cost
  tokens
  tool
  state)

(cl-defstruct (opencode-message (:constructor opencode-message-create))
  "Structured representation of an OpenCode message."
  id
  session-id
  role
  parent-id
  model-id
  provider-id
  mode
  agent
  path
  time-created
  time-completed
  finish
  summary
  parts
  text
  start-marker
  end-marker
  info)

(provide 'emacs-opencode-message)

;;; emacs-opencode-message.el ends here
