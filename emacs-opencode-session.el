;;; emacs-opencode-session.el --- OpenCode session model  -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct (opencode-session (:constructor opencode-session-create))
  "Structured representation of an OpenCode session."
  id
  slug
  version
  project-id
  directory
  title
  time-created
  time-updated
  status
  summary
  diff
  info)

(provide 'emacs-opencode-session)

;;; emacs-opencode-session.el ends here
