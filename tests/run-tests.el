;;; run-tests.el --- Load all OpenCode test files  -*- lexical-binding: t; -*-

;; Load all test modules so `ert-run-tests-batch-and-exit' finds them.

(require 'ert)

(require 'emacs-opencode-test)
(require 'emacs-opencode-connection-test)
(require 'emacs-opencode-client-test)
(require 'emacs-opencode-message-test)
(require 'emacs-opencode-session-test)
(require 'emacs-opencode-session-vars-test)
(require 'emacs-opencode-session-mode-test)
(require 'emacs-opencode-session-render-test)
(require 'emacs-opencode-session-handlers-test)
(require 'emacs-opencode-session-header-test)
(require 'emacs-opencode-session-fontify-test)
(require 'emacs-opencode-session-model-test)
(require 'emacs-opencode-acp-test)

(provide 'run-tests)
;;; run-tests.el ends here
