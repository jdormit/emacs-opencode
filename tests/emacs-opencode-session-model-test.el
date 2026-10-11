;;; emacs-opencode-session-model-test.el --- Tests for model selection  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-opencode-session-model)
(require 'emacs-opencode-session)

;;; normalize-agents

(ert-deftest test-opencode-model/normalize-agents-strings ()
  "Normalize a list of string agents."
  (should (equal (opencode-session--normalize-agents '("plan" "code"))
                 '("plan" "code"))))

(ert-deftest test-opencode-model/normalize-agents-vector ()
  "Normalize a vector of agents."
  (should (equal (opencode-session--normalize-agents ["plan" "code"])
                 '("plan" "code"))))

(ert-deftest test-opencode-model/normalize-agents-alists ()
  "Normalize alist agents with mode and hidden."
  (let ((data '(((id . "plan") (mode . "primary") (hidden . nil))
                ((id . "code") (mode . "primary") (hidden . nil))
                ((id . "hidden") (mode . "primary") (hidden . t))
                ((id . "secondary") (mode . "secondary") (hidden . nil)))))
    (let ((result (opencode-session--normalize-agents data)))
      (should (member "plan" result))
      (should (member "code" result))
      (should-not (member "hidden" result))
      (should-not (member "secondary" result)))))

(ert-deftest test-opencode-model/normalize-agents-nil ()
  "Return nil for nil."
  (should (null (opencode-session--normalize-agents nil))))

(ert-deftest test-opencode-model/normalize-agents-uses-name-fallback ()
  "Fall back to name when id is absent."
  (let ((data '(((name . "plan") (mode . "primary")))))
    (should (equal (opencode-session--normalize-agents data) '("plan")))))

;;; variant-keys

(ert-deftest test-opencode-model/variant-keys-alist ()
  "Extract variant names from alist."
  (let ((result (opencode-session--variant-keys
                 '((fast . nil)
                   (slow . nil)
                   (disabled . ((disabled . t)))))))
    (should (member "fast" result))
    (should (member "slow" result))
    (should-not (member "disabled" result))))

(ert-deftest test-opencode-model/variant-keys-hash-table ()
  "Extract variant names from hash table."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "fast" nil ht)
    (puthash "slow" nil ht)
    (puthash "off" '((disabled . t)) ht)
    (let ((result (opencode-session--variant-keys ht)))
      (should (member "fast" result))
      (should (member "slow" result))
      (should-not (member "off" result)))))

(ert-deftest test-opencode-model/variant-keys-sorted ()
  "Variant keys are sorted alphabetically."
  (let ((result (opencode-session--variant-keys
                 '((zebra . nil) (alpha . nil) (middle . nil)))))
    (should (equal result '("alpha" "middle" "zebra")))))

(ert-deftest test-opencode-model/variant-keys-nil ()
  "Return nil for nil."
  (should (null (opencode-session--variant-keys nil))))

(ert-deftest test-opencode-model/variant-keys-symbol-keys ()
  "Handle symbol keys in alist."
  (let ((result (opencode-session--variant-keys
                 '((fast . nil) (slow . nil)))))
    (should (member "fast" result))
    (should (member "slow" result))))

;;; model-candidate-tier

(ert-deftest test-opencode-model/candidate-tier-recent ()
  "Recently selected models are tier 0."
  (let ((candidate (list :provider-id "anthropic" :model-id "claude-3" :connected-p nil)))
    (should (= (opencode-session--model-candidate-tier
                candidate
                '(("anthropic" . "claude-3"))
                nil)
               0))))

(ert-deftest test-opencode-model/candidate-tier-session ()
  "Session-used models are tier 1."
  (let ((candidate (list :provider-id "openai" :model-id "gpt-4" :connected-p nil)))
    (should (= (opencode-session--model-candidate-tier
                candidate
                nil
                '(("openai" . "gpt-4")))
               1))))

(ert-deftest test-opencode-model/candidate-tier-connected ()
  "Connected models are tier 2."
  (let ((candidate (list :provider-id "openai" :model-id "gpt-4" :connected-p t)))
    (should (= (opencode-session--model-candidate-tier candidate nil nil)
               2))))

(ert-deftest test-opencode-model/candidate-tier-other ()
  "Other models are tier 3."
  (let ((candidate (list :provider-id "openai" :model-id "gpt-4" :connected-p nil)))
    (should (= (opencode-session--model-candidate-tier candidate nil nil)
               3))))

;;; model-candidate-rank

(ert-deftest test-opencode-model/candidate-rank-in-list ()
  "Rank from position in ordered list."
  (let ((candidate (list :provider-id "b" :model-id "2"))
        (ranked '(("a" . "1") ("b" . "2") ("c" . "3"))))
    (should (= (opencode-session--model-candidate-rank candidate 0 ranked) 1))))

(ert-deftest test-opencode-model/candidate-rank-not-in-list ()
  "Default rank 0 when not in list."
  (let ((candidate (list :provider-id "x" :model-id "y")))
    (should (= (opencode-session--model-candidate-rank candidate 0 nil) 0))))

(ert-deftest test-opencode-model/candidate-rank-high-tier ()
  "Tier > 1 always returns rank 0."
  (let ((candidate (list :provider-id "x" :model-id "y")))
    (should (= (opencode-session--model-candidate-rank candidate 2 nil) 0))))

;;; provider-model-items

(ert-deftest test-opencode-model/provider-model-items-list ()
  "Extract model items from a list."
  (let ((provider '((id . "anthropic")
                    (models . (("claude-3" . ((name . "Claude 3")))
                               ("claude-2" . ((name . "Claude 2"))))))))
    (let ((items (opencode-session--provider-model-items provider)))
      (should (= (length items) 2))
      (should (equal (caar items) "claude-3")))))

(ert-deftest test-opencode-model/provider-model-items-hash-table ()
  "Extract model items from a hash table."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "claude-3" '((name . "Claude 3")) ht)
    (let ((provider `((id . "anthropic") (models . ,ht))))
      (let ((items (opencode-session--provider-model-items provider)))
        (should (= (length items) 1))
        (should (equal (caar items) "claude-3"))))))

(ert-deftest test-opencode-model/provider-model-items-nil ()
  "Return nil for nil models."
  (should (null (opencode-session--provider-model-items '((id . "x"))))))

;;; provider-model-candidate-display

(ert-deftest test-opencode-model/candidate-display-connected ()
  "Display connected indicator."
  (should (equal (opencode-session--provider-model-candidate-display
                  "anthropic" "claude-3" t)
                 "anthropic/claude-3 (connected)")))

(ert-deftest test-opencode-model/candidate-display-not-connected ()
  "Display without connected indicator."
  (should (equal (opencode-session--provider-model-candidate-display
                  "anthropic" "claude-3" nil)
                 "anthropic/claude-3")))

;;; completable-agent-names

(ert-deftest test-opencode-model/completable-agent-names-subagents ()
  "Return non-hidden, non-primary agents."
  (let ((data '(((id . "build") (mode . "primary") (hidden . nil))
                ((id . "plan") (mode . "primary") (hidden . nil))
                ((id . "general") (mode . "subagent") (hidden . nil))
                ((id . "explore") (mode . "subagent") (hidden . nil))
                ((id . "compaction") (mode . "primary") (hidden . t))
                ((id . "title") (mode . "primary") (hidden . t)))))
    (let ((result (opencode-session--completable-agent-names data)))
      (should (member "general" result))
      (should (member "explore" result))
      (should-not (member "build" result))
      (should-not (member "plan" result))
      (should-not (member "compaction" result))
      (should-not (member "title" result)))))

(ert-deftest test-opencode-model/completable-agent-names-nil ()
  "Return nil for nil data."
  (should (null (opencode-session--completable-agent-names nil))))

(ert-deftest test-opencode-model/completable-agent-names-vector ()
  "Handle vector input."
  (let ((data [((id . "explore") (mode . "subagent") (hidden . nil))]))
    (should (equal (opencode-session--completable-agent-names data)
                   '("explore")))))

(ert-deftest test-opencode-model/completable-agent-names-all-mode ()
  "Include agents with mode \"all\" that are not hidden."
  (let ((data '(((id . "multi") (mode . "all") (hidden . nil)))))
    (should (equal (opencode-session--completable-agent-names data)
                   '("multi")))))

;;; agent-name

(ert-deftest test-opencode-model/agent-name-string ()
  "Return string agent as-is."
  (should (equal (opencode-session--agent-name "plan") "plan")))

(ert-deftest test-opencode-model/agent-name-alist-with-id ()
  "Extract id from alist agent."
  (should (equal (opencode-session--agent-name '((id . "explore") (name . "Explorer")))
                 "explore")))

(ert-deftest test-opencode-model/agent-name-alist-name-fallback ()
  "Fall back to name when id is absent."
  (should (equal (opencode-session--agent-name '((name . "explore")))
                 "explore")))

(ert-deftest test-opencode-model/agent-name-nil ()
  "Return nil for nil."
  (should (null (opencode-session--agent-name nil))))

;;; provider-auth-methods

(ert-deftest test-opencode-model/provider-auth-methods ()
  "Extract auth methods for a provider."
  (let ((data `((anthropic . [((type . "api") (label . "API key"))]))))
    (let ((result (opencode-session--provider-auth-methods "anthropic" data)))
      (should (= (length result) 1))
      (should (equal (alist-get 'type (car result)) "api")))))

(ert-deftest test-opencode-model/provider-auth-methods-fallback ()
  "Fall back to API key when provider not found."
  (let ((result (opencode-session--provider-auth-methods "unknown" nil)))
    (should (= (length result) 1))
    (should (equal (alist-get 'type (car result)) "api"))))

;;; push-agent / push-model

(ert-deftest test-opencode-model/push-agent ()
  "Push the selected agent to the connected server."
  (with-temp-buffer
    (setq-local opencode-session--connection
                (opencode-connection-create :base-url "http://127.0.0.1:4096"))
    (setq-local opencode-session--session (opencode-session-create :id "ses_1"))
    (let (captured)
      (cl-letf (((symbol-function 'opencode-client-session-set-agent)
                 (lambda (_conn session-id agent &rest _args)
                   (setq captured (list session-id agent)))))
        (opencode-session--push-agent "build"))
      (should (equal captured '("ses_1" "build"))))))

(ert-deftest test-opencode-model/push-agent-without-session ()
  "Do not push an agent when no session is connected."
  (with-temp-buffer
    (setq-local opencode-session--connection
                (opencode-connection-create :base-url "http://127.0.0.1:4096"))
    (let (called)
      (cl-letf (((symbol-function 'opencode-client-session-set-agent)
                 (lambda (&rest _args) (setq called t))))
        (opencode-session--push-agent "build"))
      (should-not called))))

(ert-deftest test-opencode-model/push-model-includes-variant ()
  "Push the active model and variant to the connected server."
  (with-temp-buffer
    (setq-local opencode-session--connection
                (opencode-connection-create :base-url "http://127.0.0.1:4096"))
    (setq-local opencode-session--session (opencode-session-create :id "ses_1"))
    (setq-local opencode-session--provider-id "anthropic")
    (setq-local opencode-session--model-id "claude")
    (setq-local opencode-session--variant "thinking")
    (let (captured)
      (cl-letf (((symbol-function 'opencode-client-session-set-model)
                 (lambda (_conn session-id model &rest args)
                   (setq captured (list session-id model
                                        (plist-get args :variant))))))
        (opencode-session--push-model))
      (should (equal captured '("ses_1" ("anthropic" . "claude") "thinking"))))))

(ert-deftest test-opencode-model/push-model-without-model ()
  "Do not push a model when none is selected."
  (with-temp-buffer
    (setq-local opencode-session--connection
                (opencode-connection-create :base-url "http://127.0.0.1:4096"))
    (setq-local opencode-session--session (opencode-session-create :id "ses_1"))
    (let (called)
      (cl-letf (((symbol-function 'opencode-client-session-set-model)
                 (lambda (&rest _args) (setq called t))))
        (opencode-session--push-model))
      (should-not called))))

(provide 'emacs-opencode-session-model-test)

;;; emacs-opencode-session-model-test.el ends here
