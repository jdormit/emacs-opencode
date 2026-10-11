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

(ert-deftest test-opencode-model/variant-keys-v2-objects ()
  "Extract variant names from V2 variant objects."
  (let ((result (opencode-session--variant-keys
                 '(((id . "low") (settings . ()))
                   ((id . "high") (settings . ()))
                   ((id . "off") (settings . ()) (disabled . t))))))
    (should (member "low" result))
    (should (member "high" result))
    (should-not (member "off" result))))

(ert-deftest test-opencode-model/variant-keys-v2-vector ()
  "Handle a vector of V2 variant objects."
  (let ((result (opencode-session--variant-keys
                 [((id . "low")) ((id . "high"))])))
    (should (equal result '("high" "low")))))

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

;;; provider-model-info

(ert-deftest test-opencode-model/provider-model-info-v2 ()
  "Find model metadata by provider and catalog id."
  (let ((conn (opencode-connection-create
               :models '(((id . "gpt-6-luna-fast") (modelID . "gpt-6-luna")
                          (providerID . "openai") (name . "Luna Fast"))))))
    (let ((info (opencode-session--provider-model-info
                 "openai" "gpt-6-luna-fast" conn)))
      (should (equal (alist-get 'modelID info) "gpt-6-luna")))))

(ert-deftest test-opencode-model/provider-model-info-v2-model-id-fallback ()
  "Match by modelID when the catalog id differs."
  (let ((conn (opencode-connection-create
               :models '(((id . "gpt-6-luna-fast") (modelID . "gpt-6-luna")
                          (providerID . "openai"))))))
    (should (opencode-session--provider-model-info "openai" "gpt-6-luna" conn))))

(ert-deftest test-opencode-model/provider-model-info-missing ()
  "Return nil when the model is not in the catalog."
  (let ((conn (opencode-connection-create :models nil)))
    (should-not (opencode-session--provider-model-info "openai" "nope" conn))))

;;; catalog-ready-p / ensure-providers

(ert-deftest test-opencode-model/catalog-ready-p ()
  "Report readiness only when all three caches are loaded."
  (should (opencode-session--catalog-ready-p
           (opencode-connection-create
            :providers '(((id . "openai")))
            :models '(((id . "m") (providerID . "openai")))
            :integrations '(((id . "openai"))))))
  (should-not (opencode-session--catalog-ready-p
               (opencode-connection-create
                :providers :loading
                :models '(((id . "m")))
                :integrations '(((id . "openai"))))))
  (should-not (opencode-session--catalog-ready-p (opencode-connection-create)))
  (should-not (opencode-session--catalog-ready-p nil)))

(ert-deftest test-opencode-model/ensure-providers-ready-is-noop ()
  "Do not reload the catalog or refresh headers once it is cached.

This guards against the header-render feedback loop where
`opencode-session--header-right' calls `--ensure-providers'."
  (let ((conn (opencode-connection-create
               :providers '(((id . "openai")))
               :models '(((id . "m") (providerID . "openai")))
               :integrations '(((id . "openai")))))
        (refreshed nil)
        (fetched nil))
    (cl-letf (((symbol-function 'opencode-session--refresh-headers)
               (lambda (&rest _args) (setq refreshed t)))
              ((symbol-function 'opencode-connection-ensure-providers)
               (lambda (&rest _args) (setq fetched t))))
      (opencode-session--ensure-providers conn))
    (should-not refreshed)
    (should-not fetched)))

(ert-deftest test-opencode-model/ensure-providers-loads-missing-catalog ()
  "Load the catalog and refresh headers when it is missing."
  (let ((conn (opencode-connection-create))
        (refreshed nil)
        (fetched nil))
    (cl-letf (((symbol-function 'opencode-connection-ensure-providers)
               (lambda (_conn &optional on-success)
                 (setq fetched t)
                 (when on-success (funcall on-success '(((id . "openai")))))))
              ((symbol-function 'opencode-connection-ensure-models)
               (lambda (_conn &optional on-success)
                 (when on-success (funcall on-success nil))))
              ((symbol-function 'opencode-connection-ensure-integrations)
               (lambda (_conn &optional on-success)
                 (when on-success (funcall on-success nil))))
              ((symbol-function 'opencode-session--refresh-headers)
               (lambda (&rest _args) (setq refreshed t))))
      (opencode-session--ensure-providers conn))
    (should fetched)
    (should refreshed)))

;;; connected-provider-ids

(ert-deftest test-opencode-model/connected-provider-ids ()
  "Report integrations that have connections as connected providers."
  (let ((conn (opencode-connection-create
               :integrations
               '(((id . "openai") (connections . (((type . "env") (name . "KEY")))))
                 ((id . "anthropic") (connections . []))
                 ((id . "empty"))))))
    (should (equal (opencode-session--connected-provider-ids conn)
                   '("openai")))))

;;; provider-model-candidates

(ert-deftest test-opencode-model/provider-model-candidates-v2 ()
  "Build candidates from the flat V2 model list."
  (let ((conn (opencode-connection-create
               :providers '(((id . "openai") (name . "OpenAI")))
               :models '(((id . "gpt-6") (providerID . "openai")
                          (name . "GPT 6") (status . "active"))
                         ((id . "old") (providerID . "openai")
                          (status . "deprecated")))
               :integrations
               '(((id . "openai")
                  (connections . (((type . "env") (name . "KEY")))))))))
    (let ((candidates (opencode-session--provider-model-candidates conn)))
      (should (= (length candidates) 1))
      (let ((candidate (car candidates)))
        (should (equal (plist-get candidate :provider-id) "openai"))
        (should (equal (plist-get candidate :provider-name) "OpenAI"))
        (should (equal (plist-get candidate :model-id) "gpt-6"))
        (should (equal (plist-get candidate :model-name) "GPT 6"))
        (should (plist-get candidate :connected-p))))))

;;; integration-id-for-provider

(ert-deftest test-opencode-model/integration-id-for-provider ()
  "Use the provider's integrationID when present."
  (let ((conn (opencode-connection-create
               :providers '(((id . "openai") (integrationID . "openai-codex"))))))
    (should (equal (opencode-session--integration-id-for-provider conn "openai")
                   "openai-codex"))))

(ert-deftest test-opencode-model/integration-id-for-provider-fallback ()
  "Fall back to the provider id when no integrationID is present."
  (let ((conn (opencode-connection-create :providers nil)))
    (should (equal (opencode-session--integration-id-for-provider conn "openai")
                   "openai"))))

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
  "Extract auth methods for a provider from its integration."
  (let ((data '(((id . "anthropic")
                 (methods . (((type . "oauth") (id . "browser")
                              (label . "OAuth"))))))))
    (let ((result (opencode-session--provider-auth-methods "anthropic" data)))
      (should (= (length result) 1))
      (should (equal (alist-get 'type (car result)) "oauth"))
      (should (equal (alist-get 'id (car result)) "browser")))))

(ert-deftest test-opencode-model/provider-auth-methods-key ()
  "Return key methods when the integration exposes them."
  (let ((data '(((id . "openai")
                 (methods . (((type . "key") (label . "API key"))))))))
    (let ((result (opencode-session--provider-auth-methods "openai" data)))
      (should (= (length result) 1))
      (should (equal (alist-get 'type (car result)) "key")))))

(ert-deftest test-opencode-model/provider-auth-methods-fallback ()
  "Fall back to an API key method when the integration is missing."
  (let ((result (opencode-session--provider-auth-methods "unknown" nil)))
    (should (= (length result) 1))
    (should (equal (alist-get 'type (car result)) "key"))))

;;; auth-method-label

(ert-deftest test-opencode-model/auth-method-label-explicit ()
  "Use an explicit label when present."
  (should (equal (opencode-session--auth-method-label
                  '((type . "oauth") (label . "Sign in with ChatGPT")))
                 "Sign in with ChatGPT")))

(ert-deftest test-opencode-model/auth-method-label-defaults ()
  "Fall back to a label derived from the method type."
  (should (equal (opencode-session--auth-method-label '((type . "key")))
                 "API key"))
  (should (equal (opencode-session--auth-method-label '((type . "oauth")))
                 "OAuth"))
  (should (equal (opencode-session--auth-method-label '((type . "env")))
                 "Environment variable")))

;;; run-auth-flow

(ert-deftest test-opencode-model/run-auth-flow-key ()
  "Dispatch key methods to the API key flow."
  (let ((called nil))
    (cl-letf (((symbol-function 'opencode-session--auth-api-key)
               (lambda (&rest _) (setq called 'key))))
      (opencode-session--run-auth-flow
       nil "openai" '(((type . "key"))) #'ignore)
      (should (eq called 'key)))))

(ert-deftest test-opencode-model/run-auth-flow-oauth ()
  "Dispatch OAuth methods to the OAuth flow with the method id."
  (let (captured)
    (cl-letf (((symbol-function 'opencode-session--auth-oauth)
               (lambda (_conn _provider method-id _callback)
                 (setq captured method-id))))
      (opencode-session--run-auth-flow
       nil "opencode" '(((type . "oauth") (id . "chatgpt-browser"))) #'ignore)
      (should (equal captured "chatgpt-browser")))))

(ert-deftest test-opencode-model/run-auth-flow-env ()
  "Environment methods do not start an interactive flow."
  (let ((called nil))
    (cl-letf (((symbol-function 'opencode-session--auth-api-key)
               (lambda (&rest _) (setq called 'key)))
              ((symbol-function 'opencode-session--auth-oauth)
               (lambda (&rest _) (setq called 'oauth))))
      (opencode-session--run-auth-flow
       nil "openai" '(((type . "env"))) #'ignore)
      (should-not called))))

;;; oauth-attempt-status

(ert-deftest test-opencode-model/oauth-attempt-status-string ()
  "Extract a string status."
  (should (equal (opencode-session--oauth-attempt-status '((status . "pending")))
                 "pending")))

(ert-deftest test-opencode-model/oauth-attempt-status-nil ()
  "Return nil for missing data."
  (should (null (opencode-session--oauth-attempt-status nil)))
  (should (null (opencode-session--oauth-attempt-status '((other . 1))))))

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
