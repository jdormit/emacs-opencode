;;; emacs-opencode-session-model-test.el --- Tests for model selection  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-opencode-session-mode)
(require 'emacs-opencode-session-model)

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
                ((id . "both") (mode . "all") (hidden . nil))
                ((id . "hidden") (mode . "primary") (hidden . t))
                ((id . "secondary") (mode . "subagent") (hidden . nil)))))
    (should (equal (opencode-session--normalize-agents data) '("plan" "code" "both")))))

(ert-deftest test-opencode-model/normalize-agents-nil ()
  "Return nil for nil."
  (should (null (opencode-session--normalize-agents nil))))

(ert-deftest test-opencode-model/normalize-agents-uses-name-fallback ()
  "Fall back to name when id is absent."
  (let ((data '(((name . "plan") (mode . "primary")))))
    (should (equal (opencode-session--normalize-agents data) '("plan")))))

;;; variant-keys

(ert-deftest test-opencode-model/variant-keys-in-server-order ()
  "Variant names come from the variant list, in server order."
  (should (equal (opencode-session--variant-keys
                  '(((id . "low") (settings . ((reasoningEffort . "low"))))
                    ((id . "high"))
                    ((id . "low"))))
                 '("low" "high"))))

(ert-deftest test-opencode-model/variant-keys-vector ()
  "Variant vectors are accepted."
  (should (equal (opencode-session--variant-keys [((id . "max"))]) '("max"))))

(ert-deftest test-opencode-model/variant-keys-nil ()
  "Return nil for nil."
  (should (null (opencode-session--variant-keys nil))))

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

;;; model catalog

(defun opencode-model-test--connection ()
  "Return a connection with a small model and integration catalog."
  (opencode-connection-create
   :models '(((id . "gpt") (providerID . "openai") (name . "GPT") (status . "active")
              (enabled . t) (variants . (((id . "low")) ((id . "high"))))
              (limit . ((context . 1000))))
             ((id . "old") (providerID . "openai") (name . "Old") (status . "deprecated")
              (enabled . t))
             ((id . "off") (providerID . "openai") (name . "Off") (status . "active")
              (enabled . nil)))
   :integrations '(((id . "openai") (name . "OpenAI")
                    (methods . (((type . "key")) ((type . "env") (names . ("OPENAI_API_KEY")))))
                    (connections . (((type . "env")))))
                   ((id . "anthropic") (name . "Anthropic")
                    (methods . (((type . "key")))) (connections . nil))
                   ((id . "envonly") (name . "Env Only")
                    (methods . (((type . "env") (names . ("X"))))) (connections . nil)))))

(ert-deftest test-opencode-model/provider-model-info ()
  "Model metadata is found by provider and model ID."
  (let ((conn (opencode-model-test--connection)))
    (should (equal (alist-get 'name (opencode-session--provider-model-info "openai" "gpt" conn))
                   "GPT"))
    (should-not (opencode-session--provider-model-info "openai" "nope" conn))))

(ert-deftest test-opencode-model/candidates-list-models-and-connectable-providers ()
  "Candidates list usable models plus providers that can be connected."
  (let* ((opencode-session--recent-models nil)
         (conn (opencode-model-test--connection))
         (displays (mapcar (lambda (candidate) (plist-get candidate :display))
                           (opencode-session--provider-model-candidates conn))))
    (should (equal displays '("openai/gpt" "anthropic/* (connect)")))))

(ert-deftest test-opencode-model/completion-data-for-one-provider ()
  "Completion can be limited to one connected provider."
  (let* ((opencode-session--recent-models nil)
         (data (opencode-session--provider-model-completion-data
                (opencode-model-test--connection) "openai")))
    (should (equal (car data) '("openai/gpt")))))

(ert-deftest test-opencode-model/candidate-display ()
  "Unconnected providers display as connect actions."
  (should (equal (opencode-session--provider-model-candidate-display "openai" "gpt" t)
                 "openai/gpt"))
  (should (equal (opencode-session--provider-model-candidate-display "anthropic" "*" nil)
                 "anthropic/* (connect)")))

(ert-deftest test-opencode-model/variant-for-model ()
  "Switching models keeps an available variant, else uses the default."
  (with-temp-buffer
    (setq-local opencode-session--connection (opencode-model-test--connection))
    (setq-local opencode-session--variant "high")
    (should (equal (opencode-session--variant-for-model "openai" "gpt") "high"))
    (setq-local opencode-session--variant "xhigh")
    (let ((opencode-session-default-variant "low"))
      (should (equal (opencode-session--variant-for-model "openai" "gpt") "low")))
    (let ((opencode-session-default-variant nil))
      (should-not (opencode-session--variant-for-model "openai" "gpt")))))

(ert-deftest test-opencode-model/switch-model-sends-reference ()
  "Model switches post a reference with the variant and update the buffer."
  (with-temp-buffer
    (opencode-session-mode)
    (setq-local opencode-session--connection (opencode-model-test--connection))
    (setq-local opencode-session--session (opencode-session-create :id "s1"))
    (let (sent)
      (cl-letf (((symbol-function 'opencode-client-session-switch-model)
                 (lambda (_conn session-id model &rest args)
                   (setq sent (list session-id model))
                   (funcall (plist-get args :success))))
                ((symbol-function 'message) #'ignore))
        (should-error (opencode-session--set-variant nil nil))
        (should-not sent)
        (setq-local opencode-session--provider-id "openai")
        (setq-local opencode-session--model-id "gpt")
        (opencode-session--set-variant "high" 1))
      (should (equal sent '("s1" ((providerID . "openai") (id . "gpt") (variant . "high")))))
      (should (equal opencode-session--variant "high"))
      (should (= opencode-session--variant-index 1)))))

(ert-deftest test-opencode-model/set-agent-switches-server-agent ()
  "Selecting an agent switches the session's agent on the server."
  (with-temp-buffer
    (opencode-session-mode)
    (setq-local opencode-session--connection
                (opencode-connection-create :agents '("build" "plan")))
    (setq-local opencode-session--session (opencode-session-create :id "s1"))
    (let (sent)
      (cl-letf (((symbol-function 'opencode-client-session-switch-agent)
                 (lambda (_conn session-id agent &rest args)
                   (setq sent (list session-id agent))
                   (funcall (plist-get args :success))))
                ((symbol-function 'message) #'ignore))
        (opencode-session--set-agent "plan" 1))
      (should (equal sent '("s1" "plan")))
      (should (equal opencode-session--agent "plan"))
      (should (= opencode-session--agent-index 1)))))

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

;;; auth methods

(ert-deftest test-opencode-model/usable-auth-methods-skip-env ()
  "Only key and OAuth methods are offered."
  (should (equal (opencode-session--usable-auth-methods
                  '((methods . (((type . "env") (names . ("X")))
                                ((type . "key"))
                                ((type . "command") (id . "cli") (label . "CLI"))
                                ((type . "oauth") (id . "browser") (label . "Browser"))))))
                 '(((type . "key")) ((type . "oauth") (id . "browser") (label . "Browser"))))))

(ert-deftest test-opencode-model/auth-method-labels ()
  "Methods are labelled by their label or type."
  (should (equal (opencode-session--auth-method-label '((type . "key"))) "API key"))
  (should (equal (opencode-session--auth-method-label '((type . "oauth") (label . "Sign in")))
                 "Sign in")))

(ert-deftest test-opencode-model/api-key-flow-connects-and-refreshes ()
  "The API key flow stores the key and refreshes models before the callback."
  (let ((conn (opencode-model-test--connection))
        sent called)
    (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "sk-1"))
              ((symbol-function 'opencode-client-integration-connect-key)
               (lambda (_conn id key &rest args)
                 (setq sent (list id key (plist-get args :answer)))
                 (funcall (plist-get args :success))))
              ((symbol-function 'opencode-session--refresh-providers)
               (lambda (_conn callback) (funcall callback nil)))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (setq-local opencode-session--connection conn)
        (opencode-session--connect-provider "anthropic" (lambda () (setq called t)))))
    (should (equal sent '("anthropic" "sk-1" nil)))
    (should called)))

(ert-deftest test-opencode-model/oauth-flow-polls-until-complete ()
  "Browser OAuth polls the attempt until it completes."
  (let ((conn (opencode-model-test--connection))
        (statuses (list "pending" "complete"))
        browsed called)
    (cl-letf (((symbol-function 'opencode-client-integration-oauth-connect)
               (lambda (_conn _id method-id &rest args)
                 (should (equal method-id "browser"))
                 (funcall (plist-get args :success)
                          :data '((attemptID . "con_1") (url . "https://x") (mode . "auto")))))
              ((symbol-function 'opencode-client-integration-oauth-status)
               (lambda (_conn _id _attempt &rest args)
                 (funcall (plist-get args :success)
                          :data `((status . ,(pop statuses))))))
              ((symbol-function 'browse-url) (lambda (url &rest _) (setq browsed url)))
              ((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args) (apply fn args)))
              ((symbol-function 'opencode-session--refresh-providers)
               (lambda (_conn callback) (funcall callback nil)))
              ((symbol-function 'message) #'ignore))
      (opencode-session--run-auth-flow
       conn "openai" '((type . "oauth") (id . "browser") (label . "Browser"))
       (lambda () (setq called t))))
    (should (equal browsed "https://x"))
    (should called)
    (should-not statuses)))

(ert-deftest test-opencode-model/oauth-code-flow-completes-with-code ()
  "Code-mode OAuth sends the code the user pastes."
  (let ((conn (opencode-model-test--connection))
        completed)
    (cl-letf (((symbol-function 'opencode-client-integration-oauth-connect)
               (lambda (_conn _id _method &rest args)
                 (funcall (plist-get args :success)
                          :data '((attemptID . "con_1") (url . "") (mode . "code")))))
              ((symbol-function 'read-string) (lambda (&rest _) "abc"))
              ((symbol-function 'opencode-client-integration-oauth-complete)
               (lambda (_conn _id attempt &rest args)
                 (setq completed (list attempt (plist-get args :code)))
                 (funcall (plist-get args :success))))
              ((symbol-function 'opencode-session--refresh-providers)
               (lambda (_conn callback) (funcall callback nil)))
              ((symbol-function 'message) #'ignore))
      (opencode-session--run-auth-flow
       conn "openai" '((type . "oauth") (id . "headless")) #'ignore))
    (should (equal completed '("con_1" "abc")))))

(provide 'emacs-opencode-session-model-test)

;;; emacs-opencode-session-model-test.el ends here
