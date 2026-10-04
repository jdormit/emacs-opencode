;;; emacs-opencode-session-handlers-test.el --- Tests for SSE event handlers  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-opencode-session-mode)
(require 'emacs-opencode-session-handlers)

(defmacro opencode-handlers-test--with-session (&rest body)
  "Run BODY in a fresh session buffer registered as session \"s1\"."
  (declare (indent 0))
  `(let ((opencode-session--buffers (make-hash-table :test 'equal))
         (opencode-session--subagent-parents (make-hash-table :test 'equal))
         (opencode-session--subagent-tools (make-hash-table :test 'equal))
         (buffer (generate-new-buffer " *oc-handlers-test*")))
     (unwind-protect
         (with-current-buffer buffer
           (opencode-session-mode)
           (setq-local opencode-session--session
                       (opencode-session-create :id "s1" :info '((id . "s1"))))
           (opencode-session--ensure-markers)
           (opencode-session--ensure-input-region)
           (puthash "s1" buffer opencode-session--buffers)
           (cl-letf (((symbol-function 'message) #'ignore))
             ,@body))
       (when (buffer-live-p buffer)
         (kill-buffer buffer)))))

(defun opencode-handlers-test--event (type data &rest envelope)
  "Return a server event of TYPE with DATA and extra ENVELOPE fields."
  (append `((type . ,type) (data . ,data)) envelope))

(defun opencode-handlers-test--dispatch (type data &rest envelope)
  "Dispatch a TYPE event with DATA and ENVELOPE to the registered handlers."
  (opencode-sse--dispatch type (apply #'opencode-handlers-test--event type data envelope)
                          (list :connection 'conn)))

(defun opencode-handlers-test--transcript ()
  "Return the rendered transcript of the current buffer."
  (buffer-substring-no-properties
   (point-min) (marker-position opencode-session--input-start-marker)))

(defun opencode-handlers-test--status ()
  "Return the current session status type."
  (opencode-status-type (opencode-session-status opencode-session--session)))

;;; Assistant streaming

(ert-deftest test-opencode-handlers/streams-assistant-text ()
  "Step, text, and delta events build and render an assistant reply."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.step.started"
     '((sessionID . "s1") (assistantMessageID . "a1") (agent . "build")
       (model . ((id . "gpt") (providerID . "openai"))) (started . 5)))
    (opencode-handlers-test--dispatch
     "session.text.started" '((sessionID . "s1") (assistantMessageID . "a1") (ordinal . 0)))
    (opencode-handlers-test--dispatch
     "session.text.delta" '((sessionID . "s1") (assistantMessageID . "a1") (delta . "Hel")))
    (opencode-handlers-test--dispatch
     "session.text.delta" '((sessionID . "s1") (assistantMessageID . "a1") (delta . "lo")))
    (should (string-match-p "Hello" (opencode-handlers-test--transcript)))
    (opencode-handlers-test--dispatch
     "session.text.ended" '((sessionID . "s1") (assistantMessageID . "a1") (text . "Hello!")))
    (opencode-handlers-test--dispatch
     "session.step.ended"
     '((sessionID . "s1") (assistantMessageID . "a1") (finish . "stop") (cost . 0.25)
       (tokens . ((input . 10) (output . 5)))))
    (let ((message (car opencode-session--messages)))
      (should (equal (opencode-message-role message) "assistant"))
      (should (equal (opencode-message-finish message) "stop"))
      (should (equal (alist-get 'cost (opencode-message-info message)) 0.25))
      (should (equal (opencode-message-model-id message) "gpt")))
    (should (string-match-p "Hello!" (opencode-handlers-test--transcript)))))

(ert-deftest test-opencode-handlers/step-failed-shows-error ()
  "A failed step shows its error, but not for interruptions."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.step.started" '((sessionID . "s1") (assistantMessageID . "a1")))
    (opencode-handlers-test--dispatch
     "session.step.failed"
     '((sessionID . "s1") (assistantMessageID . "a1")
       (error . ((type . "provider") (message . "rate limited")))))
    (should (string-match-p "Error: rate limited" (opencode-handlers-test--transcript)))
    (opencode-handlers-test--dispatch
     "session.step.started" '((sessionID . "s1") (assistantMessageID . "a2")))
    (opencode-handlers-test--dispatch
     "session.step.failed"
     '((sessionID . "s1") (assistantMessageID . "a2")
       (error . ((type . "interrupted") (message . "stopped")))))
    (should-not (string-match-p "stopped" (opencode-handlers-test--transcript)))))

(ert-deftest test-opencode-handlers/tool-lifecycle ()
  "Tool events move a part from pending to running to completed."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.step.started" '((sessionID . "s1") (assistantMessageID . "a1")))
    (opencode-handlers-test--dispatch
     "session.tool.input.started"
     '((sessionID . "s1") (assistantMessageID . "a1") (id . "c1") (name . "shell")))
    (should (string-match-p "Shell \\[pending\\]" (opencode-handlers-test--transcript)))
    (opencode-handlers-test--dispatch
     "session.tool.called"
     '((sessionID . "s1") (assistantMessageID . "a1") (id . "c1")
       (input . ((command . "ls")))))
    (should (string-match-p "Shell ls \\[running\\]" (opencode-handlers-test--transcript)))
    (opencode-handlers-test--dispatch
     "session.tool.success"
     '((sessionID . "s1") (assistantMessageID . "a1") (id . "c1")
       (metadata . ((exit . 0)))))
    (let ((transcript (opencode-handlers-test--transcript)))
      (should (string-match-p "Shell ls ▸" transcript))
      (should (string-match-p "\\$ ls" transcript)))))

(ert-deftest test-opencode-handlers/tool-failure-shows-error ()
  "A failed tool shows its error message."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.step.started" '((sessionID . "s1") (assistantMessageID . "a1")))
    (opencode-handlers-test--dispatch
     "session.tool.input.started"
     '((sessionID . "s1") (assistantMessageID . "a1") (id . "c1") (name . "read")))
    (opencode-handlers-test--dispatch
     "session.tool.failed"
     '((sessionID . "s1") (assistantMessageID . "a1") (id . "c1")
       (error . ((type . "tool") (message . "no such file")))))
    (let ((transcript (opencode-handlers-test--transcript)))
      (should (string-match-p "\\[error\\]" transcript))
      (should (string-match-p "no such file" transcript)))))

(ert-deftest test-opencode-handlers/edit-success-reverts-changed-files ()
  "A completed edit reloads buffers visiting the files it changed."
  (opencode-handlers-test--with-session
    (let (reverted)
      (cl-letf (((symbol-function 'opencode-session--maybe-revert-buffer)
                 (lambda (path) (push path reverted))))
        (opencode-handlers-test--dispatch
         "session.step.started" '((sessionID . "s1") (assistantMessageID . "a1")))
        (opencode-handlers-test--dispatch
         "session.tool.success"
         '((sessionID . "s1") (assistantMessageID . "a1") (id . "c1")
           (metadata . ((files . (((file . "/p/a.el")) ((file . "/p/b.el")))))))))
      (should (equal (sort reverted #'string<) '("/p/a.el" "/p/b.el"))))))

;;; Inbox

(ert-deftest test-opencode-handlers/inbox-shows-user-prompt ()
  "Enqueued prompts appear as user messages."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.inbox.enqueued"
     '((sessionID . "s1") (inboxID . "u1")
       (item . ((type . "user") (payload . ((text . "do the thing")))))))
    (should (equal (opencode-message-role (car opencode-session--messages)) "user"))
    (should (string-match-p "do the thing" (opencode-handlers-test--transcript)))))

(ert-deftest test-opencode-handlers/inbox-delivery-moves-prompt-last ()
  "A queued prompt moves after messages that arrived while it waited."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.inbox.enqueued"
     '((sessionID . "s1") (inboxID . "u1")
       (item . ((type . "user") (payload . ((text . "queued")))))))
    (opencode-handlers-test--dispatch
     "session.step.started" '((sessionID . "s1") (assistantMessageID . "a1")))
    (opencode-handlers-test--dispatch
     "session.inbox.delivered" '((sessionID . "s1") (inboxID . "u1")))
    (should (equal (mapcar #'opencode-message-id opencode-session--messages) '("a1" "u1")))))

(ert-deftest test-opencode-handlers/inbox-cancel-removes-prompt ()
  "Cancelled prompts disappear without losing typed input."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.inbox.enqueued"
     '((sessionID . "s1") (inboxID . "u1")
       (item . ((type . "user") (payload . ((text . "never mind")))))))
    (opencode-session-insert-input "draft")
    (opencode-handlers-test--dispatch
     "session.inbox.cancelled" '((sessionID . "s1") (inboxID . "u1")))
    (should-not opencode-session--messages)
    (should (equal (opencode-session--current-input) "draft"))))

(ert-deftest test-opencode-handlers/loaded-instructions-render ()
  "Instruction files loaded mid-session render as loaded lines."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.synthetic"
     '((sessionID . "s1") (description . "Loaded sub/AGENTS.md")
       (metadata . ((instruction . ((paths . ("/p/sub/AGENTS.md")))))))
     '(id . "evt_123"))
    (should (equal (opencode-message-id (car opencode-session--messages)) "msg_123"))
    (should (string-match-p "↳ Loaded /p/sub/AGENTS.md" (opencode-handlers-test--transcript)))))

(ert-deftest test-opencode-handlers/revert-drops-later-messages ()
  "Committing a revert removes messages from the boundary on."
  (opencode-handlers-test--with-session
    (dolist (id '("msg_1" "msg_2" "msg_3"))
      (opencode-handlers-test--dispatch
       "session.inbox.enqueued"
       `((sessionID . "s1") (inboxID . ,id)
         (item . ((type . "user") (payload . ((text . ,id))))))))
    (opencode-handlers-test--dispatch
     "session.revert.committed" '((sessionID . "s1") (to . "msg_2")))
    (should (equal (mapcar #'opencode-message-id opencode-session--messages) '("msg_1")))))

;;; Shell and compaction

(ert-deftest test-opencode-handlers/shell-command-lifecycle ()
  "User shell commands render running, then completed."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.shell.started"
     '((sessionID . "s1") (shell . ((id . "sh_1") (command . "make") (status . "running"))))
     '(id . "evt_9"))
    (should (string-match-p "Shell make \\[running\\]" (opencode-handlers-test--transcript)))
    (opencode-handlers-test--dispatch
     "session.shell.ended"
     '((sessionID . "s1") (shell . ((id . "sh_1") (command . "make") (status . "exited") (exit . 0)))
       (output . ((output . "built")))))
    (let ((state (opencode-message-part-state
                  (cdar (opencode-message-parts (car opencode-session--messages))))))
      (should (equal (alist-get 'status state) "completed"))
      (should (equal (alist-get 'output state) "built")))
    (should-not (string-match-p "running" (opencode-handlers-test--transcript)))))

(ert-deftest test-opencode-handlers/compaction-lifecycle ()
  "Compaction renders a marker that completes in place."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.inbox.enqueued"
     '((sessionID . "s1") (inboxID . "u1")
       (item . ((type . "user") (payload . ((text . "old text")))))))
    (opencode-handlers-test--dispatch
     "session.compaction.started" '((sessionID . "s1") (reason . "manual") (inputID . "c1")))
    (should (string-match-p "Session compacting" (opencode-handlers-test--transcript)))
    (opencode-handlers-test--dispatch
     "session.compaction.ended" '((sessionID . "s1") (reason . "manual")))
    (let ((transcript (opencode-handlers-test--transcript)))
      (should (string-match-p "old text" transcript))
      (should (string-match-p "Session compacted" transcript))
      (should-not (string-match-p "compacting" transcript)))
    (should (= (length opencode-session--messages) 2))))

(ert-deftest test-opencode-handlers/compaction-failure ()
  "A failed compaction says so."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.compaction.started" '((sessionID . "s1") (reason . "auto") (inputID . "c1")))
    (opencode-handlers-test--dispatch
     "session.compaction.failed"
     '((sessionID . "s1") (reason . "auto") (error . ((type . "x") (message . "too large")))))
    (let ((transcript (opencode-handlers-test--transcript)))
      (should (string-match-p "Session compaction failed" transcript))
      (should (string-match-p "too large" transcript)))))

;;; Session state

(ert-deftest test-opencode-handlers/execution-status ()
  "Execution events toggle the busy state."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch "session.execution.started" '((sessionID . "s1")))
    (should (equal (opencode-handlers-test--status) "busy"))
    (opencode-handlers-test--dispatch "session.execution.succeeded" '((sessionID . "s1")))
    (should (equal (opencode-handlers-test--status) "idle"))))

(ert-deftest test-opencode-handlers/retry-status-until-next-step ()
  "Scheduled retries show until the next step starts."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch "session.execution.started" '((sessionID . "s1")))
    (opencode-handlers-test--dispatch
     "session.retry.scheduled"
     '((sessionID . "s1") (assistantMessageID . "a1") (attempt . 2) (at . 1700000000000)
       (error . ((type . "x") (message . "overloaded")))))
    (let ((status (opencode-session-status opencode-session--session)))
      (should (equal (opencode-status-type status) "retry"))
      (should (= (opencode-status-attempt status) 2))
      (should (equal (opencode-status-message status) "overloaded"))
      (should (= (opencode-status-next status) 1700000000000)))
    (opencode-handlers-test--dispatch
     "session.step.started" '((sessionID . "s1") (assistantMessageID . "a1")))
    (should (equal (opencode-handlers-test--status) "busy"))))

(ert-deftest test-opencode-handlers/status-from-retry-empty-message ()
  "Empty retry messages are dropped."
  (should-not (opencode-status-message
               (opencode-session--status-from-retry
                '((attempt . 1) (error . ((type . "x") (message . ""))))))))

(ert-deftest test-opencode-handlers/rename-updates-title ()
  "Renamed sessions update their title and buffer name."
  (opencode-handlers-test--with-session
    (rename-buffer (opencode-session--buffer-name opencode-session--session) t)
    (opencode-handlers-test--dispatch "session.renamed" '((sessionID . "s1") (title . "New name")))
    (should (equal (opencode-session-title opencode-session--session) "New name"))
    (should (equal (buffer-name) "*OpenCode: New name*"))))

(ert-deftest test-opencode-handlers/selection-events-update-buffer ()
  "Agent and model selections made elsewhere show in the buffer."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch "session.agent.selected" '((sessionID . "s1") (agent . "plan")))
    (opencode-handlers-test--dispatch
     "session.model.selected"
     '((sessionID . "s1") (model . ((id . "gpt") (providerID . "openai") (variant . "high")))))
    (should (equal opencode-session--agent "plan"))
    (should (equal opencode-session--provider-id "openai"))
    (should (equal opencode-session--model-id "gpt"))
    (should (equal opencode-session--variant "high"))))

(ert-deftest test-opencode-handlers/usage-updates-session-cost ()
  "Usage events update the session totals shown in the header."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.usage.updated" '((sessionID . "s1") (cost . 1.5) (tokens . ((input . 3)))))
    (should (equal (alist-get 'cost (opencode-session-info opencode-session--session)) 1.5))))

;;; Subagents

(ert-deftest test-opencode-handlers/subagent-tool-calls-update-parent ()
  "Tool calls in a child session without a buffer update the parent's task."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.step.started" '((sessionID . "s1") (assistantMessageID . "a1")))
    (opencode-handlers-test--dispatch
     "session.tool.input.started"
     '((sessionID . "s1") (assistantMessageID . "a1") (id . "task1") (name . "subagent")))
    (opencode-handlers-test--dispatch
     "session.tool.called"
     '((sessionID . "s1") (assistantMessageID . "a1") (id . "task1")
       (input . ((agent . "explore") (description . "look around")))))
    (opencode-handlers-test--dispatch
     "session.tool.progress"
     '((sessionID . "s1") (assistantMessageID . "a1") (id . "task1")
       (metadata . ((sessionID . "child") (status . "running")))))
    (should (equal (opencode-session--subagent-parent "child") '("s1" . "task1")))
    (opencode-handlers-test--dispatch
     "session.tool.input.started"
     '((sessionID . "child") (assistantMessageID . "c1") (id . "g1") (name . "glob")))
    (opencode-handlers-test--dispatch
     "session.tool.called"
     '((sessionID . "child") (assistantMessageID . "c1") (id . "g1")
       (input . ((pattern . "*.el")))))
    (let ((transcript (opencode-handlers-test--transcript)))
      (should (string-match-p "# Explore Task" transcript))
      (should (string-match-p "look around (1 toolcalls)" transcript))
      (should (string-match-p "└ ✱ Glob \"\\*\\.el\"" transcript)))))

;;; Reconnection

(ert-deftest test-opencode-handlers/reconnect-resyncs-buffers ()
  "A reconnect reloads each session shown on that connection."
  (let ((connection (opencode-connection-create)))
    (opencode-handlers-test--with-session
      (setq-local opencode-session--connection connection)
      (let (reloaded fetched)
        (cl-letf (((symbol-function 'opencode-session--prompt-pending-requests) #'ignore)
                  ((symbol-function 'opencode-session--load-history)
                   (lambda (_conn session _buffer &rest _) (push (opencode-session-id session) reloaded)))
                  ((symbol-function 'opencode-session--fetch-info)
                   (lambda (_conn buffer) (push buffer fetched)))
                  ((symbol-function 'opencode-session--refresh-status) #'ignore))
          (opencode-session--handle-server-connected
           "server.connected" nil (list :connection connection :reconnect nil))
          (should-not reloaded)
          (opencode-session--handle-server-connected
           "server.connected" nil (list :connection connection :reconnect t)))
        (should (equal reloaded '("s1")))
        (should (equal fetched (list (current-buffer))))))))

;;; Permission helpers

(ert-deftest test-opencode-handlers/permission-detail ()
  "Permission prompts describe the action and its resources."
  (should (equal (opencode-session--permission-detail
                  '((action . "shell") (resources . ("git push"))))
                 "run git push"))
  (should (equal (opencode-session--permission-detail
                  '((action . "edit") (resources . ("a.el" "b.el"))))
                 "edit a.el, b.el"))
  (should (equal (opencode-session--permission-detail
                  '((action . "external_directory") (resources . ("/tmp/*"))))
                 "access external directory /tmp/*"))
  (should (equal (opencode-session--permission-detail
                  '((action . "subagent") (resources . ("explore"))))
                 "start subagent explore"))
  (should-not (opencode-session--permission-detail '((action . "edit")))))

(ert-deftest test-opencode-handlers/permission-prompt-label ()
  "The prompt label falls back to the action name."
  (should (equal (opencode-session--permission-prompt-label
                  '((action . "read") (resources . ("x.el"))))
                 "OpenCode wants to read x.el: "))
  (should (equal (opencode-session--permission-prompt-label '((action . "lsp")))
                 "OpenCode wants to use lsp: ")))

;;; Form helpers

(ert-deftest test-opencode-handlers/form-option-values ()
  "Selecting an option label answers with the option value."
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "GitHub.com")))
    (should (equal (opencode-session--read-form-answer
                    '(((key . "deploymentType") (type . "string")
                       (options . (((value . "github.com") (label . "GitHub.com")))))))
                   '((deploymentType . "github.com"))))))

(ert-deftest test-opencode-handlers/form-custom-answer ()
  "Choosing Other reads a custom answer."
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Other"))
            ((symbol-function 'read-string) (lambda (&rest _) "green")))
    (should (equal (opencode-session--read-form-answer
                    '(((key . "q0") (type . "string") (custom . t)
                       (options . (((value . "Red") (label . "Red")))))))
                   '((q0 . "green"))))))

(ert-deftest test-opencode-handlers/form-multiselect ()
  "Multiselect fields answer with a vector of values."
  (cl-letf (((symbol-function 'completing-read-multiple) (lambda (&rest _) '("A" "B"))))
    (should (equal (opencode-session--read-form-answer
                    '(((key . "q0") (type . "multiselect")
                       (options . (((value . "a") (label . "A")) ((value . "b") (label . "B")))))))
                   '((q0 . ["a" "b"]))))))

(ert-deftest test-opencode-handlers/form-hidden-and-conditional-fields ()
  "Hidden fields keep defaults and inactive conditional fields are skipped."
  (let (asked)
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &rest _) (push prompt asked) "typed"))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should (equal (opencode-session--read-form-answer
                      '(((key . "server") (type . "string") (hidden . t) (default . "https://x"))
                        ((key . "enterprise") (type . "boolean"))
                        ((key . "host") (type . "string") (title . "Host")
                         (when . (((key . "enterprise") (op . "eq") (value . t)))))))
                     '((server . "https://x") (enterprise . :json-false)))))
    (should-not asked)))

(ert-deftest test-opencode-handlers/form-boolean-false-is-answered ()
  "A no answer and a false hidden default are both sent, and gate conditions."
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
            ((symbol-function 'read-string) (lambda (&rest _) "typed")))
    (should (equal (opencode-session--read-form-answer
                    '(((key . "ok") (type . "boolean") (required . t))
                      ((key . "quiet") (type . "boolean") (hidden . t) (default . nil))
                      ((key . "why") (type . "string")
                       (when . (((key . "ok") (op . "eq") (value . nil)))))))
                   '((ok . :json-false) (quiet . :json-false) (why . "typed"))))))

;;; Permission and form replies

(defmacro opencode-handlers-test--with-sync-timer (&rest body)
  "Evaluate BODY with `run-at-time' running its function synchronously."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'run-at-time)
              (lambda (_secs _repeat fn &rest args)
                (apply fn args))))
     ,@body))

(ert-deftest test-opencode-handlers/permission-reply-routes-to-event-connection ()
  "A permission reply goes to the connection and session that asked.
Even when an unrelated buffer on a different connection exists, the
reply must go to the originating connection, not an arbitrary buffer."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (opencode-session--pending-prompts (make-hash-table :test #'eq))
        (other-buf (generate-new-buffer " *oc-test-other-conn*"))
        replied)
    (unwind-protect
        (progn
          (with-current-buffer other-buf
            (setq-local opencode-session--connection 'conn-other))
          (puthash "other-session" other-buf opencode-session--buffers)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "Allow once"))
                    ((symbol-function 'opencode-client-permission-reply)
                     (lambda (conn session-id request-id decision &rest _args)
                       (setq replied (list conn session-id request-id decision)))))
            (opencode-handlers-test--with-sync-timer
              (opencode-session--handle-permission-asked
               "permission.asked"
               '((data . ((id . "per_123")
                          (sessionID . "sub_agent_session")
                          (action . "read")
                          (resources . ("x.el")))))
               (list :connection 'conn-asking))))
          (should (equal replied '(conn-asking "sub_agent_session" "per_123" "once"))))
      (kill-buffer other-buf))))

(ert-deftest test-opencode-handlers/permission-always ()
  "Allow always replies with the always decision."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (opencode-session--pending-prompts (make-hash-table :test #'eq))
        decision)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Allow always"))
              ((symbol-function 'opencode-client-permission-reply)
               (lambda (_conn _session-id _request-id reply &rest _args)
                 (setq decision reply))))
      (opencode-handlers-test--with-sync-timer
        (opencode-session--handle-permission-asked
         "permission.asked" '((data . ((id . "per_1") (sessionID . "s") (action . "edit"))))
         (list :connection 'conn))))
    (should (equal decision "always"))))

(ert-deftest test-opencode-handlers/form-reply-routes-to-event-connection ()
  "Form answers go to the connection and session that asked."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (opencode-session--pending-prompts (make-hash-table :test #'eq))
        replied)
    (cl-letf (((symbol-function 'opencode-session--read-form-answer)
               (lambda (&rest _) '((q0 . "Blue"))))
              ((symbol-function 'opencode-client-form-reply)
               (lambda (conn session-id form-id answer &rest _args)
                 (setq replied (list conn session-id form-id answer)))))
      (opencode-handlers-test--with-sync-timer
        (opencode-session--handle-form-created
         "form.created"
         '((data . ((form . ((id . "frm_1") (sessionID . "s9") (title . "Questions")
                             (fields . (((key . "q0") (type . "string")))))))))
         (list :connection 'conn-asking))))
    (should (equal replied '(conn-asking "s9" "frm_1" ((q0 . "Blue")))))))

;;; Cross-client prompt resolution

(ert-deftest test-opencode-handlers/resolved-before-timer-skips-prompt ()
  "A remotely resolved request is not prompted after its timer fires."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        timer-function
        (prompted nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest _args)
                 (setq timer-function fn)))
              ((symbol-function 'opencode-session--prompt-permission)
               (lambda (&rest _args)
                 (setq prompted t))))
      (opencode-session--handle-permission-asked
       "permission.asked" '((data . ((id . "per_remote") (sessionID . "session"))))
       (list :connection 'conn))
      (opencode-session--handle-permission-replied
       "permission.replied" '((data . ((requestID . "per_remote"))))
       (list :connection 'conn))
      (funcall timer-function))
    (should-not prompted)))

(ert-deftest test-opencode-handlers/duplicate-asked-schedules-one-prompt ()
  "Duplicate asked events schedule only one minibuffer prompt."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (timer-count 0))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _args)
                 (setq timer-count (1+ timer-count)))))
      (dotimes (_ 2)
        (opencode-session--handle-form-created
         "form.created" '((data . ((form . ((id . "frm_dup") (sessionID . "session"))))))
         (list :connection 'conn))))
    (should (= timer-count 1))))

(ert-deftest test-opencode-handlers/remote-permission-reply-dismisses-prompt ()
  "A remote permission reply aborts the prompt without replying again."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (reply-count 0))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (opencode-session--handle-permission-replied
                  "permission.replied" '((data . ((requestID . "per_remote"))))
                  (list :connection 'conn))))
              ((symbol-function 'opencode-session--prompt-active-p)
               (lambda (_state) t))
              ((symbol-function 'abort-recursive-edit)
               (lambda () (signal 'quit nil)))
              ((symbol-function 'opencode-client-permission-reply)
               (lambda (&rest _args)
                 (setq reply-count (1+ reply-count)))))
      (opencode-handlers-test--with-sync-timer
        (opencode-session--handle-permission-asked
         "permission.asked"
         '((data . ((id . "per_remote") (sessionID . "session") (action . "read"))))
         (list :connection 'conn))))
    (should (= reply-count 0))))

(ert-deftest test-opencode-handlers/remote-form-answer-dismisses-prompt ()
  "A form answered in another client aborts the prompt without replying."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (calls 0))
    (cl-letf (((symbol-function 'opencode-session--read-form-answer)
               (lambda (&rest _args)
                 (opencode-session--handle-form-resolved
                  "form.replied" '((data . ((id . "frm_remote"))))
                  (list :connection 'conn))))
              ((symbol-function 'opencode-session--prompt-active-p)
               (lambda (_state) t))
              ((symbol-function 'abort-recursive-edit)
               (lambda () (signal 'quit nil)))
              ((symbol-function 'opencode-client-form-reply)
               (lambda (&rest _args) (cl-incf calls)))
              ((symbol-function 'opencode-client-form-cancel)
               (lambda (&rest _args) (cl-incf calls))))
      (opencode-handlers-test--with-sync-timer
        (opencode-session--handle-form-created
         "form.created"
         '((data . ((form . ((id . "frm_remote") (sessionID . "session"))))))
         (list :connection 'conn))))
    (should (= calls 0))))

(ert-deftest test-opencode-handlers/resolution-does-not-abort-other-prompt ()
  "Resolving one request does not abort a different active prompt."
  (let* ((opencode-session--pending-prompts (make-hash-table :test #'eq))
         (connection 'conn)
         (active (opencode-session--register-prompt connection "per_active"))
         (_other (opencode-session--register-prompt connection "per_other"))
         (aborted nil)
         (opencode-session--active-prompt active))
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () t))
              ((symbol-function 'abort-recursive-edit)
               (lambda () (setq aborted t))))
      (opencode-session--handle-permission-replied
       "permission.replied" '((data . ((requestID . "per_other"))))
       (list :connection connection)))
    (should-not aborted)
    (should (eq (opencode-session--prompt-state-status active) 'pending))))

(ert-deftest test-opencode-handlers/prompt-active-requires-matching-minibuffer ()
  "Prompt ownership requires both active state and a matching minibuffer."
  (let* ((state (opencode-session--prompt-state-create :status 'pending))
         (other (opencode-session--prompt-state-create :status 'pending))
         (opencode-session--active-prompt state)
         (opencode-session--minibuffer-prompt other))
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () t))
              ((symbol-function 'window-buffer) (lambda (_window) (current-buffer))))
      (should-not (opencode-session--prompt-active-p state))
      (setq opencode-session--minibuffer-prompt state)
      (should (opencode-session--prompt-active-p state)))))

(ert-deftest test-opencode-handlers/prompt-error-clears-pending-state ()
  "A failed prompt does not suppress a later event for the same request."
  (let* ((opencode-session--pending-prompts (make-hash-table :test #'eq))
         (connection 'conn)
         (state (opencode-session--register-prompt connection "per_retry")))
    (should-error
     (opencode-session--run-prompt state (lambda () (error "Prompt failed"))))
    (should (opencode-session--register-prompt connection "per_retry"))))

(ert-deftest test-opencode-handlers/user-quit-still-denies-permission ()
  "Quitting a permission prompt still sends a rejection."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        reply)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) (signal 'quit nil)))
              ((symbol-function 'opencode-client-permission-reply)
               (lambda (_connection _session-id _request-id value &rest _args)
                 (setq reply value))))
      (opencode-handlers-test--with-sync-timer
        (opencode-session--handle-permission-asked
         "permission.asked"
         '((data . ((id . "per_quit") (sessionID . "session") (action . "read"))))
         (list :connection 'conn))))
    (should (equal reply "reject"))))

(ert-deftest test-opencode-handlers/user-quit-cancels-form ()
  "Quitting a form dismisses it on the server."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        cancelled)
    (cl-letf (((symbol-function 'opencode-session--read-form-answer)
               (lambda (&rest _args) (signal 'quit nil)))
              ((symbol-function 'opencode-client-form-cancel)
               (lambda (_conn session-id form-id &rest _args)
                 (setq cancelled (list session-id form-id)))))
      (opencode-handlers-test--with-sync-timer
        (opencode-session--handle-form-created
         "form.created"
         '((data . ((form . ((id . "frm_quit") (sessionID . "session"))))))
         (list :connection 'conn))))
    (should (equal cancelled '("session" "frm_quit")))))

;;; File events

(ert-deftest test-opencode-handlers/filesystem-event-paths ()
  "Filesystem events name one file."
  (should (equal (opencode-session--event-file-paths '((data . ((file . "/p/a.el")))))
                 '("/p/a.el")))
  (should-not (opencode-session--event-file-paths '((data . ((other . "x")))))))

(ert-deftest test-opencode-handlers/file-revert-skips-unchanged-buffers ()
  "Buffers whose files did not change on disk are not reverted."
  (let ((file (make-temp-file "oc-revert" nil ".txt" "one")))
    (unwind-protect
        (let ((buffer (find-file-noselect file))
              reverted)
          (unwind-protect
              (cl-letf (((symbol-function 'revert-buffer)
                         (lambda (&rest _) (setq reverted t)))
                        ((symbol-function 'message) #'ignore))
                (opencode-session--maybe-revert-buffer file)
                (should-not reverted)
                (set-file-times file (time-add (current-time) 10))
                (opencode-session--maybe-revert-buffer file)
                (should reverted))
            (kill-buffer buffer)))
      (delete-file file))))

(ert-deftest test-opencode-handlers/shell-timeout-renders-error ()
  "Shell commands that time out render as errors."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.shell.started"
     '((sessionID . "s1") (shell . ((id . "sh_1") (command . "sleep 99") (status . "running"))))
     '(id . "evt_9"))
    (opencode-handlers-test--dispatch
     "session.shell.ended"
     '((sessionID . "s1") (shell . ((id . "sh_1") (command . "sleep 99") (status . "timeout")))))
    (let ((transcript (opencode-handlers-test--transcript)))
      (should (string-match-p "\\[error\\]" transcript))
      (should (string-match-p "Command timed out" transcript)))))

;;; History-load races

(ert-deftest test-opencode-handlers/events-during-history-load-replay ()
  "Events that arrive while history loads are applied to the loaded snapshot.
Changes the snapshot already holds are not applied twice."
  (opencode-handlers-test--with-session
    (setq-local opencode-session--connection
                (opencode-connection-create :models-state :loaded))
    (let (finish-inbox finish-messages)
      (cl-letf (((symbol-function 'opencode-client-session-inbox)
                 (lambda (_conn _sid &rest args) (setq finish-inbox (plist-get args :success))))
                ((symbol-function 'opencode-client-session-messages)
                 (lambda (_conn _sid &rest args) (setq finish-messages (plist-get args :success)))))
        (opencode-session--load-history opencode-session--connection opencode-session--session (current-buffer))
        ;; Already in the snapshot: the step, its first text block, a started tool.
        (opencode-handlers-test--dispatch
         "session.text.started" '((sessionID . "s1") (assistantMessageID . "a1") (ordinal . 0)))
        (opencode-handlers-test--dispatch
         "session.tool.success" '((sessionID . "s1") (assistantMessageID . "a1") (id . "c1")))
        (opencode-handlers-test--dispatch
         "session.text.delta" '((sessionID . "s1") (assistantMessageID . "a1")
                                (ordinal . 0) (delta . "live")))
        (should-not opencode-session--messages)
        (funcall finish-inbox :data '(((id . "u9") (type . "user")
                                       (payload . ((text . "queued prompt")))
                                       (delivery . "queue"))))
        (funcall finish-messages
                 :data '(((id . "a1") (type . "assistant")
                          (content . (((type . "text") (text . ""))
                                      ((type . "tool") (id . "c1") (name . "read")
                                       (state . ((status . "running") (input . ((path . "x"))))))))))))
      (let* ((message (opencode-session--find-message "a1"))
             (texts (cl-remove-if-not (lambda (entry) (equal (opencode-message-part-type (cdr entry)) "text"))
                                      (opencode-message-parts message))))
        (should (= (length texts) 1))
        (should (equal (opencode-message-part-text (cdar texts)) "live"))
        (should (equal (alist-get 'status (opencode-message-part-state
                                           (cdr (assoc "c1" (opencode-message-parts message)))))
                       "completed")))
      (should (equal (mapcar #'opencode-message-id opencode-session--messages) '("a1" "u9")))
      (should (string-match-p "queued prompt" (opencode-handlers-test--transcript))))))

(ert-deftest test-opencode-handlers/replayed-tool-call-does-not-regress ()
  "A replayed tool.called does not move a completed tool back to running."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.step.started" '((sessionID . "s1") (assistantMessageID . "a1")))
    (opencode-handlers-test--dispatch
     "session.tool.input.started" '((sessionID . "s1") (assistantMessageID . "a1") (id . "c1") (name . "glob")))
    (opencode-handlers-test--dispatch
     "session.tool.called" '((sessionID . "s1") (assistantMessageID . "a1") (id . "c1") (input . ((pattern . "*")))))
    (opencode-handlers-test--dispatch
     "session.tool.success" '((sessionID . "s1") (assistantMessageID . "a1") (id . "c1")))
    (opencode-handlers-test--dispatch
     "session.tool.called" '((sessionID . "s1") (assistantMessageID . "a1") (id . "c1") (input . ((pattern . "*")))))
    (should (equal (alist-get 'status (opencode-message-part-state
                                       (cdar (opencode-message-parts (car opencode-session--messages)))))
                   "completed"))))

(ert-deftest test-opencode-handlers/delivered-moves-only-pending-input ()
  "Delivery moves an input queued in this client, not one already placed."
  (opencode-handlers-test--with-session
    (opencode-handlers-test--dispatch
     "session.inbox.enqueued"
     '((sessionID . "s1") (inboxID . "u1") (item . ((type . "user") (payload . ((text . "a")))))))
    (opencode-handlers-test--dispatch
     "session.step.started" '((sessionID . "s1") (assistantMessageID . "a1")))
    (opencode-handlers-test--dispatch "session.inbox.delivered" '((sessionID . "s1") (inboxID . "u1")))
    (opencode-handlers-test--dispatch
     "session.step.started" '((sessionID . "s1") (assistantMessageID . "a2")))
    (opencode-handlers-test--dispatch "session.inbox.delivered" '((sessionID . "s1") (inboxID . "u1")))
    (should (equal (mapcar #'opencode-message-id opencode-session--messages) '("a1" "u1" "a2")))))

(ert-deftest test-opencode-handlers/run-end-with-unsettled-tool-reloads ()
  "A run that ends while a tool still looks unfinished reloads the transcript."
  (opencode-handlers-test--with-session
    (setq-local opencode-session--connection
                (opencode-connection-create :models-state :loaded))
    (let (reloaded)
      (cl-letf (((symbol-function 'opencode-session--load-history)
                 (lambda (&rest _) (setq reloaded t))))
        (opencode-handlers-test--dispatch
         "session.step.started" '((sessionID . "s1") (assistantMessageID . "a1")))
        (opencode-handlers-test--dispatch "session.execution.succeeded" '((sessionID . "s1")))
        (should-not reloaded)
        (opencode-handlers-test--dispatch
         "session.tool.input.started" '((sessionID . "s1") (assistantMessageID . "a1") (id . "c1") (name . "read")))
        (opencode-handlers-test--dispatch "session.execution.succeeded" '((sessionID . "s1")))
        (should reloaded)))))

(ert-deftest test-opencode-handlers/connect-prompts-pending-requests ()
  "Connecting asks for permissions and forms the server is already waiting on."
  (let ((conn (opencode-connection-create))
        prompted)
    (cl-letf (((symbol-function 'opencode-client-pending-permissions)
               (lambda (_c &rest args)
                 (funcall (plist-get args :success) :data '(((id . "per_1") (sessionID . "s"))))))
              ((symbol-function 'opencode-client-pending-forms)
               (lambda (_c &rest args)
                 (funcall (plist-get args :success) :data '(((id . "frm_1") (sessionID . "s"))))))
              ((symbol-function 'opencode-session--handle-permission-asked)
               (lambda (_e data meta) (push (list (alist-get 'id (alist-get 'data data)) (plist-get meta :connection)) prompted)))
              ((symbol-function 'opencode-session--handle-form-created)
               (lambda (_e data meta) (push (list (alist-get 'id (alist-get 'form (alist-get 'data data)))
                                                  (plist-get meta :connection))
                                            prompted))))
      (opencode-session--handle-server-connected "server.connected" nil (list :connection conn)))
    (should (equal (sort (mapcar #'car prompted) #'string<) '("frm_1" "per_1")))
    (should (cl-every (lambda (entry) (eq (cadr entry) conn)) prompted))))

(ert-deftest test-opencode-handlers/connect-reloads-catalogs ()
  "Every stream connect drops cached catalogs so they are fetched again."
  (let ((conn (opencode-connection-create :models '(x) :models-state :loaded
                                          :commands '(y) :commands-state :loaded)))
    (cl-letf (((symbol-function 'opencode-session--prompt-pending-requests) #'ignore))
      (opencode-session--handle-server-connected "server.connected" nil (list :connection conn)))
    (should-not (opencode-connection-models-state conn))
    (should-not (opencode-connection-commands-state conn))))

(provide 'emacs-opencode-session-handlers-test)

;;; emacs-opencode-session-handlers-test.el ends here
