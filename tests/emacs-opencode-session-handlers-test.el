;;; emacs-opencode-session-handlers-test.el --- Tests for SSE event handlers  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-opencode-session-handlers)
(require 'emacs-opencode-session-mode)

;;; session-error-text

(ert-deftest test-opencode-handlers/error-text-detail-message ()
  "Extract detail message from nested alist."
  (should (equal (opencode-session--session-error-text
                  '((name . "SomeError")
                    (data . ((message . "something went wrong")))))
                 "something went wrong")))

(ert-deftest test-opencode-handlers/error-text-v2-shape ()
  "Extract the message from a V2 error alist."
  (should (equal (opencode-session--session-error-text
                  '((type . "provider.auth")
                    (message . "Invalid API key")
                    (status . 401)))
                 "Invalid API key")))

(ert-deftest test-opencode-handlers/error-text-name-only ()
  "Fall back to error name when no detail message."
  (should (equal (opencode-session--session-error-text
                  '((name . "ConnectionError")
                    (data . ((message . "")))))
                 "ConnectionError")))

(ert-deftest test-opencode-handlers/error-text-string ()
  "Handle string error info."
  (should (equal (opencode-session--session-error-text "plain error")
                 "plain error")))

(ert-deftest test-opencode-handlers/error-text-nil ()
  "Fall back to default for nil."
  (should (equal (opencode-session--session-error-text nil)
                 "An error occurred")))

(ert-deftest test-opencode-handlers/error-text-empty-name ()
  "Fall back to default when name is empty."
  (should (equal (opencode-session--session-error-text
                  '((name . "") (data . nil)))
                 "An error occurred")))

;;; permission helpers (V2 Permission.Request shape)

(ert-deftest test-opencode-handlers/permission-patterns-vector ()
  "Extract resources from vector."
  (should (equal (opencode-session--permission-patterns
                  '((resources . ["*.el" "*.org"])))
                 '("*.el" "*.org"))))

(ert-deftest test-opencode-handlers/permission-patterns-list ()
  "Extract resources from list."
  (should (equal (opencode-session--permission-patterns
                  '((resources . ("*.el"))))
                 '("*.el"))))

(ert-deftest test-opencode-handlers/permission-patterns-nil ()
  "Return nil when no resources."
  (should (null (opencode-session--permission-patterns '((other . "x"))))))

(ert-deftest test-opencode-handlers/permission-detail-read ()
  "Detail for a read permission."
  (should (equal (opencode-session--permission-detail
                  '((action . "read")
                    (resources . ["foo.el"])))
                 "read foo.el")))

(ert-deftest test-opencode-handlers/permission-detail-first-resource ()
  "Detail uses the first resource when several are present."
  (should (equal (opencode-session--permission-detail
                  '((action . "edit")
                    (resources . ["a.el" "b.el"])))
                 "edit a.el")))

(ert-deftest test-opencode-handlers/permission-detail-resource-only ()
  "Fall back to the resource when action is absent."
  (should (equal (opencode-session--permission-detail
                  '((resources . ["bar.el"])))
                 "bar.el")))

(ert-deftest test-opencode-handlers/permission-detail-action-only ()
  "No detail when only the action is present; the label falls back."
  (should (null (opencode-session--permission-detail
                 '((action . "webfetch"))))))

(ert-deftest test-opencode-handlers/permission-detail-nil ()
  "Return nil when nothing usable is present."
  (should (null (opencode-session--permission-detail
                 '((metadata . ((description . "x"))))))))

(ert-deftest test-opencode-handlers/permission-prompt-label ()
  "Build a prompt label from action and resource."
  (let ((result (opencode-session--permission-prompt-label
                 '((action . "read")
                   (resources . ["foo.el"])))))
    (should (string-match-p "read foo\\.el" result))
    (should (string-suffix-p ": " result))))

(ert-deftest test-opencode-handlers/permission-prompt-label-fallback ()
  "Prompt label falls back to action."
  (let ((result (opencode-session--permission-prompt-label
                 '((action . "custom")))))
    (should (string-match-p "use custom" result))))

;;; event-file-paths (V2 envelope)

(ert-deftest test-opencode-handlers/event-file-paths-vector ()
  "Extract paths from vector."
  (should (equal (opencode-session--event-file-paths
                  '((data . ((paths . ["a.el" "b.el"])))))
                 '("a.el" "b.el"))))

(ert-deftest test-opencode-handlers/event-file-paths-list ()
  "Extract paths from list."
  (should (equal (opencode-session--event-file-paths
                  '((data . ((paths . ("a.el" "b.el"))))))
                 '("a.el" "b.el"))))

(ert-deftest test-opencode-handlers/event-file-paths-nil ()
  "Return nil when no paths found."
  (should (null (opencode-session--event-file-paths
                 '((data . ((other . "x"))))))))

;;; permission reply routing by connection

(defmacro opencode-handlers-test--with-sync-timer (&rest body)
  "Evaluate BODY with `run-at-time' running its function synchronously.
This lets handler tests observe the deferred prompt without a real timer."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'run-at-time)
              (lambda (_secs _repeat fn &rest args)
                (apply fn args))))
     ,@body))

(ert-deftest test-opencode-handlers/permission-reply-routes-to-event-connection ()
  "A subagent permission reply is sent via the connection that asked.
Even when an unrelated buffer on a different connection exists, the
reply must go to the originating connection."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (event-conn 'conn-asking)
        (other-conn 'conn-other)
        (other-buf (generate-new-buffer " *oc-test-other-conn*"))
        (replied nil))
    (unwind-protect
        (progn
          ;; A buffer belonging to a DIFFERENT connection is registered.
          (with-current-buffer other-buf
            (setq-local opencode-session--connection other-conn))
          (let ((opencode-session--buffers (make-hash-table :test 'equal)))
            (puthash "other-session" other-buf opencode-session--buffers)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "Allow once"))
                      ((symbol-function 'opencode-client-permission-reply)
                       (lambda (conn session-id request-id reply &rest _args)
                         (setq replied (list conn session-id request-id reply)))))
              (opencode-handlers-test--with-sync-timer
                (opencode-session--handle-permission-asked
                 '((id . "evt_1")
                   (data . ((id . "per_123")
                            (sessionID . "sub_agent_session")
                            (action . "read")
                            (resources . ["x.el"]))))
                 (list :connection event-conn)))))
          (should (equal replied (list event-conn "sub_agent_session" "per_123" "once"))))
      (kill-buffer other-buf))))

(ert-deftest test-opencode-handlers/permission-reply-no-buffer-still-replies ()
  "With no buffer at all, the reply still goes via the event connection."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (opencode-session--buffers (make-hash-table :test 'equal))
        (event-conn 'conn-asking)
        (replied-conn :unset))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Allow always"))
              ((symbol-function 'opencode-client-permission-reply)
               (lambda (conn _session-id _request-id reply &rest _args)
                 (setq replied-conn (cons conn reply)))))
      (opencode-handlers-test--with-sync-timer
        (opencode-session--handle-permission-asked
         '((id . "evt_2")
           (data . ((id . "per_456")
                    (sessionID . "sub_agent_session")
                    (action . "read"))))
         (list :connection event-conn))))
    (should (equal replied-conn (cons event-conn "always")))))

;;; Cross-client prompt resolution

(ert-deftest test-opencode-handlers/resolved-before-timer-skips-prompt ()
  "A remotely resolved request is not prompted after its timer fires."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (connection 'conn)
        timer-function
        (prompted nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest _args)
                 (setq timer-function fn)))
              ((symbol-function 'opencode-session--prompt-permission)
               (lambda (&rest _args)
                 (setq prompted t))))
      (opencode-session--handle-permission-asked
       '((id . "evt_3")
         (data . ((id . "per_remote")
                  (sessionID . "session"))))
       (list :connection connection))
      (opencode-session--handle-prompt-resolved
       '((data . ((requestID . "per_remote"))))
       (list :connection connection))
      (funcall timer-function))
    (should-not prompted)))

(ert-deftest test-opencode-handlers/duplicate-asked-schedules-one-prompt ()
  "Duplicate asked events schedule only one minibuffer prompt."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (connection 'conn)
        (timer-count 0))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _args)
                 (setq timer-count (1+ timer-count)))))
      (dotimes (_ 2)
        (opencode-session--handle-permission-asked
         '((id . "evt_4")
           (data . ((id . "per_duplicate")
                    (sessionID . "session")
                    (action . "read"))))
         (list :connection connection))))
    (should (= timer-count 1))))

(ert-deftest test-opencode-handlers/remote-permission-reply-dismisses-prompt ()
  "A remote permission reply aborts the prompt without replying again."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (connection 'conn)
        (reply-count 0))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (opencode-session--handle-prompt-resolved
                  '((data . ((requestID . "per_remote"))))
                  (list :connection connection))))
              ((symbol-function 'opencode-session--prompt-active-p)
               (lambda (_state) t))
              ((symbol-function 'abort-recursive-edit)
               (lambda () (signal 'quit nil)))
              ((symbol-function 'opencode-client-permission-reply)
               (lambda (&rest _args)
                 (setq reply-count (1+ reply-count)))))
      (opencode-handlers-test--with-sync-timer
        (opencode-session--handle-permission-asked
         '((id . "evt_5")
           (data . ((id . "per_remote")
                    (sessionID . "session")
                    (action . "read"))))
         (list :connection connection))))
    (should (= reply-count 0))))

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
      (opencode-session--handle-prompt-resolved
       '((data . ((requestID . "per_other"))))
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
        (connection 'conn)
        reply)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) (signal 'quit nil)))
              ((symbol-function 'opencode-client-permission-reply)
               (lambda (_connection _session-id _request-id value &rest _args)
                 (setq reply value))))
      (opencode-handlers-test--with-sync-timer
        (opencode-session--handle-permission-asked
         '((id . "evt_6")
           (data . ((id . "per_quit")
                    (sessionID . "session")
                    (action . "read"))))
         (list :connection connection))))
    (should (equal reply "reject"))))

;;; User inbox

(ert-deftest test-opencode-handlers/inbox-enqueued-renders-user-message ()
  "session.inbox.enqueued renders the user message text."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-inbox-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq-local opencode-session--session (opencode-session-create :id "s1"))
          (opencode-session--ensure-markers)
          (puthash "s1" buffer opencode-session--buffers)
          (opencode-session--handle-inbox-enqueued
           '((data . ((inboxID . "msg_user_1")
                      (sessionID . "s1")
                      (item . ((type . "user")
                               (payload . ((text . "Hello there")
                                           (files . [])))
                               (delivery . "steer")))))))
          (should (string-match-p "Hello there"
                                  (buffer-substring-no-properties (point-min) (point-max))))
          (should (= (length opencode-session--messages) 1))
          (should (equal (opencode-message-role (car opencode-session--messages)) "user")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; Text streaming

(ert-deftest test-opencode-handlers/text-stream-part-updates ()
  "Text events create and update a message part with the V2 envelope."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-text-stream-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq-local opencode-session--session (opencode-session-create :id "s1"))
          (opencode-session--ensure-markers)
          (puthash "s1" buffer opencode-session--buffers)
          (opencode-session--handle-text-started
           '((created . 1)
             (data . ((sessionID . "s1")
                      (assistantMessageID . "msg_1")
                      (ordinal . 0)))))
          (opencode-session--handle-text-delta
           '((data . ((sessionID . "s1")
                      (assistantMessageID . "msg_1")
                      (ordinal . 0)
                      (delta . "Hel")))))
          (opencode-session--handle-text-delta
           '((data . ((sessionID . "s1")
                      (assistantMessageID . "msg_1")
                      (ordinal . 0)
                      (delta . "lo")))))
          (opencode-session--handle-text-ended
           '((created . 2)
             (data . ((sessionID . "s1")
                      (assistantMessageID . "msg_1")
                      (ordinal . 0)
                      (text . "Hello")))))
          (let* ((message (car opencode-session--messages))
                 (part (cdr (assoc "text-0" (opencode-message-parts message)))))
            (should (opencode-message-part-p part))
            (should (equal (opencode-message-part-text part) "Hello"))
            (should (equal (opencode-message-role message) "assistant"))
            (should (string-match-p "Hello"
                                    (buffer-substring-no-properties (point-min) (point-max))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; Tool streaming

(ert-deftest test-opencode-handlers/tool-events-map-content-to-output ()
  "Tool events build a completed part with V2 state.content as output."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-tool-stream-test*"))
        (connection 'conn))
    (unwind-protect
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq-local opencode-session--session (opencode-session-create :id "s1"))
          (opencode-session--ensure-markers)
          (puthash "s1" buffer opencode-session--buffers)
          (opencode-session--handle-tool-input-started
           '((data . ((sessionID . "s1")
                      (assistantMessageID . "msg_1")
                      (id . "call_1")
                      (name . "bash"))))
           (list :connection connection))
          (opencode-session--handle-tool-input-ended
           '((data . ((sessionID . "s1")
                      (assistantMessageID . "msg_1")
                      (id . "call_1")
                      (text . "{\"command\": \"echo hi\"}")))))
          (opencode-session--handle-tool-called
           '((data . ((sessionID . "s1")
                      (assistantMessageID . "msg_1")
                      (id . "call_1")
                      (input . ((command . "echo hi")))
                      (executed . :json-false)))))
          (opencode-session--handle-tool-success
           '((data . ((sessionID . "s1")
                      (assistantMessageID . "msg_1")
                      (id . "call_1")
                      (content . [((type . "text") (text . "hi"))])
                      (metadata . ((status . "completed"))))))
           (list :connection connection))
          (let* ((message (car opencode-session--messages))
                 (part (cdr (assoc "call_1" (opencode-message-parts message))))
                 (state (opencode-message-part-state part)))
            (should (equal (opencode-message-part-tool part) "bash"))
            (should (equal (alist-get 'status state) "completed"))
            (should (equal (alist-get 'output state) "hi"))
            ;; input survives the success update (carried from the called state)
            (should (equal (alist-get 'input state) '((command . "echo hi")))))
          (should (string-match-p "hi"
                                  (buffer-substring-no-properties (point-min) (point-max)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; Session events

(ert-deftest test-opencode-handlers/session-created-updates-buffer ()
  "session.created updates the buffer session from the V2 payload."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-session-created-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq-local opencode-session--session nil)
          (opencode-session--ensure-markers)
          (puthash "s1" buffer opencode-session--buffers)
          (opencode-session--handle-session-upsert
           '((data . ((sessionID . "s1")
                      (title . "My Session")
                      (location . ((directory . "/tmp/work")))))))
          (should (opencode-session-p opencode-session--session))
          (should (equal (opencode-session-title opencode-session--session) "My Session"))
          (should (equal (opencode-session-directory opencode-session--session) "/tmp/work")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; form questions

(defun opencode-handlers-test--form ()
  "Return a Form.Detail alist exercising all field types."
  '((id . "frm_test1")
    (sessionID . "ses_form")
    (title . "Deploy checklist")
    (fields .
     [((key . "env") (type . "string") (title . "Environment")
       (required . t)
       (options . [((value . "prod") (label . "Production"))
                   ((value . "staging") (label . "Staging"))]))
      ((key . "count") (type . "integer") (title . "Replicas") (default . 3))
      ((key . "dryRun") (type . "boolean") (title . "Dry run"))
      ((key . "regions") (type . "multiselect") (title . "Regions")
       (options . [((value . "us-east-1") (label . "US East"))
                   ((value . "eu-west-1") (label . "EU West"))]))
      ((key . "token") (type . "external") (title . "Token URL")
       (url . "https://example.com/oauth"))
      ((key . "secret") (type . "string") (title . "Secret") (hidden . t))
      ((key . "notes") (type . "string") (title . "Notes")
       (when . [((key . "env") (op . "neq") (value . "staging"))]))])))

(ert-deftest test-opencode-handlers/form-condition-p ()
  "Form WHEN conditions match on eq and invert on neq."
  (let ((answers '(("env" . "prod"))))
    (should (opencode-session--form-condition-p
             '((key . "env") (op . "eq") (value . "prod")) answers))
    (should-not (opencode-session--form-condition-p
                 '((key . "env") (op . "eq") (value . "staging")) answers))
    (should-not (opencode-session--form-condition-p
                 '((key . "env") (op . "eq") (value . "prod")) nil))
    (should (opencode-session--form-condition-p
             '((key . "env") (op . "neq") (value . "staging")) answers))
    (should-not (opencode-session--form-condition-p
                 '((key . "env") (op . "neq") (value . "prod")) answers))))

(ert-deftest test-opencode-handlers/form-field-visible-p ()
  "Hidden fields and mismatched WHEN conditions hide a field."
  (let ((answers '(("env" . "prod"))))
    (should (opencode-session--form-field-visible-p
             '((key . "a") (type . "string")) answers))
    (should-not (opencode-session--form-field-visible-p
                 '((key . "a") (type . "string") (hidden . t)) answers))
    (should (opencode-session--form-field-visible-p
             '((key . "a") (type . "string")
               (when . [((key . "env") (op . "neq") (value . "staging"))]))
             answers))
    (should-not (opencode-session--form-field-visible-p
                 '((key . "a") (type . "string")
                   (when . [((key . "env") (op . "eq") (value . "staging"))]))
                 answers))))

(ert-deftest test-opencode-handlers/form-option-helpers ()
  "Option labels fall back to values and vice versa."
  (should (equal (opencode-session--form-option-label
                  '((value . "prod") (label . "Production")))
                 "Production"))
  (should (equal (opencode-session--form-option-label '((value . "prod")))
                 "prod"))
  (should (equal (opencode-session--form-option-value
                  '((value . "prod") (label . "Production")))
                 "prod"))
  (should (equal (opencode-session--form-option-value '((label . "X")))
                 "X"))
  (should (equal (opencode-session--form-option-labels
                  '(((value . "a")) ((value . "b"))))
                 '("a" "b")))
  (should (equal (opencode-session--form-option-for-label
                  '(((value . "prod") (label . "Production")))
                  "Production")
                 '((value . "prod") (label . "Production")))))

(ert-deftest test-opencode-handlers/form-string-valid-p ()
  "String answers honor minLength, maxLength and pattern."
  (should (opencode-session--form-string-valid-p '((minLength . 3)) "abc"))
  (should-not (opencode-session--form-string-valid-p '((minLength . 4)) "abc"))
  (should (opencode-session--form-string-valid-p '((maxLength . 3)) "abc"))
  (should-not (opencode-session--form-string-valid-p '((maxLength . 2)) "abc"))
  (should (opencode-session--form-string-valid-p '((pattern . "^[a-z]+$")) "ab"))
  (should-not (opencode-session--form-string-valid-p '((pattern . "^[a-z]+$")) "AB"))
  ;; Invalid Emacs regexps are ignored rather than signalled.
  (should (opencode-session--form-string-valid-p '((pattern . "[unclosed")) "ab")))

(ert-deftest test-opencode-handlers/form-answers-collected ()
  "Answers are collected per field type with JSON-safe values."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "Production"))
            ((symbol-function 'read-number) (lambda (&rest _) 5))
            ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
            ((symbol-function 'completing-read-multiple)
             (lambda (&rest _) '("US East" "EU West")))
            ((symbol-function 'read-string) (lambda (&rest _) "ok")))
    (let ((answers (opencode-session--form-answers
                    (opencode-handlers-test--form))))
      (should (equal (alist-get "env" answers nil nil #'equal) "prod"))
      (should (equal (alist-get "count" answers nil nil #'equal) 5))
      (should (eq (alist-get "dryRun" answers nil nil #'equal) :json-false))
      (should (equal (alist-get "regions" answers nil nil #'equal)
                     ["us-east-1" "eu-west-1"]))
      (should (eq (alist-get "token" answers nil nil #'equal) t))
      (should (equal (alist-get "notes" answers nil nil #'equal) "ok"))
      (should-not (assoc "secret" answers))
      (should (= (length answers) 6)))))

(ert-deftest test-opencode-handlers/form-created-prompts-and-renders ()
  "form.created renders a pending block and prompts via the client."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-form-test*"))
        (connection 'conn-form)
        (replied nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local opencode-session--connection connection))
          (puthash "ses_form" buffer opencode-session--buffers)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "Production"))
                    ((symbol-function 'read-number) (lambda (&rest _) 5))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                    ((symbol-function 'completing-read-multiple)
                     (lambda (&rest _) '("US East" "EU West")))
                    ((symbol-function 'read-string) (lambda (&rest _) "ok"))
                    ((symbol-function 'opencode-client-form-reply)
                     (lambda (conn session-id form-id answer &rest args)
                       (setq replied (list conn session-id form-id answer))
                       (let ((success (plist-get args :success)))
                         (when success (funcall success :data nil))))))
            (opencode-handlers-test--with-sync-timer
              (opencode-session--handle-form-created
               `((data . ((form . ,(opencode-handlers-test--form)))))
               (list :connection connection))))
          (should (equal (nth 0 replied) connection))
          (should (equal (nth 1 replied) "ses_form"))
          (should (equal (nth 2 replied) "frm_test1"))
          (should (equal (nth 3 replied)
                         '(("env" . "prod")
                           ("count" . 5)
                           ("dryRun" . :json-false)
                           ("regions" . ["us-east-1" "eu-west-1"])
                           ("token" . t)
                           ("notes" . "ok"))))
          (with-current-buffer buffer
            (should (= (length opencode-session--forms) 1))
            (should (eq (opencode-session--form-state-status
                         (car opencode-session--forms))
                        'answered))
            (should (opencode-session--form-state-self-answered
                     (car opencode-session--forms)))))
      (kill-buffer buffer))))

(ert-deftest test-opencode-handlers/form-duplicate-created-schedules-one-prompt ()
  "Repeated form.created for the same form schedules one prompt."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-form-dup*"))
        (connection 'conn-form)
        (timers 0))
    (unwind-protect
        (progn
          (puthash "ses_form" buffer opencode-session--buffers)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _args)
                       (cl-incf timers)
                       nil)))
            (opencode-session--handle-form-created
             `((data . ((form . ,(opencode-handlers-test--form)))))
             (list :connection connection))
            (opencode-session--handle-form-created
             `((data . ((form . ,(opencode-handlers-test--form)))))
             (list :connection connection)))
          (should (= timers 1))
          (with-current-buffer buffer
            (should (= (length opencode-session--forms) 1))))
      (kill-buffer buffer))))

(ert-deftest test-opencode-handlers/form-replied-settles-buffer ()
  "form.replied settles the rendered block with the event answer."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-form-replied*"))
        (connection 'conn-form))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local opencode-session--connection connection))
          (puthash "ses_form" buffer opencode-session--buffers)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "Production"))
                    ((symbol-function 'read-number) (lambda (&rest _) 5))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                    ((symbol-function 'completing-read-multiple)
                     (lambda (&rest _) '("US East" "EU West")))
                    ((symbol-function 'read-string) (lambda (&rest _) "ok"))
                    ((symbol-function 'opencode-client-form-reply)
                     (lambda (&rest _args) nil)))
            (opencode-handlers-test--with-sync-timer
              (opencode-session--handle-form-created
               `((data . ((form . ,(opencode-handlers-test--form)))))
               (list :connection connection))))
          ;; A remote client settles the form; the rendered block
          ;; updates even though we never got a reply success callback.
          (opencode-session--handle-form-replied
           `((data . ((id . "frm_test1")
                      (sessionID . "ses_form")
                      (answer . (("env" . "prod") ("count" . 5))))))
           (list :connection connection))
          (with-current-buffer buffer
            (let ((form-state (car opencode-session--forms)))
              (should (eq (opencode-session--form-state-status form-state)
                          'answered))
              (should (equal (opencode-session--form-state-answers form-state)
                             '(("env" . "prod") ("count" . 5)))))))
      (kill-buffer buffer))))

(ert-deftest test-opencode-handlers/form-cancelled-settles-buffer ()
  "form.cancelled settles the rendered block and drops the prompt."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-form-cancelled*"))
        (connection 'conn-form))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local opencode-session--connection connection))
          (puthash "ses_form" buffer opencode-session--buffers)
          ;; Prompt timer never fires before the cancel arrives.
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _args) nil)))
            (opencode-session--handle-form-created
             `((data . ((form . ,(opencode-handlers-test--form)))))
             (list :connection connection)))
          (should (gethash "frm_test1"
                           (opencode-session--prompt-table connection)))
          (opencode-session--handle-form-cancelled
           `((data . ((id . "frm_test1") (sessionID . "ses_form"))))
           (list :connection connection))
          (with-current-buffer buffer
            (should (eq (opencode-session--form-state-status
                         (car opencode-session--forms))
                        'cancelled)))
          (should-not (when-let* ((table (opencode-session--prompt-table
                                          connection)))
                        (gethash "frm_test1" table))))
      (kill-buffer buffer))))

(ert-deftest test-opencode-handlers/form-quit-cancels-form ()
  "Quitting a field prompt cancels the form on the server."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-form-quit*"))
        (connection 'conn-form)
        (cancelled nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local opencode-session--connection connection))
          (puthash "ses_form" buffer opencode-session--buffers)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) (signal 'quit nil)))
                    ((symbol-function 'opencode-client-form-cancel)
                     (lambda (conn session-id form-id &rest _args)
                       (setq cancelled (list conn session-id form-id)))))
            (opencode-handlers-test--with-sync-timer
              (opencode-session--handle-form-created
               `((data . ((form . ,(opencode-handlers-test--form)))))
               (list :connection connection))))
          (should (equal cancelled
                         '(conn-form "ses_form" "frm_test1")))
          (with-current-buffer buffer
            (should (eq (opencode-session--form-state-status
                         (car opencode-session--forms))
                        'pending))))
      (kill-buffer buffer))))

(ert-deftest test-opencode-handlers/form-sync-prompts-pending ()
  "sync-forms fetches pending forms and prompts for each."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-form-sync*"))
        (connection 'conn-form)
        (timers 0))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local opencode-session--connection connection))
          (puthash "ses_form" buffer opencode-session--buffers)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _args)
                       (cl-incf timers)
                       nil))
                    ((symbol-function 'opencode-client-form-list)
                     (lambda (conn session-id &rest args)
                       (let ((success (plist-get args :success)))
                         (funcall success :data
                                  (list (opencode-handlers-test--form)))))))
            (opencode-session--sync-forms connection "ses_form"))
          (should (= timers 1))
          (with-current-buffer buffer
            (should (= (length opencode-session--forms) 1))))
      (kill-buffer buffer))))

(provide 'emacs-opencode-session-handlers-test)

;;; emacs-opencode-session-handlers-test.el ends here