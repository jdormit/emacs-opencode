;;; emacs-opencode-session-handlers-test.el --- Tests for SSE event handlers  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-opencode-session-handlers)

;;; session-error-text

(ert-deftest test-opencode-handlers/error-text-detail-message ()
  "Extract detail message from nested alist."
  (should (equal (opencode-session--session-error-text
                  '((name . "SomeError")
                    (data . ((message . "something went wrong")))))
                 "something went wrong")))

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

;;; status-from-info

(ert-deftest test-opencode-handlers/status-from-info-idle ()
  "Build an idle status from info."
  (let ((status (opencode-session--status-from-info '((type . "idle")))))
    (should (opencode-status-p status))
    (should (equal (opencode-status-type status) "idle"))
    (should (null (opencode-status-attempt status)))
    (should (null (opencode-status-message status)))
    (should (null (opencode-status-next status)))))

(ert-deftest test-opencode-handlers/status-from-info-retry-full ()
  "Build a retry status with all fields populated."
  (let ((status (opencode-session--status-from-info
                 '((type . "retry")
                   (attempt . 2)
                   (message . "Provider is overloaded")
                   (next . 1700000000000)))))
    (should (equal (opencode-status-type status) "retry"))
    (should (= (opencode-status-attempt status) 2))
    (should (equal (opencode-status-message status) "Provider is overloaded"))
    (should (= (opencode-status-next status) 1700000000000))))

(ert-deftest test-opencode-handlers/status-from-info-retry-empty-message ()
  "Empty retry message is normalized to nil."
  (let ((status (opencode-session--status-from-info
                 '((type . "retry")
                   (attempt . 1)
                   (message . "")))))
    (should (equal (opencode-status-type status) "retry"))
    (should (null (opencode-status-message status)))))

(ert-deftest test-opencode-handlers/status-from-info-missing-type ()
  "Missing type defaults to idle."
  (let ((status (opencode-session--status-from-info nil)))
    (should (equal (opencode-status-type status) "idle"))))

;;; permission helpers

(ert-deftest test-opencode-handlers/permission-patterns-vector ()
  "Extract patterns from vector."
  (should (equal (opencode-session--permission-patterns
                  '((patterns . ["*.el" "*.org"])))
                 '("*.el" "*.org"))))

(ert-deftest test-opencode-handlers/permission-patterns-list ()
  "Extract patterns from list."
  (should (equal (opencode-session--permission-patterns
                  '((patterns . ("*.el"))))
                 '("*.el"))))

(ert-deftest test-opencode-handlers/permission-patterns-nil ()
  "Return nil when no patterns."
  (should (null (opencode-session--permission-patterns '((other . "x"))))))

(ert-deftest test-opencode-handlers/permission-detail-read ()
  "Detail for read permission."
  (should (equal (opencode-session--permission-detail
                  '((permission . "read")
                    (metadata . ((filePath . "foo.el")))))
                 "read foo.el")))

(ert-deftest test-opencode-handlers/permission-detail-edit ()
  "Detail for edit permission."
  (should (equal (opencode-session--permission-detail
                  '((permission . "edit")
                    (metadata . ((filepath . "bar.el")))))
                 "edit bar.el")))

(ert-deftest test-opencode-handlers/permission-detail-bash ()
  "Detail for bash permission."
  (should (equal (opencode-session--permission-detail
                  '((permission . "bash")
                    (metadata . ((command . "ls -la")))))
                 "ls -la")))

(ert-deftest test-opencode-handlers/permission-detail-bash-with-description ()
  "Detail for bash permission with description."
  (should (equal (opencode-session--permission-detail
                  '((permission . "bash")
                    (metadata . ((description . "list files")
                                 (command . "ls -la")))))
                 "list files (ls -la)")))

(ert-deftest test-opencode-handlers/permission-detail-glob ()
  "Detail for glob permission."
  (should (equal (opencode-session--permission-detail
                  '((permission . "glob")
                    (metadata . ((pattern . "*.el")))))
                 "glob *.el")))

(ert-deftest test-opencode-handlers/permission-detail-external-directory ()
  "Detail for external directory with pattern."
  (should (equal (opencode-session--permission-detail
                  '((permission . "external_directory")
                    (patterns . ["/tmp/other"])))
                 "access external directory /tmp/other")))

(ert-deftest test-opencode-handlers/permission-detail-nil ()
  "Return nil when no metadata matches."
  (should (null (opencode-session--permission-detail
                 '((permission . "unknown")
                   (metadata . nil))))))

(ert-deftest test-opencode-handlers/permission-prompt-label ()
  "Build a prompt label."
  (let ((result (opencode-session--permission-prompt-label
                 '((permission . "read")
                   (metadata . ((filePath . "foo.el")))))))
    (should (string-match-p "read foo\\.el" result))
    (should (string-suffix-p ": " result))))

(ert-deftest test-opencode-handlers/permission-prompt-label-fallback ()
  "Prompt label falls back to kind."
  (let ((result (opencode-session--permission-prompt-label
                 '((permission . "custom")))))
    (should (string-match-p "use custom" result))))

;;; question helpers

(ert-deftest test-opencode-handlers/question-list-vector ()
  "Normalize question vector."
  (should (equal (opencode-session--question-list [1 2]) '(1 2))))

(ert-deftest test-opencode-handlers/question-list-nil ()
  "Return nil for nil."
  (should (null (opencode-session--question-list nil))))

(ert-deftest test-opencode-handlers/question-options ()
  "Extract option labels."
  (should (equal (opencode-session--question-options
                  '((options . [((label . "Yes")) ((label . "No"))])))
                 '("Yes" "No"))))

(ert-deftest test-opencode-handlers/question-multiple-p-true ()
  "Detect multiple-answer questions."
  (should (opencode-session--question-multiple-p '((multiple . t)))))

(ert-deftest test-opencode-handlers/question-multiple-p-false ()
  "Non-multiple returns nil."
  (should (null (opencode-session--question-multiple-p '((multiple . nil))))))

(ert-deftest test-opencode-handlers/question-custom-p-true ()
  "Detect custom-answer questions."
  (should (opencode-session--question-custom-p '((custom . t)))))

(ert-deftest test-opencode-handlers/question-custom-p-false ()
  "Non-custom returns nil."
  (should (null (opencode-session--question-custom-p '((custom . :json-false))))))

(ert-deftest test-opencode-handlers/question-custom-p-nil ()
  "Nil custom returns nil."
  (should (null (opencode-session--question-custom-p '((custom . nil))))))

(ert-deftest test-opencode-handlers/question-prompt-label ()
  "Build question prompt."
  (should (equal (opencode-session--question-prompt-label
                  '((header . "Auth") (question . "Enter key")))
                 "OpenCode Auth: Enter key ")))

(ert-deftest test-opencode-handlers/question-prompt-label-no-header ()
  "Build question prompt without header."
  (should (equal (opencode-session--question-prompt-label
                  '((question . "Choose one")))
                 "OpenCode: Choose one ")))

;;; event-file-paths

(ert-deftest test-opencode-handlers/compaction-started-renders-marker ()
  "Compaction start events render a marker without removing history."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-compaction-start-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq-local opencode-session--session (opencode-session-create :id "s1"))
          (setq-local opencode-session--messages
                      (list (opencode-message-create :id "old" :role "assistant" :text "old text")))
          (opencode-session--ensure-markers)
          (opencode-session--ensure-input-region)
          (opencode-session--render-buffer)
          (puthash "s1" buffer opencode-session--buffers)
          (opencode-session--handle-compaction-started
           "session.next.compaction.started"
           '((properties . ((sessionID . "s1")
                            (messageID . "m-compact")
                            (reason . "manual")))))
          (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "old text" contents))
            (should (string-match-p "Session compacting" contents))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-opencode-handlers/compaction-ended-updates-marker ()
  "Compaction end events update the existing marker."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-compaction-end-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq-local opencode-session--session (opencode-session-create :id "s1"))
          (opencode-session--ensure-markers)
          (opencode-session--ensure-input-region)
          (puthash "s1" buffer opencode-session--buffers)
          (opencode-session--handle-compaction-started
           "session.next.compaction.started"
           '((properties . ((sessionID . "s1")
                            (messageID . "m-compact")
                            (reason . "manual")))))
          (opencode-session--handle-compaction-ended
           "session.next.compaction.ended"
           '((properties . ((sessionID . "s1")
                            (messageID . "m-compact")
                            (reason . "manual")
                            (timestamp . "done")))))
          (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "Session compacted" contents))
            (should (= (length opencode-session--messages) 1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-opencode-handlers/event-file-paths-vector ()
  "Extract paths from vector."
  (should (equal (opencode-session--event-file-paths
                  '((properties . ((paths . ["a.el" "b.el"])))))
                 '("a.el" "b.el"))))

(ert-deftest test-opencode-handlers/event-file-paths-list ()
  "Extract paths from list."
  (should (equal (opencode-session--event-file-paths
                  '((properties . ((paths . ("a.el" "b.el"))))))
                 '("a.el" "b.el"))))

(ert-deftest test-opencode-handlers/event-file-paths-nil ()
  "Return nil when no paths found."
  ;; Note: (listp nil) is t in Elisp, so when paths/files are absent
  ;; the function returns nil via the (listp paths) branch.
  (should (null (opencode-session--event-file-paths
                 '((properties . ((other . "x"))))))))

;;; permission/question reply routing by connection

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
reply must go to the originating connection, not an arbitrary buffer."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (event-conn 'conn-asking)
        (other-conn 'conn-other)
        (other-buf (generate-new-buffer " *oc-test-other-conn*"))
        (replied-conn :unset)
        (replied-id :unset))
    (unwind-protect
        (progn
          ;; A buffer belonging to a DIFFERENT connection is registered.
          (with-current-buffer other-buf
            (setq-local opencode-session--connection other-conn))
          (puthash "other-session" other-buf opencode-session--buffers)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "Allow once"))
                    ((symbol-function 'opencode-client-permission-reply)
                     (lambda (conn request-id _reply &rest _args)
                       (setq replied-conn conn
                             replied-id request-id))))
            (opencode-handlers-test--with-sync-timer
              (opencode-session--handle-permission-asked
               "permission.asked"
               '((properties . ((id . "per_123")
                                 (sessionID . "sub_agent_session")
                                 (permission . "read")
                                 (metadata . ((filePath . "x.el"))))))
               (list :connection event-conn))))
          (should (eq replied-conn event-conn))
          (should (equal replied-id "per_123")))
      (kill-buffer other-buf))))

(ert-deftest test-opencode-handlers/permission-reply-no-buffer-still-replies ()
  "With no buffer at all, the reply still goes via the event connection."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (event-conn 'conn-asking)
        (replied-conn :unset))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Allow always"))
              ((symbol-function 'opencode-client-permission-reply)
               (lambda (conn _request-id reply &rest _args)
                 (setq replied-conn (cons conn reply)))))
      (opencode-handlers-test--with-sync-timer
        (opencode-session--handle-permission-asked
         "permission.asked"
         '((properties . ((id . "per_456")
                           (sessionID . "sub_agent_session")
                           (permission . "read"))))
         (list :connection event-conn))))
    (should (equal replied-conn (cons event-conn "always")))))

(ert-deftest test-opencode-handlers/question-reply-routes-to-event-connection ()
  "A subagent question reply is sent via the connection that asked."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (event-conn 'conn-asking)
        (replied-conn :unset)
        (replied-id :unset))
    (cl-letf (((symbol-function 'opencode-session--question-answers)
               (lambda (&rest _) '(("Yes"))))
              ((symbol-function 'opencode-client-question-reply)
               (lambda (conn request-id _answers &rest _args)
                 (setq replied-conn conn
                       replied-id request-id))))
      (opencode-handlers-test--with-sync-timer
        (opencode-session--handle-question-asked
         "question.asked"
         '((properties . ((id . "qst_789")
                           (sessionID . "sub_agent_session")
                           (questions . [((question . "Pick") (options . [((label . "Yes"))]))]))))
         (list :connection event-conn))))
    (should (eq replied-conn event-conn))
    (should (equal replied-id "qst_789"))))

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
       "permission.asked"
       '((properties . ((id . "per_remote")
                        (sessionID . "session"))))
       (list :connection connection))
      (opencode-session--handle-prompt-resolved
       "permission.replied"
       '((properties . ((requestID . "per_remote"))))
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
        (opencode-session--handle-question-asked
         "question.asked"
         '((properties . ((id . "que_duplicate")
                          (sessionID . "session")
                          (questions . []))))
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
                  "permission.replied"
                  '((properties . ((requestID . "per_remote"))))
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
         "permission.asked"
         '((properties . ((id . "per_remote")
                          (sessionID . "session")
                          (permission . "read"))))
         (list :connection connection))))
    (should (= reply-count 0))))

(ert-deftest test-opencode-handlers/remote-question-rejection-dismisses-prompt ()
  "A remote question rejection aborts the prompt without rejecting again."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (connection 'conn)
        (reject-count 0))
    (cl-letf (((symbol-function 'opencode-session--question-answers)
               (lambda (&rest _args)
                 (opencode-session--handle-prompt-resolved
                  "question.rejected"
                  '((properties . ((requestID . "que_remote"))))
                  (list :connection connection))))
              ((symbol-function 'opencode-session--prompt-active-p)
               (lambda (_state) t))
              ((symbol-function 'abort-recursive-edit)
               (lambda () (signal 'quit nil)))
              ((symbol-function 'opencode-client-question-reject)
               (lambda (&rest _args)
                 (setq reject-count (1+ reject-count)))))
      (opencode-handlers-test--with-sync-timer
        (opencode-session--handle-question-asked
         "question.asked"
         '((properties . ((id . "que_remote")
                          (sessionID . "session")
                          (questions . []))))
         (list :connection connection))))
    (should (= reject-count 0))))

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
       "permission.replied"
       '((properties . ((requestID . "per_other"))))
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
               (lambda (_connection _request-id value &rest _args)
                 (setq reply value))))
      (opencode-handlers-test--with-sync-timer
        (opencode-session--handle-permission-asked
         "permission.asked"
         '((properties . ((id . "per_quit")
                          (sessionID . "session")
                          (permission . "read"))))
         (list :connection connection))))
    (should (equal reply "reject"))))

(ert-deftest test-opencode-handlers/user-quit-still-rejects-question ()
  "Quitting a question prompt still calls the rejection endpoint."
  (let ((opencode-session--pending-prompts (make-hash-table :test #'eq))
        (connection 'conn)
        (rejected nil))
    (cl-letf (((symbol-function 'opencode-session--question-answers)
               (lambda (&rest _args) (signal 'quit nil)))
              ((symbol-function 'opencode-client-question-reject)
               (lambda (&rest _args)
                 (setq rejected t))))
      (opencode-handlers-test--with-sync-timer
        (opencode-session--handle-question-asked
         "question.asked"
         '((properties . ((id . "que_quit")
                          (sessionID . "session")
                          (questions . []))))
         (list :connection connection))))
    (should rejected)))

(provide 'emacs-opencode-session-handlers-test)

;;; emacs-opencode-session-handlers-test.el ends here
