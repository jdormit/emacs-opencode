;;; emacs-opencode-acp-test.el --- Tests for ACP transport  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-opencode-acp)

;;; JSON serialization

(ert-deftest test-opencode-acp/json-parse-simple ()
  "Parse a simple JSON object to alist."
  (let ((result (opencode-acp--json-parse "{\"id\":1,\"method\":\"test\"}")))
    (should (equal (alist-get 'id result) 1))
    (should (equal (alist-get 'method result) "test"))))

(ert-deftest test-opencode-acp/json-parse-null ()
  "Parse JSON null as nil."
  (let ((result (opencode-acp--json-parse "{\"value\":null}")))
    (should (null (alist-get 'value result)))))

(ert-deftest test-opencode-acp/json-parse-false ()
  "Parse JSON false as nil."
  (let ((result (opencode-acp--json-parse "{\"value\":false}")))
    (should (null (alist-get 'value result)))))

(ert-deftest test-opencode-acp/json-parse-array ()
  "Parse JSON arrays as lists."
  (let ((result (opencode-acp--json-parse "{\"items\":[1,2,3]}")))
    (should (equal (alist-get 'items result) '(1 2 3)))))

(ert-deftest test-opencode-acp/json-serialize-alist ()
  "Serialize an alist to JSON."
  (let ((result (opencode-acp--json-serialize '((id . 1) (method . "test")))))
    (should (stringp result))
    ;; Parse it back to verify round-trip
    (let ((parsed (opencode-acp--json-parse result)))
      (should (equal (alist-get 'id parsed) 1))
      (should (equal (alist-get 'method parsed) "test")))))

;;; Process filter — ndjson framing

(ert-deftest test-opencode-acp/filter-complete-line ()
  "Process filter handles a complete JSON line."
  (let* ((received nil)
         ;; Mock process
         (proc (start-process "test" nil "true"))
         (filter (opencode-acp--make-filter proc)))
    ;; Override dispatch to capture messages
    (cl-letf (((symbol-function 'opencode-acp--handle-message)
               (lambda (_proc msg) (push msg received))))
      (funcall filter proc "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}\n"))
    (should (= (length received) 1))
    (should (equal (alist-get 'id (car received)) 1))
    (delete-process proc)))

(ert-deftest test-opencode-acp/filter-partial-lines ()
  "Process filter handles data split across multiple chunks."
  (let* ((received nil)
         (proc (start-process "test" nil "true"))
         (filter (opencode-acp--make-filter proc)))
    (cl-letf (((symbol-function 'opencode-acp--handle-message)
               (lambda (_proc msg) (push msg received))))
      ;; First chunk: partial line
      (funcall filter proc "{\"jsonrpc\":\"2.0\",")
      (should (= (length received) 0))
      ;; Second chunk: complete the line
      (funcall filter proc "\"id\":1,\"result\":{}}\n"))
    (should (= (length received) 1))
    (should (equal (alist-get 'id (car received)) 1))
    (delete-process proc)))

(ert-deftest test-opencode-acp/filter-multiple-lines ()
  "Process filter handles multiple lines in one chunk."
  (let* ((received nil)
         (proc (start-process "test" nil "true"))
         (filter (opencode-acp--make-filter proc)))
    (cl-letf (((symbol-function 'opencode-acp--handle-message)
               (lambda (_proc msg) (push msg received))))
      (funcall filter proc
               (concat "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}\n"
                       "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{}}\n")))
    (should (= (length received) 2))
    ;; received is in reverse order due to push
    (should (equal (alist-get 'id (nth 1 received)) 1))
    (should (equal (alist-get 'id (nth 0 received)) 2))
    (delete-process proc)))

(ert-deftest test-opencode-acp/filter-empty-lines ()
  "Process filter ignores empty lines."
  (let* ((received nil)
         (proc (start-process "test" nil "true"))
         (filter (opencode-acp--make-filter proc)))
    (cl-letf (((symbol-function 'opencode-acp--handle-message)
               (lambda (_proc msg) (push msg received))))
      (funcall filter proc "\n\n{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}\n\n"))
    (should (= (length received) 1))
    (delete-process proc)))

;;; Response handling

(ert-deftest test-opencode-acp/handle-response-success ()
  "Handle a successful response to a pending request."
  (let ((result nil)
        (opencode-acp--pending-requests (make-hash-table :test 'eql)))
    (puthash 1 (list (lambda (r) (setq result r)) nil nil)
             opencode-acp--pending-requests)
    (opencode-acp--handle-response
     '((jsonrpc . "2.0") (id . 1) (result . ((foo . "bar")))))
    (should (equal (alist-get 'foo result) "bar"))
    (should (= (hash-table-count opencode-acp--pending-requests) 0))))

(ert-deftest test-opencode-acp/handle-response-error ()
  "Handle an error response to a pending request."
  (let ((err nil)
        (opencode-acp--pending-requests (make-hash-table :test 'eql)))
    (puthash 1 (list nil (lambda (e) (setq err e)) nil)
             opencode-acp--pending-requests)
    (opencode-acp--handle-response
     '((jsonrpc . "2.0") (id . 1)
       (error . ((code . -32603) (message . "Internal error")))))
    (should (equal (alist-get 'code err) -32603))
    (should (equal (alist-get 'message err) "Internal error"))
    (should (= (hash-table-count opencode-acp--pending-requests) 0))))

(ert-deftest test-opencode-acp/handle-response-cancels-timer ()
  "Response handling cancels the timeout timer."
  (let* ((timer (run-at-time 999 nil #'ignore))
         (opencode-acp--pending-requests (make-hash-table :test 'eql)))
    (puthash 1 (list #'ignore nil timer) opencode-acp--pending-requests)
    (opencode-acp--handle-response
     '((jsonrpc . "2.0") (id . 1) (result . nil)))
    ;; Timer should be cancelled (not pending)
    (should-not (memq timer timer-list))))

(ert-deftest test-opencode-acp/handle-response-unknown-id ()
  "Response for unknown ID is silently ignored."
  (let ((opencode-acp--pending-requests (make-hash-table :test 'eql)))
    ;; Should not error
    (opencode-acp--handle-response
     '((jsonrpc . "2.0") (id . 999) (result . nil)))))

;;; Notification handling

(ert-deftest test-opencode-acp/handle-notification ()
  "Dispatch a notification to registered handlers."
  (let ((received nil)
        (opencode-acp--notification-handlers nil))
    (opencode-acp-register-notification-handler
     "session/update"
     (lambda (_method params) (push params received)))
    (opencode-acp--handle-notification
     '((jsonrpc . "2.0")
       (method . "session/update")
       (params . ((sessionId . "s1") (update . ((sessionUpdate . "test")))))))
    (should (= (length received) 1))
    (should (equal (alist-get 'sessionId (car received)) "s1"))))

(ert-deftest test-opencode-acp/notification-no-handler ()
  "Notification with no handler is silently ignored."
  (let ((opencode-acp--notification-handlers nil))
    ;; Should not error
    (opencode-acp--handle-notification
     '((jsonrpc . "2.0")
       (method . "unknown/method")
       (params . nil)))))

(ert-deftest test-opencode-acp/notification-multiple-handlers ()
  "Multiple handlers for the same method are all called."
  (let ((calls 0)
        (opencode-acp--notification-handlers nil))
    (opencode-acp-register-notification-handler
     "test/event" (lambda (_m _p) (cl-incf calls)))
    (opencode-acp-register-notification-handler
     "test/event" (lambda (_m _p) (cl-incf calls)))
    (opencode-acp--handle-notification
     '((jsonrpc . "2.0") (method . "test/event") (params . nil)))
    (should (= calls 2))))

;;; Agent-to-client request handling

(ert-deftest test-opencode-acp/agent-request-success ()
  "Handle an agent-to-client request and send success response."
  (let ((sent nil)
        (opencode-acp--request-handlers nil)
        (opencode-acp-log-io nil))
    (opencode-acp-register-request-handler
     "fs/read_text_file"
     (lambda (_params) '((content . "file contents"))))
    (cl-letf (((symbol-function 'opencode-acp--send)
               (lambda (_proc msg) (setq sent msg))))
      (opencode-acp--handle-agent-request
       nil
       '((jsonrpc . "2.0") (id . 5) (method . "fs/read_text_file")
         (params . ((path . "/tmp/test.txt"))))))
    (should sent)
    (should (equal (alist-get 'id sent) 5))
    (should (alist-get 'result sent))))

(ert-deftest test-opencode-acp/agent-request-handler-error ()
  "Send error response when handler signals an error."
  (let ((sent nil)
        (opencode-acp--request-handlers nil)
        (opencode-acp-log-io nil))
    (opencode-acp-register-request-handler
     "fs/read_text_file"
     (lambda (_params) (error "File not found")))
    (cl-letf (((symbol-function 'opencode-acp--send)
               (lambda (_proc msg) (setq sent msg))))
      (opencode-acp--handle-agent-request
       nil
       '((jsonrpc . "2.0") (id . 5) (method . "fs/read_text_file")
         (params . ((path . "/tmp/missing.txt"))))))
    (should sent)
    (should (equal (alist-get 'id sent) 5))
    (should (alist-get 'error sent))
    (should (equal (alist-get 'code (alist-get 'error sent)) -32603))))

(ert-deftest test-opencode-acp/agent-request-no-handler ()
  "Send method-not-found error when no handler is registered."
  (let ((sent nil)
        (opencode-acp--request-handlers nil)
        (opencode-acp-log-io nil))
    (cl-letf (((symbol-function 'opencode-acp--send)
               (lambda (_proc msg) (setq sent msg))))
      (opencode-acp--handle-agent-request
       nil
       '((jsonrpc . "2.0") (id . 5) (method . "unknown/method")
         (params . nil))))
    (should sent)
    (should (equal (alist-get 'code (alist-get 'error sent)) -32601))))

;;; Message dispatch

(ert-deftest test-opencode-acp/dispatch-response ()
  "Message with id but no method is a response."
  (let ((opencode-acp--pending-requests (make-hash-table :test 'eql))
        (called nil))
    (puthash 1 (list (lambda (_) (setq called t)) nil nil)
             opencode-acp--pending-requests)
    (opencode-acp--handle-message nil '((id . 1) (result . nil)))
    (should called)))

(ert-deftest test-opencode-acp/dispatch-notification ()
  "Message with method but no id is a notification."
  (let ((opencode-acp--notification-handlers nil)
        (called nil))
    (opencode-acp-register-notification-handler
     "test" (lambda (_m _p) (setq called t)))
    (opencode-acp--handle-message nil '((method . "test") (params . nil)))
    (should called)))

(ert-deftest test-opencode-acp/dispatch-agent-request ()
  "Message with both id and method is an agent-to-client request."
  (let ((opencode-acp--request-handlers nil)
        (sent nil)
        (opencode-acp-log-io nil))
    (opencode-acp-register-request-handler
     "test" (lambda (_params) '((ok . t))))
    (cl-letf (((symbol-function 'opencode-acp--send)
               (lambda (_proc msg) (setq sent msg))))
      (opencode-acp--handle-message
       nil '((id . 1) (method . "test") (params . nil))))
    (should sent)
    (should (equal (alist-get 'id sent) 1))))

;;; Handler macros

(ert-deftest test-opencode-acp/define-notification-handler ()
  "Macro registers a notification handler."
  (let ((opencode-acp--notification-handlers nil))
    (opencode-acp-define-notification-handler test-macro-handler
        "test/macro" (_method _params)
      nil)
    (should (alist-get "test/macro" opencode-acp--notification-handlers
                       nil nil #'string=))))

(ert-deftest test-opencode-acp/define-request-handler ()
  "Macro registers a request handler."
  (let ((opencode-acp--request-handlers nil))
    (opencode-acp-define-request-handler test-macro-req-handler
        "test/macro-req" (_params)
      '((result . t)))
    (should (alist-get "test/macro-req" opencode-acp--request-handlers
                       nil nil #'string=))))

;;; Request sending

(ert-deftest test-opencode-acp/request-increments-id ()
  "Each request gets a unique incrementing ID."
  (let ((opencode-acp--next-id 0)
        (opencode-acp--pending-requests (make-hash-table :test 'eql))
        (opencode-acp-request-timeout nil)
        (opencode-acp-log-io nil)
        (ids nil))
    (cl-letf (((symbol-function 'opencode-acp--send)
               (lambda (_proc msg) (push (alist-get 'id msg) ids))))
      (opencode-acp-request nil "test1" nil)
      (opencode-acp-request nil "test2" nil))
    (should (equal (nreverse ids) '(0 1)))))

(ert-deftest test-opencode-acp/request-registers-pending ()
  "Request registers in the pending table."
  (let ((opencode-acp--next-id 0)
        (opencode-acp--pending-requests (make-hash-table :test 'eql))
        (opencode-acp-request-timeout nil)
        (opencode-acp-log-io nil))
    (cl-letf (((symbol-function 'opencode-acp--send) #'ignore))
      (opencode-acp-request nil "test" nil :success #'ignore))
    (should (= (hash-table-count opencode-acp--pending-requests) 1))
    (should (gethash 0 opencode-acp--pending-requests))))

;;; Notify

(ert-deftest test-opencode-acp/notify-no-id ()
  "Notification messages have no id field."
  (let ((sent nil)
        (opencode-acp-log-io nil))
    (cl-letf (((symbol-function 'opencode-acp--send)
               (lambda (_proc msg) (setq sent msg))))
      (opencode-acp-notify nil "session/cancel" '((sessionId . "s1"))))
    (should sent)
    (should-not (alist-get 'id sent))
    (should (equal (alist-get 'method sent) "session/cancel"))))

;;; Reset

(ert-deftest test-opencode-acp/reset ()
  "Reset clears state."
  (let ((opencode-acp--next-id 42)
        (opencode-acp--pending-requests (make-hash-table :test 'eql)))
    (puthash 1 '(nil nil nil) opencode-acp--pending-requests)
    (opencode-acp-reset)
    (should (= opencode-acp--next-id 0))
    (should (= (hash-table-count opencode-acp--pending-requests) 0))))

(provide 'emacs-opencode-acp-test)

;;; emacs-opencode-acp-test.el ends here
