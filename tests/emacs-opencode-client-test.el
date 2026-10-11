;;; emacs-opencode-client-test.el --- Tests for HTTP client  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-opencode-client)

(defmacro opencode-client-test--with-captured-request (url-var args-var &rest body)
  "Run BODY with `request' stubbed to capture its arguments.
URL-VAR and ARGS-VAR are bound to the URL and keyword arguments of the
last `request' call made during BODY."
  (declare (indent 2))
  `(let ((,url-var nil)
         (,args-var nil))
     (cl-letf (((symbol-function 'request)
                (lambda (&rest request-args)
                  (setq ,url-var (car request-args)
                        ,args-var (cdr request-args))
                  nil)))
       ,@body)))

(defun opencode-client-test--connection (&optional directory)
  "Return a connection for DIRECTORY pointed at a fake base URL."
  (opencode-connection-create
   :base-url "http://127.0.0.1:4096"
   :hostname "127.0.0.1"
   :port 4096
   :directory directory
   :timeout 10))

;;; location params and data envelope

(ert-deftest test-opencode-client/location-params-use-deepobject ()
  "Config endpoints scope the request with location[directory]."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (should (equal (opencode-client--location-params conn)
                   '(("location[directory]" . "/tmp/project"))))))

(ert-deftest test-opencode-client/location-params-nil-without-directory ()
  "No location params when the connection has no directory."
  (let ((conn (opencode-client-test--connection nil)))
    (should-not (opencode-client--location-params conn))))

(ert-deftest test-opencode-client/directory-params-use-plain-directory ()
  "Session lists scope with a plain directory query parameter."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (should (equal (opencode-client--directory-params conn)
                   '(("directory" . "/tmp/project"))))))

(ert-deftest test-opencode-client/directory-strips-trailing-slash ()
  "The directory helper removes the trailing slash."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (should (equal (opencode-client--directory conn) "/tmp/project"))))

(ert-deftest test-opencode-client/location-body-wraps-directory ()
  "The location body nests the directory."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (should (equal (opencode-client--location-body conn)
                   '((location (directory . "/tmp/project")))))))

(ert-deftest test-opencode-client/health-hits-api-info ()
  "Health uses the V2 /api/info endpoint."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-health conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/info"))
      (should (equal (plist-get args :type) "GET")))))

(ert-deftest test-opencode-client/agents-send-location-params ()
  "Agent listing scopes with location[directory]."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-agents conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/agent"))
      (should (equal (plist-get args :params)
                     '(("location[directory]" . "/tmp/project")))))))

(ert-deftest test-opencode-client/commands-send-location-params ()
  "Command listing scopes with location[directory]."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-commands conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/command"))
      (should (equal (plist-get args :params)
                     '(("location[directory]" . "/tmp/project")))))))

(ert-deftest test-opencode-client/providers-send-location-params ()
  "Provider listing scopes with location[directory]."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-providers conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/provider"))
      (should (equal (plist-get args :params)
                     '(("location[directory]" . "/tmp/project")))))))

;;; models and integrations

(ert-deftest test-opencode-client/models-send-location-params ()
  "Model listing scopes with location[directory]."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-models conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/model"))
      (should (equal (plist-get args :type) "GET"))
      (should (equal (plist-get args :params)
                     '(("location[directory]" . "/tmp/project")))))))

(ert-deftest test-opencode-client/integrations-send-location-params ()
  "Integration listing scopes with location[directory]."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-integrations conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/integration"))
      (should (equal (plist-get args :type) "GET"))
      (should (equal (plist-get args :params)
                     '(("location[directory]" . "/tmp/project")))))))

(ert-deftest test-opencode-client/integration-connect-key ()
  "Connecting with a key posts the key to the integration."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-integration-connect-key
       conn "openai" "sk-secret" :success #'ignore :error #'ignore)
      (should (equal url
                     "http://127.0.0.1:4096/api/integration/openai/connect/key"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data)
                     (json-encode '((key . "sk-secret"))))))))

(ert-deftest test-opencode-client/integration-connect-key-label ()
  "A label is included when provided."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-integration-connect-key
       conn "openai" "sk-secret" :label "Work"
       :success #'ignore :error #'ignore)
      (should (equal (plist-get args :data)
                     (json-encode '((key . "sk-secret")
                                    (label . "Work"))))))))

(ert-deftest test-opencode-client/integration-connect-oauth ()
  "Starting OAuth posts the method id."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-integration-connect-oauth
       conn "opencode" "chatgpt-browser" :success #'ignore :error #'ignore)
      (should (equal url
                     "http://127.0.0.1:4096/api/integration/opencode/connect/oauth"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data)
                     (json-encode '((methodID . "chatgpt-browser"))))))))

(ert-deftest test-opencode-client/integration-attempt-status ()
  "Attempt status is fetched by attempt id."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-integration-attempt-status
       conn "att_1" :success #'ignore :error #'ignore)
      (should (equal url
                     "http://127.0.0.1:4096/api/integration/attempt/att_1"))
      (should (equal (plist-get args :type) "GET"))
      (should (equal (plist-get args :params)
                     '(("location[directory]" . "/tmp/project")))))))

(ert-deftest test-opencode-client/integration-attempt-complete ()
  "Completing an attempt posts the code."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-integration-attempt-complete
       conn "att_1" :code "123456" :success #'ignore :error #'ignore)
      (should (equal url
                     "http://127.0.0.1:4096/api/integration/attempt/att_1/complete"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data)
                     (json-encode '((code . "123456"))))))))

(ert-deftest test-opencode-client/integration-attempt-complete-no-code ()
  "Completing without a code sends no body."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-integration-attempt-complete
       conn "att_1" :success #'ignore :error #'ignore)
      (should (null (plist-get args :data))))))

(ert-deftest test-opencode-client/json-read-data-unwraps-envelope ()
  "A top-level data envelope is unwrapped."
  (with-temp-buffer
    (insert "{\"data\":[1,2],\"cursor\":null}")
    (goto-char (point-min))
    (should (equal (opencode-client--json-read-data) '(1 2)))))

(ert-deftest test-opencode-client/json-read-data-passes-through ()
  "A payload without a data envelope is returned unchanged."
  (with-temp-buffer
    (insert "{\"version\":\"2.0.18\"}")
    (goto-char (point-min))
    (should (equal (opencode-client--json-read-data)
                   '((version . "2.0.18"))))))

(ert-deftest test-opencode-client/request-sends-authorization-header ()
  "Requests carry an explicit Basic auth header, not :auth."
  (let ((conn (opencode-connection-create
               :base-url "http://127.0.0.1:4096"
               :username "opencode" :password "secret")))
    (opencode-client-test--with-captured-request _url args
      (opencode-request conn 'GET "/api/info")
      (should (equal (cdr (assoc "Authorization" (plist-get args :headers)))
                     (format "Basic %s"
                             (base64-encode-string "opencode:secret" t))))
      (should (null (plist-get args :auth))))))

(ert-deftest test-opencode-client/request-respects-caller-authorization ()
  "An explicit Authorization header is not overridden."
  (let ((conn (opencode-connection-create
               :base-url "http://127.0.0.1:4096"
               :username "opencode" :password "secret")))
    (opencode-client-test--with-captured-request _url args
      (opencode-request conn 'GET "/api/info"
                        :headers '(("Authorization" . "Bearer token")))
      (should (equal (cdr (assoc "Authorization" (plist-get args :headers)))
                     "Bearer token")))))

;;; session-create

(ert-deftest test-opencode-client/session-create-sends-location ()
  "Session creation posts /api/session with the location body."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-create conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data)
                     (json-encode '((location (directory . "/tmp/project")))))))))

(ert-deftest test-opencode-client/session-create-empty-object-without-directory ()
  "Session creation sends an empty JSON object when unscoped."
  (let ((conn (opencode-client-test--connection nil)))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-session-create conn :success #'ignore :error #'ignore)
      (should (equal (plist-get args :data) "{}")))))

(ert-deftest test-opencode-client/session-create-includes-agent-and-model ()
  "Session creation can seed the agent and model."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-session-create conn
                                      :agent "build"
                                      :model '("anthropic" . "claude")
                                      :variant "thinking"
                                      :success #'ignore :error #'ignore)
      (should (equal (plist-get args :data)
                     (json-encode '((location (directory . "/tmp/project"))
                                    (agent . "build")
                                    (model (id . "claude")
                                           (providerID . "anthropic")
                                           (variant . "thinking")))))))))

;;; sessions

(ert-deftest test-opencode-client/sessions-scope-to-directory ()
  "Listing sessions scopes to the connection directory."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-sessions conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session"))
      (should (equal (plist-get args :type) "GET"))
      (should (equal (plist-get args :params)
                     '(("directory" . "/tmp/project"))))
      (should (null (plist-get args :data))))))

(ert-deftest test-opencode-client/sessions-forwards-limit ()
  "A limit is forwarded as a limit query parameter, not a request body."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-sessions conn :limit 1000
                                :success #'ignore :error #'ignore)
      (should (equal (plist-get args :params)
                     '(("directory" . "/tmp/project") ("limit" . 1000))))
      (should (null (plist-get args :data))))))

(ert-deftest test-opencode-client/sessions-omit-directory-without-connection-directory ()
  "Session lists omit the directory parameter when the connection has none."
  (let ((conn (opencode-client-test--connection nil)))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-sessions conn :limit 5
                                :success #'ignore :error #'ignore)
      (should (equal (plist-get args :params) '(("limit" . 5)))))))

;;; session-messages

(ert-deftest test-opencode-client/session-messages-no-limit-omits-params ()
  "Fetching messages without a limit sends no query params."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-messages conn "ses_1"
                                         :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/message"))
      (should (equal (plist-get args :type) "GET"))
      (should (null (plist-get args :params)))
      (should (null (plist-get args :data))))))

(ert-deftest test-opencode-client/session-messages-forwards-limit ()
  "A limit is forwarded as a limit query parameter, not a request body."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-session-messages conn "ses_1" :limit 5
                                         :success #'ignore :error #'ignore)
      (should (equal (plist-get args :params) '(("limit" . 5))))
      (should (null (plist-get args :data))))))

(ert-deftest test-opencode-client/session-messages-forwards-order-and-cursor ()
  "Order and cursor are forwarded as query parameters for paging."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-session-messages conn "ses_1" :order "asc" :cursor "cur1"
                                         :success #'ignore :error #'ignore)
      (should (equal (plist-get args :params)
                     '(("order" . "asc") ("cursor" . "cur1"))))
      (should (null (plist-get args :data))))))

;;; session-message

(ert-deftest test-opencode-client/session-message-fetches-single-message ()
  "Fetching one message hits the message endpoint with both IDs."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-message conn "ses_1" "msg_1"
                                        :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/message/msg_1"))
      (should (equal (plist-get args :type) "GET"))
      (should (null (plist-get args :data))))))

;;; session-fork

(ert-deftest test-opencode-client/session-fork-without-message-sends-no-body ()
  "Forking a whole session posts no request body."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-fork conn "ses_1" :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/fork"))
      (should (equal (plist-get args :type) "POST"))
      (should (null (plist-get args :data))))))

(ert-deftest test-opencode-client/session-fork-with-message-sends-before ()
  "Forking at a message sends BEFORE in the JSON body."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-session-fork conn "ses_1" :message-id "msg_1"
                                    :success #'ignore :error #'ignore)
      (should (equal (plist-get args :data)
                     (json-encode '((before . "msg_1"))))))))

;;; session-rename

(ert-deftest test-opencode-client/session-rename-sends-title ()
  "Renaming a session patches its title."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-rename conn "ses_1" "New title"
                                      :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1"))
      (should (equal (plist-get args :type) "PATCH"))
      (should (equal (plist-get args :data)
                     (json-encode '((title . "New title"))))))))

;;; session-compact

(ert-deftest test-opencode-client/session-compact-posts-no-body ()
  "Compacting a session posts to /compact with no body."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-compact conn "ses_1"
                                       :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/compact"))
      (should (equal (plist-get args :type) "POST"))
      (should (null (plist-get args :data))))))

;;; session-prompt

(ert-deftest test-opencode-client/session-prompt-posts-to-prompt ()
  "Prompts post to the V2 /prompt endpoint as text plus attachments."
  (let ((conn (opencode-client-test--connection "/tmp/project/"))
        (agents '(((name . "explore"))))
        (files '(((uri . "file:///tmp/a.txt") (name . "a.txt")))))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-prompt-async conn "ses_1" "hello"
                                            :agents agents
                                            :files files
                                            :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/prompt"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data)
                     (json-encode `((text . "hello")
                                    (files . ,files)
                                    (agents . ,agents))))))))

(ert-deftest test-opencode-client/session-prompt-omits-empty-attachments ()
  "Prompt bodies omit files/agents when there are none."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-session-prompt-async conn "ses_1" "hello"
                                            :success #'ignore :error #'ignore)
      (should (equal (plist-get args :data)
                     (json-encode '((text . "hello"))))))))

;;; session-set-agent / session-set-model

(ert-deftest test-opencode-client/session-set-agent-posts-agent ()
  "Switching agents posts {agent} to the agent endpoint."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-set-agent conn "ses_1" "build"
                                         :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/agent"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data)
                     (json-encode '((agent . "build"))))))))

(ert-deftest test-opencode-client/session-set-model-posts-model ()
  "Switching models posts {model:{id,providerID}} to the model endpoint."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-set-model conn "ses_1" '("anthropic" . "claude")
                                         :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/model"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data)
                     (json-encode '((model (id . "claude")
                                           (providerID . "anthropic")))))))))

(ert-deftest test-opencode-client/session-set-model-includes-variant ()
  "A variant is nested inside the model reference."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-session-set-model conn "ses_1" '("anthropic" . "claude")
                                         :variant "thinking"
                                         :success #'ignore :error #'ignore)
      (should (equal (plist-get args :data)
                     (json-encode '((model (id . "claude")
                                           (providerID . "anthropic")
                                           (variant . "thinking")))))))))

;;; session-abort

(ert-deftest test-opencode-client/session-abort-posts-to-interrupt ()
  "Abort posts to the V2 /interrupt endpoint."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-abort conn "ses_1"
                                     :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/interrupt"))
      (should (equal (plist-get args :type) "POST")))))

;;; session-command

(ert-deftest test-opencode-client/session-command-posts-to-command ()
  "Commands post to /api/session/{id}/command."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-command conn "ses_1" "review" "main"
                                       :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/command"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data)
                     (json-encode '((name . "review")
                                    (text . "main"))))))))

;;; session-shell

(ert-deftest test-opencode-client/session-shell-sends-command-only ()
  "Shell drops agent/model and sends only the command."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-shell conn "ses_1" "ls -la"
                                     :agent "build"
                                     :model '("anthropic" . "claude")
                                     :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/shell"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data)
                     (json-encode '((command . "ls -la"))))))))

;;; permission-reply

(ert-deftest test-opencode-client/permission-reply-posts-decision ()
  "Permission replies post the decision to the V2 reply endpoint."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-permission-reply conn "ses_1" "per_1" "once"
                                        :success #'ignore :error #'ignore)
      (should (equal url
                     "http://127.0.0.1:4096/api/session/ses_1/permission/per_1/reply"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data)
                     (json-encode '((decision . "once"))))))))

(ert-deftest test-opencode-client/permission-reply-includes-message ()
  "A permission reply includes the optional message when provided."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-permission-reply conn "ses_1" "per_1" "reject"
                                        :message "not this time"
                                        :success #'ignore :error #'ignore)
      (should (equal (plist-get args :data)
                     (json-encode '((decision . "reject")
                                    (message . "not this time"))))))))

;;; format-error

(ert-deftest test-opencode-client/format-error-status-and-tag ()
  "Format status code and server error tag."
  (let ((response (make-request-response :status-code 400)))
    (should (equal (opencode-client-format-error
                    (list :response response :data '((_tag . "BadRequest"))))
                   "HTTP 400: BadRequest"))))

(ert-deftest test-opencode-client/format-error-prefers-message-field ()
  "Prefer a server-provided message over the error tag."
  (let ((response (make-request-response :status-code 400)))
    (should (equal (opencode-client-format-error
                    (list :response response
                          :data '((_tag . "BadRequest")
                                  (message . "directory is invalid"))))
                   "HTTP 400: directory is invalid"))))

(ert-deftest test-opencode-client/format-error-status-only ()
  "Fall back to the status code alone."
  (let ((response (make-request-response :status-code 500)))
    (should (equal (opencode-client-format-error (list :response response))
                   "HTTP 500"))))

(ert-deftest test-opencode-client/format-error-error-thrown ()
  "Fall back to the thrown error when there is no response."
  (should (equal (opencode-client-format-error
                  (list :error-thrown '(error . "connection refused")))
                 "(error . connection refused)")))

(ert-deftest test-opencode-client/format-error-nil-without-detail ()
  "Return nil when no detail is available."
  (should (null (opencode-client-format-error nil))))

;;; vectorize-answers

(ert-deftest test-opencode-client/vectorize-answers-list-of-lists ()
  "Convert list of lists to vector of vectors."
  (let ((result (opencode--vectorize-answers '(("a" "b") ("c")))))
    (should (vectorp result))
    (should (= (length result) 2))
    (should (vectorp (aref result 0)))
    (should (equal (aref result 0) ["a" "b"]))
    (should (equal (aref result 1) ["c"]))))

(ert-deftest test-opencode-client/vectorize-answers-vector-input ()
  "Handle vector input."
  (let ((result (opencode--vectorize-answers [("a") ("b")])))
    (should (vectorp result))
    (should (= (length result) 2))))

(ert-deftest test-opencode-client/vectorize-answers-strings ()
  "Wrap plain strings in vectors."
  (let ((result (opencode--vectorize-answers '("hello" "world"))))
    (should (vectorp result))
    (should (equal (aref result 0) ["hello"]))
    (should (equal (aref result 1) ["world"]))))

(ert-deftest test-opencode-client/vectorize-answers-mixed ()
  "Handle mixed input types."
  (let ((result (opencode--vectorize-answers '(["a"] ("b") "c"))))
    (should (vectorp result))
    (should (equal (aref result 0) ["a"]))
    (should (equal (aref result 1) ["b"]))
    (should (equal (aref result 2) ["c"]))))

(ert-deftest test-opencode-client/vectorize-answers-nil ()
  "Handle nil input."
  (let ((result (opencode--vectorize-answers nil)))
    (should (vectorp result))
    (should (= (length result) 0))))

;;; form reply, cancel, and list

(ert-deftest test-opencode-client/form-reply-posts-answer ()
  "Form replies POST the answer object to the V2 reply endpoint."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-form-reply
       conn "ses_1" "frm_1" '((name . "Jo"))
       :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/form/frm_1/reply"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data)
                     "{\"answer\":{\"name\":\"Jo\"}}")))))

(ert-deftest test-opencode-client/form-reply-normalizes-json-values ()
  "Form answers normalize lists to vectors and nil to false."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-form-reply
       conn "ses_1" "frm_1" '((tags . ("a" "b"))
                             (dry-run . nil)
                             (ok . t)
                             (count . 3))
       :success #'ignore :error #'ignore)
      (should (equal (plist-get args :data)
                     (concat "{\"answer\":{\"tags\":[\"a\",\"b\"],"
                             "\"dry-run\":false,\"ok\":true,"
                             "\"count\":3}}"))))))

(ert-deftest test-opencode-client/form-reply-empty-answer ()
  "An empty answer map posts as an empty object."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-form-reply
       conn "ses_1" "frm_1" nil
       :success #'ignore :error #'ignore)
      (should (equal (plist-get args :data) "{\"answer\":{}}")))))

(ert-deftest test-opencode-client/form-cancel-deletes ()
  "Cancelling a form DELETEs the form endpoint."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-form-cancel
       conn "ses_1" "frm_1"
       :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/form/frm_1"))
      (should (equal (plist-get args :type) "DELETE")))))

(ert-deftest test-opencode-client/form-list-fetches ()
  "Pending forms are fetched from the session form endpoint."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-form-list
       conn "ses_1"
       :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/form"))
      (should (equal (plist-get args :type) "GET")))))

(ert-deftest test-opencode-client/json-normalize-value-basics ()
  "JSON normalization maps nil to false and lists to vectors."
  (should (eq (opencode--normalize-json-value nil) :json-false))
  (should (eq (opencode--normalize-json-value t) t))
  (should (eq (opencode--normalize-json-value :json-false) :json-false))
  (should (equal (opencode--normalize-json-value '("a" "b"))
                 ["a" "b"]))
  (should (equal (opencode--normalize-json-value "x") "x"))
  (should (equal (opencode--normalize-json-value 3) 3)))

(ert-deftest test-opencode-client/json-normalize-answer-keeps-vectors ()
  "Pre-vectorized multiselect answers pass through unchanged."
  (let ((answer '((regions . ["a" "b"]) (count . 2))))
    (should (equal (opencode--normalize-json-answer answer)
                   '((regions . ["a" "b"]) (count . 2))))))

(provide 'emacs-opencode-client-test)

;;; emacs-opencode-client-test.el ends here
