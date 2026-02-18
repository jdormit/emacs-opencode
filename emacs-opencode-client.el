;;; emacs-opencode-client.el --- OpenCode HTTP client  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'request)
(require 'subr-x)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-sse)

(declare-function opencode--json-read "emacs-opencode-sse")

(cl-defmethod opencode-request ((conn opencode-connection) method path &rest args &key data json parser headers timeout &allow-other-keys)
  "Send a raw HTTP request using CONN.

METHOD is a HTTP verb symbol like `GET` or `POST`. PATH is appended to the
connection base URL. DATA is passed through to `request`. When JSON is
provided, it is encoded and sent with a JSON content type. PARSER defaults to
`json-read` when omitted. HEADERS is an alist of HTTP headers. Any remaining
ARGS are forwarded to `request`."
  (let* ((base-url (opencode-connection-base-url conn))
         (url (concat (string-remove-suffix "/" base-url) path))
         (auth (when (opencode-connection-password conn)
                 (list (or (opencode-connection-username conn) "opencode")
                       (opencode-connection-password conn))))
         (payload (when json (json-encode json)))
         (merged-headers (if json
                             (append headers '(("Content-Type" . "application/json")))
                            headers))
         (timeout-value (if (plist-member args :timeout)
                            timeout
                          (or (opencode-connection-timeout conn) 10))))
    (apply
     #'request
     url
     :type (symbol-name method)
     :data (or payload data)
     :parser (or parser #'opencode--json-read)
     :headers merged-headers
     :auth auth
     :timeout timeout-value
     args)))

(cl-defmethod opencode-client-health ((conn opencode-connection) &key success error)
  "Fetch OpenCode server health."
  (opencode-request
   conn
   'GET
   "/global/health"
   :success success
   :error error))

(cl-defmethod opencode-client-sessions ((conn opencode-connection) &key success error)
  "Fetch OpenCode sessions list."
  (opencode-request
   conn
   'GET
   "/session"
   :success success
   :error error))

(cl-defmethod opencode-client-session-messages ((conn opencode-connection) session-id &key success error limit)
  "Fetch messages for SESSION-ID.

LIMIT restricts the number of returned messages when provided."
  (opencode-request
   conn
   'GET
   (format "/session/%s/message" session-id)
   :data (when limit `(("limit" . ,limit)))
   :success success
   :error error))

(cl-defmethod opencode-client-agents ((conn opencode-connection) &key success error)
  "Fetch available agents from the server."
  (opencode-request
   conn
   'GET
   "/agent"
   :success success
   :error error))

(cl-defmethod opencode-client-providers ((conn opencode-connection) &key success error)
  "Fetch available providers from the server."
  (opencode-request
   conn
   'GET
   "/provider"
   :success success
   :error error))

(cl-defmethod opencode-client-commands ((conn opencode-connection) &key success error)
  "Fetch available commands from the server."
  (opencode-request
   conn
   'GET
   "/command"
   :success success
   :error error))

(cl-defmethod opencode-client-instance-dispose ((conn opencode-connection) &key success error)
  "Dispose the current OpenCode instance for CONN."
  (opencode-request
   conn
   'POST
   "/instance/dispose"
   :parser (lambda () nil)
   :success success
   :error error))

(cl-defmethod opencode-client-provider-auth-methods ((conn opencode-connection) &key success error)
  "Fetch available auth methods for all providers."
  (opencode-request
   conn
   'GET
   "/provider/auth"
   :success success
   :error error))

(cl-defmethod opencode-client-provider-oauth-authorize
  ((conn opencode-connection) provider-id method-index &key success error)
  "Start OAuth authorization for PROVIDER-ID using METHOD-INDEX."
  (opencode-request
   conn
   'POST
   (format "/provider/%s/oauth/authorize" provider-id)
   :json `((method . ,method-index))
   :success success
   :error error))

(cl-defmethod opencode-client-provider-oauth-callback
  ((conn opencode-connection) provider-id method-index &key code success error)
  "Complete OAuth callback for PROVIDER-ID using METHOD-INDEX.

CODE is the authorization code for the \"code\" flow."
  (let ((payload `((method . ,method-index))))
    (when code
      (setq payload (append payload `((code . ,code)))))
    (opencode-request
     conn
     'POST
     (format "/provider/%s/oauth/callback" provider-id)
     :json payload
     :timeout nil
     :success success
     :error error)))

(cl-defmethod opencode-client-auth-set
  ((conn opencode-connection) provider-id auth-info &key success error)
  "Set auth credentials for PROVIDER-ID.

AUTH-INFO is an alist representing the auth payload."
  (opencode-request
   conn
   'PUT
   (format "/auth/%s" provider-id)
   :json auth-info
   :success success
   :error error))

(cl-defmethod opencode-client-session-prompt-async ((conn opencode-connection) session-id parts &key success error agent model)
  "Send PARTS to SESSION-ID asynchronously.

PARTS is a list of message part objects for the request body.  AGENT is
included when provided.  MODEL is a cons (PROVIDER-ID . MODEL-ID) included
when provided."
  (opencode-request
   conn
   'POST
   (format "/session/%s/prompt_async" session-id)
   :json (append (when agent `((agent . ,agent)))
                 (when model `((model . ((providerID . ,(car model))
                                         (modelID . ,(cdr model))))))
                 `((parts . ,parts)))
   :parser (lambda () nil)
   :success success
   :error error))

(cl-defmethod opencode-client-session-abort ((conn opencode-connection) session-id &key success error)
  "Abort the active prompt for SESSION-ID."
  (opencode-request
   conn
   'POST
   (format "/session/%s/abort" session-id)
   :parser (lambda () nil)
   :success success
   :error error))

(cl-defmethod opencode-client-permission-reply ((conn opencode-connection) request-id reply &key message success error)
  "Reply to permission REQUEST-ID with REPLY.

MESSAGE is sent when provided."
  (let ((payload `((reply . ,reply))))
    (when message
      (setq payload (append payload `((message . ,message)))))
    (opencode-request
     conn
     'POST
     (format "/permission/%s/reply" request-id)
     :json payload
     :success success
     :error error)))

(defun opencode--vectorize-answers (answers)
  "Return ANSWERS as a vector of answer vectors."
  (let ((items (cond
                ((vectorp answers) (append answers nil))
                ((listp answers) answers)
                (t nil))))
    (apply #'vector
           (mapcar (lambda (answer)
                     (cond
                      ((vectorp answer) answer)
                      ((listp answer) (vconcat answer))
                      ((stringp answer) (vector answer))
                      (t (vector))))
                   items))))

(cl-defmethod opencode-client-question-reply ((conn opencode-connection) request-id answers &key success error)
  "Reply to question REQUEST-ID with ANSWERS.

ANSWERS is a list of string lists aligned to the requested questions."
  (opencode-request
   conn
   'POST
   (format "/question/%s/reply" request-id)
   :json `((answers . ,(opencode--vectorize-answers answers)))
   :success success
   :error error))

(cl-defmethod opencode-client-question-reject ((conn opencode-connection) request-id &key success error)
  "Reject the question REQUEST-ID."
  (opencode-request
   conn
   'POST
   (format "/question/%s/reject" request-id)
   :parser (lambda () nil)
   :success success
   :error error))

(cl-defmethod opencode-client-session-command
  ((conn opencode-connection) session-id command arguments &key success error agent model)
  "Send COMMAND with ARGUMENTS to SESSION-ID.

MODEL is a \"provider/model\" string included when provided."
  (let ((payload `((command . ,command)
                   (arguments . ,(or arguments "")))))
    (when agent
      (setq payload (append payload `((agent . ,agent)))))
    (when model
      (setq payload (append payload `((model . ,model)))))
    (opencode-request
     conn
     'POST
     (format "/session/%s/command" session-id)
     :json payload
     :parser (lambda () nil)
     :timeout nil
     :success success
     :error error)))

(provide 'emacs-opencode-client)

;;; emacs-opencode-client.el ends here
