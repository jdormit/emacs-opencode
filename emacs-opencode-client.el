;;; emacs-opencode-client.el --- OpenCode HTTP client  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'request)
(require 'subr-x)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-sse)

(declare-function opencode--json-read "emacs-opencode-sse")

(defun opencode-client--directory (conn)
  "Return CONN's directory without a trailing slash, or nil."
  (when-let* ((directory (opencode-connection-directory conn)))
    (directory-file-name directory)))

(defun opencode-client--location-params (conn)
  "Return deepObject location query params for CONN.

OpenCode V2 scopes config-style endpoints with a `location[directory]'
query parameter instead of the V1 x-opencode-directory request header."
  (when-let* ((directory (opencode-client--directory conn)))
    `(("location[directory]" . ,directory))))

(defun opencode-client--directory-params (conn)
  "Return a plain `directory' query param for CONN when it has one."
  (when-let* ((directory (opencode-client--directory conn)))
    `(("directory" . ,directory))))

(defun opencode-client--location-body (conn)
  "Return a `location' request body for CONN when it has a directory."
  (when-let* ((directory (opencode-client--directory conn)))
    `((location . ((directory . ,directory))))))

(defun opencode-client--auth-header (conn)
  "Return an Authorization header alist for CONN when it has a password.

The `request' package's :auth keyword resolves credentials through
auth-source instead of using the supplied values, so the header is set
explicitly, matching `opencode-sse--auth-header'."
  (when-let* ((password (opencode-connection-password conn)))
    (let ((user (or (opencode-connection-username conn) "opencode")))
      `(("Authorization"
         . ,(format "Basic %s"
                    (base64-encode-string
                     (format "%s:%s" user password) t)))))))

(defun opencode-client--json-read-data ()
  "Read JSON and unwrap a top-level `data' envelope when present.

OpenCode V2 wraps most payloads as {\"data\": ...}.  Unwrapping here keeps
the API wrappers returning bare resources and lists, matching what the
callers expect."
  (let ((data (opencode--json-read)))
    (if (and (listp data) (assoc 'data data))
        (alist-get 'data data)
      data)))

(defun opencode-client-format-error (args)
  "Return a short description of a failed request from ARGS.

ARGS is the keyword plist passed to `request' error callbacks.  Returns a
string like \"HTTP 400: BadRequest\", or nil when no detail is available."
  (let* ((response (plist-get args :response))
         (status (and response (request-response-status-code response)))
         (data (plist-get args :data))
         (detail (and (listp data)
                      (or (alist-get 'message data)
                          (alist-get '_tag data))))
         (error-thrown (plist-get args :error-thrown)))
    (cond
     ((and status detail) (format "HTTP %s: %s" status detail))
     (status (format "HTTP %s" status))
     (detail (format "%s" detail))
     (error-thrown (format "%s" error-thrown)))))

(cl-defmethod opencode-request ((conn opencode-connection) method path &rest args &key data json parser headers timeout &allow-other-keys)
  "Send a raw HTTP request using CONN.

METHOD is a HTTP verb symbol like `GET` or `POST`. PATH is appended to the
connection base URL. DATA is passed through to `request`. When JSON is
provided, it is encoded and sent with a JSON content type. PARSER defaults to
`opencode-client--json-read-data', which unwraps the V2 `data' envelope.
HEADERS is an alist of HTTP headers. Any remaining ARGS are forwarded to
`request'."
  (let* ((base-url (opencode-connection-base-url conn))
         (url (concat (string-remove-suffix "/" base-url) path))
         (payload (when json (json-encode json)))
         (merged-headers (append headers
                                 (unless (assoc "Authorization" headers)
                                   (opencode-client--auth-header conn))
                                 (when json
                                   '(("Content-Type" . "application/json")))))
         (timeout-value (if (plist-member args :timeout)
                            timeout
                          (or (opencode-connection-timeout conn) 10))))
    (apply
     #'request
     url
     :type (symbol-name method)
     :data (or payload data)
     :parser (or parser #'opencode-client--json-read-data)
     :headers merged-headers
     :timeout timeout-value
     args)))

(cl-defmethod opencode-client-health ((conn opencode-connection) &key success error)
  "Fetch OpenCode server health."
  (opencode-request
   conn
   'GET
   "/api/info"
   :success success
   :error error))

(cl-defmethod opencode-client-session-create
  ((conn opencode-connection) &key title agent model variant success error)
  "Create a new session in CONN's directory.

The target directory is sent as the `location' body field so the server
creates the session in the connection's workspace.  TITLE, AGENT and MODEL
are optional.  MODEL is a cons (PROVIDER-ID . MODEL-ID) and VARIANT is
included in the model reference when provided."
  (let ((body (append (opencode-client--location-body conn)
                      (when title `((title . ,title)))
                      (when agent `((agent . ,agent)))
                      (when model
                        `((model . ((id . ,(cdr model))
                                    (providerID . ,(car model))
                                    ,@(when variant
                                        `((variant . ,variant))))))))))
    (opencode-request
     conn
     'POST
     "/api/session"
     :json (or body (make-hash-table))
     :success success
     :error error)))

(cl-defmethod opencode-client-sessions ((conn opencode-connection) &key success error limit)
  "Fetch OpenCode sessions list.

LIMIT restricts the number of returned sessions when provided.  The
server caps the list at 100 sessions by default, so a higher LIMIT is
required to retrieve more.  Results are scoped to CONN's directory."
  (opencode-request
   conn
   'GET
   "/api/session"
   :params (append (opencode-client--directory-params conn)
                   (when limit `(("limit" . ,limit))))
   :success success
   :error error))

(cl-defmethod opencode-client-session-messages
  ((conn opencode-connection) session-id &key success error limit order cursor)
  "Fetch messages for SESSION-ID, paged with ORDER and CURSOR.

LIMIT restricts the number of returned messages per page.  ORDER
selects the first page direction (\"asc\" for oldest first, \"desc\"
for newest first).  CURSOR is an opaque paging cursor taken from a
previous response's `cursor.next' or `cursor.previous'; per the V2
API it must not be combined with ORDER.  The success callback receives
the raw body `((data . [...]) (cursor . ((next . N) (previous . P))))'
so callers can follow the pages."
  (opencode-request
   conn
   'GET
   (format "/api/session/%s/message" session-id)
   :parser (lambda () (opencode--json-read))
   :params (append (when limit `(("limit" . ,limit)))
                   (when order `(("order" . ,order)))
                   (when cursor `(("cursor" . ,cursor))))
   :success success
   :error error))

(cl-defmethod opencode-client-session-message ((conn opencode-connection) session-id message-id &key success error)
  "Fetch the single message MESSAGE-ID in SESSION-ID.

Unlike the SSE stream, the response includes full tool output in each
part's state."
  (opencode-request
   conn
   'GET
   (format "/api/session/%s/message/%s" session-id message-id)
   :success success
   :error error))

(cl-defmethod opencode-client-session-fork ((conn opencode-connection) session-id &key message-id success error)
  "Fork SESSION-ID.

When MESSAGE-ID is provided, fork the session before that message.  When
MESSAGE-ID is nil, fork the whole session."
  (opencode-request
   conn
   'POST
   (format "/api/session/%s/fork" session-id)
   :json (when message-id `((before . ,message-id)))
   :success success
   :error error))

(cl-defmethod opencode-client-session-rename
  ((conn opencode-connection) session-id title &key success error)
  "Use CONN to rename SESSION-ID to TITLE."
  (opencode-request
   conn
   'PATCH
   (format "/api/session/%s" session-id)
   :json `((title . ,title))
   :success success
   :error error))

(cl-defmethod opencode-client-session-compact
  ((conn opencode-connection) session-id &key success error)
  "Compact SESSION-ID.

In OpenCode V2 the model used for compaction comes from the session, so
no model argument is required."
  (opencode-request
   conn
   'POST
   (format "/api/session/%s/compact" session-id)
   :parser (lambda () nil)
   :timeout nil
   :success success
   :error error))

(cl-defmethod opencode-client-session-set-agent
  ((conn opencode-connection) session-id agent &key success error)
  "Set the active AGENT for SESSION-ID."
  (opencode-request
   conn
   'POST
   (format "/api/session/%s/agent" session-id)
   :json `((agent . ,agent))
   :parser (lambda () nil)
   :success success
   :error error))

(cl-defmethod opencode-client-session-set-model
  ((conn opencode-connection) session-id model &key variant success error)
  "Switch SESSION-ID to MODEL.

MODEL is a cons (PROVIDER-ID . MODEL-ID).  VARIANT is included in the
model reference when provided."
  (opencode-request
   conn
   'POST
   (format "/api/session/%s/model" session-id)
   :json `((model . ((id . ,(cdr model))
                     (providerID . ,(car model))
                     ,@(when variant `((variant . ,variant))))))
   :parser (lambda () nil)
   :success success
   :error error))

(cl-defmethod opencode-client-agents ((conn opencode-connection) &key success error)
  "Fetch available agents from the server."
  (opencode-request
   conn
   'GET
   "/api/agent"
   :params (opencode-client--location-params conn)
   :success success
   :error error))

(cl-defmethod opencode-client-providers ((conn opencode-connection) &key success error)
  "Fetch available providers from the server."
  (opencode-request
   conn
   'GET
   "/api/provider"
   :params (opencode-client--location-params conn)
   :success success
   :error error))

(cl-defmethod opencode-client-commands ((conn opencode-connection) &key success error)
  "Fetch available commands from the server."
  (opencode-request
   conn
   'GET
   "/api/command"
   :params (opencode-client--location-params conn)
   :success success
   :error error))

(cl-defmethod opencode-client-models ((conn opencode-connection) &key success error)
  "Fetch the available models from the server."
  (opencode-request
   conn
   'GET
   "/api/model"
   :params (opencode-client--location-params conn)
   :success success
   :error error))

(cl-defmethod opencode-client-integrations ((conn opencode-connection) &key success error)
  "Fetch available integrations from the server.

Integrations describe how a provider is authenticated, including its
methods and any established connections."
  (opencode-request
   conn
   'GET
   "/api/integration"
   :params (opencode-client--location-params conn)
   :success success
   :error error))

(cl-defmethod opencode-client-integration-connect-key
  ((conn opencode-connection) integration-id key &key label success error)
  "Connect INTEGRATION-ID on CONN using an API KEY.

LABEL optionally names the stored credential."
  (opencode-request
   conn
   'POST
   (format "/api/integration/%s/connect/key" integration-id)
   :json `((key . ,key)
           ,@(when label `((label . ,label))))
   :parser (lambda () nil)
   :success success
   :error error))

(cl-defmethod opencode-client-integration-connect-oauth
  ((conn opencode-connection) integration-id method-id &key inputs label success error)
  "Start an OAuth flow for INTEGRATION-ID on CONN using METHOD-ID.

INPUTS is an optional alist of method input values.  LABEL optionally
names the stored credential.  The success callback receives an attempt
alist with `attemptID', `url', `instructions' and `mode'."
  (opencode-request
   conn
   'POST
   (format "/api/integration/%s/connect/oauth" integration-id)
   :json `((methodID . ,method-id)
           ,@(when inputs `((inputs . ,inputs)))
           ,@(when label `((label . ,label))))
   :success success
   :error error))

(cl-defmethod opencode-client-integration-attempt-status
  ((conn opencode-connection) attempt-id &key success error)
  "Fetch the status of OAuth ATTEMPT-ID on CONN.

The success callback receives a status alist with a `status' key whose
value is one of \"pending\", \"complete\", \"failed\" or \"expired\"."
  (opencode-request
   conn
   'GET
   (format "/api/integration/attempt/%s" attempt-id)
   :params (opencode-client--location-params conn)
   :success success
   :error error))

(cl-defmethod opencode-client-integration-attempt-complete
  ((conn opencode-connection) attempt-id &key code success error)
  "Complete OAuth ATTEMPT-ID on CONN, optionally supplying CODE."
  (opencode-request
   conn
   'POST
   (format "/api/integration/attempt/%s/complete" attempt-id)
   :json (when code `((code . ,code)))
   :parser (lambda () nil)
   :success success
   :error error))

(cl-defmethod opencode-client-session-prompt-async
  ((conn opencode-connection) session-id text &key files agents success error)
  "Send TEXT to SESSION-ID asynchronously.

FILES is a list of file attachment alists and AGENTS a list of agent
attachment alists to include with the prompt.  The active agent and model
are session state in V2 and must be set with `opencode-client-session-set-agent'
and `opencode-client-session-set-model'."
  (opencode-request
   conn
   'POST
   (format "/api/session/%s/prompt" session-id)
   :json (append `((text . ,text))
                 (when files `((files . ,files)))
                 (when agents `((agents . ,agents))))
   :parser (lambda () nil)
   :success success
   :error error))

(cl-defmethod opencode-client-session-abort ((conn opencode-connection) session-id &key success error)
  "Abort the active prompt for SESSION-ID."
  (opencode-request
   conn
   'POST
   (format "/api/session/%s/interrupt" session-id)
   :parser (lambda () nil)
   :success success
   :error error))

(cl-defmethod opencode-client-permission-reply ((conn opencode-connection) session-id request-id reply &key message success error)
  "Reply to permission REQUEST-ID in SESSION-ID with REPLY.

REPLY is one of \"once\", \"always\", or \"reject\".  MESSAGE is
sent alongside the decision when provided."
  (let ((payload `((decision . ,reply))))
    (when message
      (setq payload (append payload `((message . ,message)))))
    (opencode-request
     conn
     'POST
     (format "/api/session/%s/permission/%s/reply" session-id request-id)
     :json payload
     :success success
     :error error)))

(defun opencode--normalize-json-value (value)
  "Return VALUE normalized for `json-encode'.

Lists become vectors, nil becomes `:json-false', and all other values
pass through unchanged.  Used for form answers, where multiselect
answers are lists of strings and booleans must keep the true/false
distinction after JSON round-tripping."
  (cond
   ((null value) :json-false)
   ((listp value) (apply #'vector
                         (mapcar #'opencode--normalize-json-value value)))
   (t value)))

(defun opencode--normalize-json-answer (answer)
  "Return ANSWER normalized for `json-encode'.

ANSWER is an alist mapping form field keys to values; each value is
run through `opencode--normalize-json-value'."
  (mapcar (lambda (pair)
            (cons (car pair)
                  (opencode--normalize-json-value (cdr pair))))
          answer))

(cl-defmethod opencode-client-form-reply ((conn opencode-connection) session-id form-id answer &key success error)
  "Reply to form FORM-ID in SESSION-ID with ANSWER.

ANSWER is an alist mapping field keys to values.  Multiselect values
are lists of strings, booleans are t or nil, and external fields are
acknowledged with a non-nil value."
  (let ((normalized (opencode--normalize-json-answer answer)))
    (opencode-request
     conn
     'POST
     (format "/api/session/%s/form/%s/reply" session-id form-id)
     ;; An empty answer must still be a JSON object (`{}') rather than
     ;; null, so fall back to an empty hash table when nothing to send.
     :json `((answer . ,(if normalized
                            normalized
                          (make-hash-table :test #'equal))))
     :parser (lambda () nil)
     :success success
     :error error)))

(cl-defmethod opencode-client-form-cancel ((conn opencode-connection) session-id form-id &key success error)
  "Cancel the pending form FORM-ID in SESSION-ID."
  (opencode-request
   conn
   'DELETE
   (format "/api/session/%s/form/%s" session-id form-id)
   :parser (lambda () nil)
   :success success
   :error error))

(cl-defmethod opencode-client-form-list ((conn opencode-connection) session-id &key success error)
  "Fetch the pending forms for SESSION-ID.

The SUCCESS callback receives a list of form alists; ERROR receives
the request failure arguments."
  (opencode-request
   conn
   'GET
   (format "/api/session/%s/form" session-id)
   :success success
   :error error))

(cl-defmethod opencode-client-session-command
  ((conn opencode-connection) session-id name text &key success error)
  "Send command NAME with argument TEXT to SESSION-ID.

The selected agent and model are session state in V2, so they are not part
of the command request."
  (opencode-request
   conn
   'POST
   (format "/api/session/%s/command" session-id)
   :json `((name . ,name)
           (text . ,(or text "")))
   :parser (lambda () nil)
   :timeout nil
   :success success
   :error error))

(cl-defmethod opencode-client-session-shell
  ((conn opencode-connection) session-id command
   &key success error agent model)
  "Execute shell COMMAND in SESSION-ID.

In OpenCode V2 the shell endpoint accepts only the command; AGENT and
MODEL are ignored because the session owns those selections."
  (ignore agent model)
  (opencode-request
   conn
   'POST
   (format "/api/session/%s/shell" session-id)
   :json `((command . ,command))
   :parser (lambda () nil)
   :success success
   :error error))

(provide 'emacs-opencode-client)

;;; emacs-opencode-client.el ends here
