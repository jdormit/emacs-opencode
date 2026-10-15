;;; emacs-opencode-client.el --- OpenCode HTTP client  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'request)
(require 'subr-x)
(require 'url-util)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-sse)

(declare-function opencode--json-read "emacs-opencode-sse")

(defconst opencode-client--message-page-size 200
  "Number of messages requested per page when loading session history.
This is the largest page the server accepts.")

(defun opencode-client--location-directory (conn)
  "Return CONN's directory in the form the server stores locations.
Symlinks are resolved and the trailing slash is dropped so sessions created
here match the server's view of the project."
  (directory-file-name
   (file-truename (or (opencode-connection-directory conn) default-directory))))

(defun opencode-client--directory-header (conn headers)
  "Return a directory routing header alist for CONN.

Returns nil when HEADERS already contains an x-opencode-directory entry.
The value is url-encoded to match the OpenCode SDK."
  (when-let* ((directory (opencode-client--location-directory conn)))
    (unless (assoc "x-opencode-directory" headers)
      `(("x-opencode-directory" . ,(url-hexify-string directory))))))

(defun opencode-client--auth-header (conn)
  "Return a Basic Authorization header alist for CONN, or nil without a password."
  (when-let* ((password (opencode-connection-password conn)))
    `(("Authorization"
       . ,(concat "Basic "
                  (base64-encode-string
                   (format "%s:%s" (or (opencode-connection-username conn) "opencode")
                           password)
                   t))))))

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

(defun opencode-client--unwrap (success)
  "Wrap SUCCESS so it receives the response's `data' field as :data.
Most endpoints return {\"data\": ...}; callers only need the payload."
  (when success
    (lambda (&rest args)
      (let ((data (plist-get args :data)))
        (apply success
               (plist-put (copy-sequence args) :data
                          (and (listp data) (alist-get 'data data))))))))

(defun opencode-client--no-content ()
  "Parse an empty response body."
  nil)

(cl-defmethod opencode-request ((conn opencode-connection) method path &rest args &key data json parser headers timeout &allow-other-keys)
  "Send a raw HTTP request using CONN.

METHOD is a HTTP verb symbol like `GET` or `POST`. PATH is appended to the
connection base URL. DATA is passed through to `request`. When JSON is
provided, it is encoded and sent with a JSON content type. PARSER defaults to
`json-read` when omitted. HEADERS is an alist of HTTP headers. Any remaining
ARGS are forwarded to `request`.  Every request carries an
x-opencode-directory header derived from the calling directory so the
server routes requests to the right workspace."
  (let* ((directory (or (opencode-connection-directory conn) default-directory))
         (transport (or (opencode-connection-service conn) conn))
         (base-url (opencode-connection-base-url transport))
         (url (concat (string-remove-suffix "/" base-url) path))
         (payload (when json (json-encode json)))
         (merged-headers (append (opencode-client--directory-header conn headers)
                                 (opencode-client--auth-header transport)
                                 headers
                                 (when json
                                   '(("Content-Type" . "application/json")))))
         (timeout-value (if (plist-member args :timeout)
                            timeout
                          (or (opencode-connection-timeout conn) 10))))
    (dolist (key '(:success :error :complete))
      (when-let* ((callback (plist-get args key)))
        (setq args
              (plist-put args key
                         (lambda (&rest callback-args)
                           (let ((default-directory directory))
                             (apply callback callback-args)))))))
    (apply
     #'request
     url
     :type (symbol-name method)
     :data (or payload data)
     :parser (or parser #'opencode--json-read)
     :headers merged-headers
     :timeout timeout-value
     args)))

(defun opencode-client--path (format-string &rest ids)
  "Return FORMAT-STRING with url-encoded IDS substituted."
  (apply #'format format-string (mapcar #'url-hexify-string ids)))

(cl-defmethod opencode-client-health ((conn opencode-connection) &key success error)
  "Fetch OpenCode server info, which doubles as a health check."
  (opencode-request conn 'GET "/api/info" :success success :error error))

(cl-defmethod opencode-client-location ((conn opencode-connection) &key success error)
  "Fetch the location and project that CONN's directory resolves to."
  (opencode-request conn 'GET "/api/location" :success success :error error))

(cl-defmethod opencode-client-session-create ((conn opencode-connection) &key agent model permissions success error)
  "Create a new session in CONN's directory.

AGENT names the initial agent.  MODEL is a model reference alist with
`providerID', `id', and optional `variant'.  PERMISSIONS is a vector of
permission rules evaluated after the agent's own."
  (opencode-request
   conn
   'POST
   "/api/session"
   :json (append `((location . ((directory . ,(opencode-client--location-directory conn)))))
                 (when agent `((agent . ,agent)))
                 (when model `((model . ,model)))
                 (when permissions `((permissions . ,permissions))))
   :success (opencode-client--unwrap success)
   :error error))

(cl-defmethod opencode-client-session-get ((conn opencode-connection) session-id &key success error)
  "Fetch session info for SESSION-ID."
  (opencode-request
   conn
   'GET
   (opencode-client--path "/api/session/%s" session-id)
   :success (opencode-client--unwrap success)
   :error error))

(cl-defmethod opencode-client-sessions ((conn opencode-connection) &key success error limit roots project directory)
  "Fetch OpenCode sessions, newest first.

LIMIT restricts the number of returned sessions.  When ROOTS is non-nil,
return only sessions without a parent.  PROJECT restricts results to a
project ID and DIRECTORY to an exact session directory."
  (opencode-request
   conn
   'GET
   "/api/session"
   :params (append (when roots '(("parentID" . "null")))
                   (when limit `(("limit" . ,limit)))
                   (when project `(("project" . ,project)))
                   (when directory `(("directory" . ,directory))))
   :success (opencode-client--unwrap success)
   :error error))

(cl-defmethod opencode-client-session-active ((conn opencode-connection) &key success error)
  "Fetch the sessions currently running on CONN's server.
The data is an alist keyed by session ID."
  (opencode-request
   conn
   'GET
   "/api/session/active"
   :success (opencode-client--unwrap success)
   :error error))

(cl-defmethod opencode-client-session-messages ((conn opencode-connection) session-id &key success error)
  "Fetch every message for SESSION-ID in chronological order.

Pages through the history and calls SUCCESS once with the full list as
:data."
  (let ((messages nil))
    (cl-labels
        ((fetch (cursor)
           (opencode-request
            conn
            'GET
            (opencode-client--path "/api/session/%s/message" session-id)
            :params (append `(("limit" . ,opencode-client--message-page-size))
                            (if cursor
                                `(("cursor" . ,cursor))
                              '(("order" . "asc"))))
            :success (lambda (&rest args)
                       (let* ((body (plist-get args :data))
                              (page (alist-get 'data body))
                              (next (alist-get 'next (alist-get 'cursor body))))
                         (setq messages (nconc messages page))
                         (if (and next (>= (length page) opencode-client--message-page-size))
                             (fetch next)
                           (when success
                             (apply success (plist-put (copy-sequence args)
                                                       :data messages))))))
            :error error)))
      (fetch nil))))

(cl-defmethod opencode-client-session-message ((conn opencode-connection) session-id message-id &key success error)
  "Fetch the single message MESSAGE-ID in SESSION-ID.

Unlike the SSE stream, the response includes full tool output."
  (opencode-request
   conn
   'GET
   (opencode-client--path "/api/session/%s/message/%s" session-id message-id)
   :success (opencode-client--unwrap success)
   :error error))

(cl-defmethod opencode-client-session-fork ((conn opencode-connection) session-id &key message-id success error)
  "Fork SESSION-ID.

When MESSAGE-ID is provided, fork the session before that message.  When
MESSAGE-ID is nil, fork the whole session."
  (opencode-request
   conn
   'POST
   (opencode-client--path "/api/session/%s/fork" session-id)
   :json (if message-id `((before . ,message-id)) (make-hash-table))
   :success (opencode-client--unwrap success)
   :error error))

(cl-defmethod opencode-client-session-rename
  ((conn opencode-connection) session-id title &key success error)
  "Use CONN to rename SESSION-ID to TITLE."
  (opencode-request
   conn
   'PATCH
   (opencode-client--path "/api/session/%s" session-id)
   :json `((title . ,title))
   :parser #'opencode-client--no-content
   :success success
   :error error))

(cl-defmethod opencode-client-session-compact ((conn opencode-connection) session-id &key success error)
  "Queue compaction of SESSION-ID."
  (opencode-request
   conn
   'POST
   (opencode-client--path "/api/session/%s/compact" session-id)
   :json (make-hash-table)
   :success success
   :error error))

(cl-defmethod opencode-client-session-switch-agent
  ((conn opencode-connection) session-id agent &key success error)
  "Switch SESSION-ID to AGENT for subsequent turns."
  (opencode-request
   conn
   'POST
   (opencode-client--path "/api/session/%s/agent" session-id)
   :json `((agent . ,agent))
   :parser #'opencode-client--no-content
   :success success
   :error error))

(cl-defmethod opencode-client-session-switch-model
  ((conn opencode-connection) session-id model &key success error)
  "Switch SESSION-ID to MODEL for subsequent turns.

MODEL is a model reference alist with `providerID', `id', and optional
`variant'."
  (opencode-request
   conn
   'POST
   (opencode-client--path "/api/session/%s/model" session-id)
   :json `((model . ,model))
   :parser #'opencode-client--no-content
   :success success
   :error error))

(cl-defmethod opencode-client-agents ((conn opencode-connection) &key success error)
  "Fetch available agents from the server."
  (opencode-request conn 'GET "/api/agent"
                    :success (opencode-client--unwrap success)
                    :error error))

(cl-defmethod opencode-client-models ((conn opencode-connection) &key success error)
  "Fetch models available from connected providers."
  (opencode-request conn 'GET "/api/model"
                    :success (opencode-client--unwrap success)
                    :error error))

(cl-defmethod opencode-client-integrations ((conn opencode-connection) &key success error)
  "Fetch provider integrations and their authentication methods."
  (opencode-request conn 'GET "/api/integration"
                    :success (opencode-client--unwrap success)
                    :error error))

(cl-defmethod opencode-client-commands ((conn opencode-connection) &key success error)
  "Fetch available commands from the server."
  (opencode-request conn 'GET "/api/command"
                    :success (opencode-client--unwrap success)
                    :error error))

(cl-defmethod opencode-client-integration-connect-key
  ((conn opencode-connection) integration-id key &key answer success error)
  "Store KEY as the credential for INTEGRATION-ID.
ANSWER is an alist answering the method's form, when it has one."
  (opencode-request
   conn
   'POST
   (opencode-client--path "/api/integration/%s/connect/key" integration-id)
   :json (append `((key . ,key)) (when answer `((answer . ,answer))))
   :parser #'opencode-client--no-content
   :success success
   :error error))

(cl-defmethod opencode-client-integration-oauth-connect
  ((conn opencode-connection) integration-id method-id &key answer success error)
  "Start OAuth METHOD-ID for INTEGRATION-ID.
ANSWER is an alist answering the method's form, when it has one.  The
data describes the attempt: its ID, URL, instructions, and mode."
  (opencode-request
   conn
   'POST
   (opencode-client--path "/api/integration/%s/connect/oauth" integration-id)
   :json (append `((methodID . ,method-id)) (when answer `((answer . ,answer))))
   :success (opencode-client--unwrap success)
   :error error))

(cl-defmethod opencode-client-integration-oauth-status
  ((conn opencode-connection) integration-id attempt-id &key success error)
  "Fetch the status of OAuth ATTEMPT-ID for INTEGRATION-ID."
  (opencode-request
   conn
   'GET
   (opencode-client--path "/api/integration/%s/connect/oauth/%s" integration-id attempt-id)
   :success (opencode-client--unwrap success)
   :error error))

(cl-defmethod opencode-client-integration-oauth-complete
  ((conn opencode-connection) integration-id attempt-id &key code success error)
  "Complete code-based OAuth ATTEMPT-ID for INTEGRATION-ID with CODE."
  (opencode-request
   conn
   'POST
   (opencode-client--path "/api/integration/%s/connect/oauth/%s/complete"
                          integration-id attempt-id)
   :json (if code `((code . ,code)) (make-hash-table))
   :parser #'opencode-client--no-content
   :timeout nil
   :success success
   :error error))

(cl-defmethod opencode-client-pending-permissions ((conn opencode-connection) &key success error)
  "Fetch pending permission requests for every session in CONN's directory."
  (opencode-request conn 'GET "/api/permission/request"
                    :success (opencode-client--unwrap success)
                    :error error))

(cl-defmethod opencode-client-pending-forms ((conn opencode-connection) &key success error)
  "Fetch pending forms for every session in CONN's directory."
  (opencode-request conn 'GET "/api/form"
                    :success (opencode-client--unwrap success)
                    :error error))

(cl-defmethod opencode-client-session-inbox ((conn opencode-connection) session-id &key success error)
  "Fetch the inputs queued for SESSION-ID that it has not received yet."
  (opencode-request
   conn
   'GET
   (opencode-client--path "/api/session/%s/inbox" session-id)
   :success (opencode-client--unwrap success)
   :error error))

(cl-defmethod opencode-client-session-prompt
  ((conn opencode-connection) session-id text &key agents success error)
  "Send TEXT to SESSION-ID.

AGENTS is a list of agent names mentioned in TEXT; the server delegates to
them."
  (opencode-request
   conn
   'POST
   (opencode-client--path "/api/session/%s/prompt" session-id)
   :json (append `((text . ,text))
                 (when agents
                   `((agents . ,(apply #'vector
                                       (mapcar (lambda (name) `((name . ,name)))
                                               agents))))))
   :success (opencode-client--unwrap success)
   :error error))

(cl-defmethod opencode-client-session-interrupt ((conn opencode-connection) session-id &key success error)
  "Interrupt the active execution of SESSION-ID."
  (opencode-request
   conn
   'POST
   (opencode-client--path "/api/session/%s/interrupt" session-id)
   :success success
   :error error))

(cl-defmethod opencode-client-session-background ((conn opencode-connection) session-id &key success error)
  "Move blocking shell and subagent tool calls in SESSION-ID to the background."
  (opencode-request
   conn
   'POST
   (opencode-client--path "/api/session/%s/background" session-id)
   :parser #'opencode-client--no-content
   :success success
   :error error))

(cl-defmethod opencode-client-session-wait ((conn opencode-connection) session-id &key success error)
  "Wait until SESSION-ID becomes idle."
  (opencode-request
   conn
   'POST
   (opencode-client--path "/api/experimental/session/%s/wait" session-id)
   :parser #'opencode-client--no-content
   :timeout nil
   :success success
   :error error))

(cl-defmethod opencode-client-permission-reply
  ((conn opencode-connection) session-id request-id decision &key message success error)
  "Reply to permission REQUEST-ID in SESSION-ID with DECISION.

DECISION is \"once\", \"always\", or \"reject\".  MESSAGE is sent when
provided."
  (opencode-request
   conn
   'POST
   (opencode-client--path "/api/session/%s/permission/%s/reply" session-id request-id)
   :json (append `((decision . ,decision))
                 (when message `((message . ,message))))
   :parser #'opencode-client--no-content
   :success success
   :error error))

(cl-defmethod opencode-client-form-reply
  ((conn opencode-connection) session-id form-id answer &key success error)
  "Answer FORM-ID in SESSION-ID with ANSWER.
ANSWER is an alist mapping field keys to values."
  (opencode-request
   conn
   'POST
   (opencode-client--path "/api/session/%s/form/%s/reply" session-id form-id)
   :json `((answer . ,(or answer (make-hash-table))))
   :parser #'opencode-client--no-content
   :success success
   :error error))

(cl-defmethod opencode-client-form-cancel
  ((conn opencode-connection) session-id form-id &key success error)
  "Dismiss FORM-ID in SESSION-ID without answering it."
  (opencode-request
   conn
   'DELETE
   (opencode-client--path "/api/session/%s/form/%s" session-id form-id)
   :parser #'opencode-client--no-content
   :success success
   :error error))

(cl-defmethod opencode-client-session-command
  ((conn opencode-connection) session-id command arguments &key success error)
  "Run COMMAND with ARGUMENTS in SESSION-ID."
  (opencode-request
   conn
   'POST
   (opencode-client--path "/api/session/%s/command" session-id)
   :json `((name . ,command)
           (text . ,(or arguments "")))
   :parser #'opencode-client--no-content
   :timeout nil
   :success success
   :error error))

(cl-defmethod opencode-client-session-shell
  ((conn opencode-connection) session-id command &key success error)
  "Execute shell COMMAND in SESSION-ID.
The server answers once the command finishes, so there is no timeout."
  (opencode-request
   conn
   'POST
   (opencode-client--path "/api/session/%s/shell" session-id)
   :json `((command . ,command))
   :parser #'opencode-client--no-content
   :timeout nil
   :success success
   :error error))

(provide 'emacs-opencode-client)

;;; emacs-opencode-client.el ends here
