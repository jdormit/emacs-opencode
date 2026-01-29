;;; emacs-opencode-client.el --- OpenCode HTTP client  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'request)
(require 'subr-x)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-sse)

(declare-function opencode--json-read "emacs-opencode-sse")

(cl-defmethod opencode-request ((conn opencode-connection) method path &rest args &key data json parser headers &allow-other-keys)
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
                           headers)))
    (apply
     #'request
     url
     :type (symbol-name method)
     :data (or payload data)
     :parser (or parser #'opencode--json-read)
     :headers merged-headers
     :auth auth
     :timeout (or (opencode-connection-timeout conn) 10)
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

(cl-defmethod opencode-client-session-prompt-async ((conn opencode-connection) session-id parts &key success error agent)
  "Send PARTS to SESSION-ID asynchronously.

PARTS is a list of message part objects for the request body. AGENT is
included when provided." 
  (opencode-request
   conn
   'POST
   (format "/session/%s/prompt_async" session-id)
   :json (append (when agent `((agent . ,agent)))
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

(provide 'emacs-opencode-client)

;;; emacs-opencode-client.el ends here
