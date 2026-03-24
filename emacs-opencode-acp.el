;;; emacs-opencode-acp.el --- ACP JSON-RPC transport for OpenCode  -*- lexical-binding: t; -*-

;; ACP (Agent Client Protocol) transport layer.  Handles JSON-RPC 2.0
;; communication over newline-delimited JSON (ndjson) via stdin/stdout of
;; a child process running `opencode acp`.

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'emacs-opencode-connection)

;;; Customization

(defcustom opencode-acp-request-timeout 300
  "Timeout in seconds for ACP JSON-RPC requests.
Long timeout because `session/prompt' blocks until the agent finishes."
  :type 'integer
  :group 'emacs-opencode)

(defcustom opencode-acp-log-io nil
  "When non-nil, log JSON-RPC messages to the *opencode-acp-log* buffer."
  :type 'boolean
  :group 'emacs-opencode)

;;; Internal state

(defvar opencode-acp--next-id 0
  "Next JSON-RPC request ID.")

(defvar opencode-acp--pending-requests (make-hash-table :test 'eql)
  "Hash-table mapping request ID to (success-cb . error-cb . timer).")

(defvar opencode-acp--notification-handlers nil
  "Alist mapping JSON-RPC method strings to handler functions.
Each handler receives (METHOD PARAMS).")

(defvar opencode-acp--request-handlers nil
  "Alist mapping agent-to-client request method strings to handler functions.
Each handler receives (PARAMS) and returns a result value, or signals an
error which becomes a JSON-RPC error response.")

;;; JSON serialization
;;
;; Incoming JSON (from ACP agent) is parsed with :array-type 'list so
;; arrays become Lisp lists.  Outgoing JSON is serialized with
;; json-serialize which encodes vectors as JSON arrays and nil as null.
;; When constructing params that contain empty arrays, use [] (empty
;; vector) rather than nil to avoid serializing as null.

(defun opencode-acp--json-parse (string)
  "Parse JSON STRING to an alist/list structure."
  (json-parse-string string
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun opencode-acp--json-serialize (object)
  "Serialize OBJECT to a JSON string."
  (json-serialize object
                  :null-object nil
                  :false-object :json-false))

;;; Logging

(defun opencode-acp--log (direction message)
  "Log a JSON-RPC MESSAGE in DIRECTION (\"->\" or \"<-\")."
  (when opencode-acp-log-io
    (let ((buf (get-buffer-create "*opencode-acp-log*")))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format-time-string "%H:%M:%S.%3N")
                " " direction " "
                (if (stringp message)
                    message
                  (json-encode message))
                "\n")))))

;;; Process filter — ndjson framing

(defun opencode-acp--make-filter (process)
  "Return a process filter function for PROCESS.
Accumulates bytes, splits on newlines, parses JSON, dispatches."
  (let ((buffer ""))
    (lambda (_proc output)
      (setq buffer (concat buffer output))
      (let ((lines (split-string buffer "\n")))
        ;; All complete lines are everything except the last element.
        ;; The last element is either "" (if output ended with \n) or
        ;; a partial line still accumulating.
        (setq buffer (car (last lines)))
        (setq lines (butlast lines))
        (dolist (line lines)
          (unless (string-empty-p (string-trim line))
            (condition-case err
                (let ((msg (opencode-acp--json-parse line)))
                  (opencode-acp--log "<-" line)
                  (opencode-acp--handle-message process msg))
              (error
               (message "OpenCode ACP: failed to parse JSON-RPC message: %s"
                        (error-message-string err))))))))))

;;; Message dispatch

(defun opencode-acp--handle-message (process msg)
  "Dispatch a parsed JSON-RPC MSG from PROCESS."
  (let ((id (alist-get 'id msg))
        (method (alist-get 'method msg)))
    (cond
     ;; Response to a request we sent (has id, no method)
     ((and id (not method))
      (opencode-acp--handle-response msg))

     ;; Agent-to-client request (has both id and method)
     ((and id method)
      (opencode-acp--handle-agent-request process msg))

     ;; Notification (has method, no id)
     (method
      (opencode-acp--handle-notification msg))

     (t
      (message "OpenCode ACP: unrecognized message: %S" msg)))))

(defun opencode-acp--handle-response (msg)
  "Handle a JSON-RPC response MSG."
  (let* ((id (alist-get 'id msg))
         (entry (gethash id opencode-acp--pending-requests)))
    (when entry
      (remhash id opencode-acp--pending-requests)
      (let ((success-cb (nth 0 entry))
            (error-cb (nth 1 entry))
            (timer (nth 2 entry)))
        (when timer (cancel-timer timer))
        (if-let ((err (alist-get 'error msg)))
            (when error-cb
              (funcall error-cb err))
          (when success-cb
            (funcall success-cb (alist-get 'result msg))))))))

(defun opencode-acp--handle-notification (msg)
  "Handle a JSON-RPC notification MSG."
  (let* ((method (alist-get 'method msg))
         (params (alist-get 'params msg))
         (handlers (alist-get method opencode-acp--notification-handlers
                              nil nil #'string=)))
    (dolist (handler handlers)
      (condition-case err
          (funcall handler method params)
        (error
         (message "OpenCode ACP: notification handler error for %s: %s"
                  method (error-message-string err)))))))

(defun opencode-acp--handle-agent-request (process msg)
  "Handle an agent-to-client request MSG and send a response via PROCESS."
  (let* ((id (alist-get 'id msg))
         (method (alist-get 'method msg))
         (params (alist-get 'params msg))
         (handler (alist-get method opencode-acp--request-handlers
                             nil nil #'string=)))
    (if handler
        ;; Handler may be async — it can return a value immediately or
        ;; call the provided callback later.  For now, use synchronous
        ;; handlers that return a result or signal an error.
        (condition-case err
            (let ((result (funcall handler params)))
              (opencode-acp--send-response process id result))
          (error
           (opencode-acp--send-error-response
            process id -32603
            (format "Handler error: %s" (error-message-string err)))))
      ;; No handler registered — return method not found
      (opencode-acp--send-error-response
       process id -32601
       (format "Method not supported: %s" method)))))

;;; Sending messages

(defun opencode-acp--send (process message)
  "Send a JSON-RPC MESSAGE to PROCESS as ndjson."
  (let ((json-str (opencode-acp--json-serialize message)))
    (opencode-acp--log "->" json-str)
    (process-send-string process (concat json-str "\n"))))

(defun opencode-acp--send-response (process id result)
  "Send a JSON-RPC success response for request ID via PROCESS."
  (opencode-acp--send
   process
   `((jsonrpc . "2.0")
     (id . ,id)
     (result . ,(or result :json-false)))))

(defun opencode-acp--send-error-response (process id code message)
  "Send a JSON-RPC error response for request ID via PROCESS."
  (opencode-acp--send
   process
   `((jsonrpc . "2.0")
     (id . ,id)
     (error . ((code . ,code)
               (message . ,message))))))

(cl-defun opencode-acp-request (process method params &key success error)
  "Send a JSON-RPC request to PROCESS.

METHOD is the RPC method string.  PARAMS is an alist of parameters.
SUCCESS is called with the result on success.
ERROR is called with the error object on failure."
  (let* ((id opencode-acp--next-id)
         (timer (when (and opencode-acp-request-timeout
                           (> opencode-acp-request-timeout 0))
                  (run-at-time
                   opencode-acp-request-timeout nil
                   (lambda ()
                     (when-let ((entry (gethash id opencode-acp--pending-requests)))
                       (remhash id opencode-acp--pending-requests)
                       (when (nth 1 entry)
                         (funcall (nth 1 entry)
                                  `((code . -32000)
                                    (message . "Request timed out")))))))))
         (msg `((jsonrpc . "2.0")
                (id . ,id)
                (method . ,method)
                ,@(when params `((params . ,params))))))
    (setq opencode-acp--next-id (1+ opencode-acp--next-id))
    (puthash id (list success error timer) opencode-acp--pending-requests)
    (opencode-acp--send process msg)
    id))

(defun opencode-acp-notify (process method params)
  "Send a JSON-RPC notification to PROCESS.

METHOD is the RPC method string.  PARAMS is an alist of parameters.
Notifications have no response."
  (let ((msg `((jsonrpc . "2.0")
               (method . ,method)
               ,@(when params `((params . ,params))))))
    (opencode-acp--send process msg)))

;;; Handler registration

(defun opencode-acp-register-notification-handler (method handler)
  "Register HANDLER for notification METHOD.

HANDLER receives (METHOD PARAMS)."
  (let ((current (alist-get method opencode-acp--notification-handlers
                            nil nil #'string=)))
    (unless (memq handler current)
      (setf (alist-get method opencode-acp--notification-handlers
                       nil nil #'string=)
            (cons handler current)))))

(defun opencode-acp-register-request-handler (method handler)
  "Register HANDLER for agent-to-client request METHOD.

HANDLER receives (PARAMS) and returns a result value.  Signal an error
to return a JSON-RPC error response."
  (setf (alist-get method opencode-acp--request-handlers
                   nil nil #'string=)
        handler))

(defmacro opencode-acp-define-notification-handler (name method args &rest body)
  "Define and register a notification handler for METHOD.

NAME is used to construct the function name.  ARGS should be
\(METHOD PARAMS).  BODY is the handler body."
  (declare (indent defun))
  (let ((fn-name (intern (format "opencode-acp--%s-handler" name))))
    `(progn
       (defun ,fn-name ,args
         ,(format "Handle ACP notification %s." method)
         ,@body)
       (opencode-acp-register-notification-handler ,method #',fn-name)
       #',fn-name)))

(defmacro opencode-acp-define-request-handler (name method args &rest body)
  "Define and register an agent-to-client request handler for METHOD.

NAME is used to construct the function name.  ARGS should be (PARAMS).
BODY should return the result value or signal an error."
  (declare (indent defun))
  (let ((fn-name (intern (format "opencode-acp--%s-handler" name))))
    `(progn
       (defun ,fn-name ,args
         ,(format "Handle ACP agent request %s." method)
         ,@body)
       (opencode-acp-register-request-handler ,method #',fn-name)
       #',fn-name)))

;;; Process lifecycle

(defun opencode-acp-start (directory port &optional ready-callback)
  "Start an OpenCode ACP subprocess for DIRECTORY.

PORT is the TCP port for the embedded HTTP server.
READY-CALLBACK is called with the process when the ACP initialize
handshake succeeds.  Returns the process."
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (process-environment (opencode-connection--process-environment
                               opencode-server-environment))
         (command (list (executable-find opencode-server-command)
                        "acp"
                        "--port" (number-to-string port)
                        "--hostname" "127.0.0.1"))
         (stderr-buffer (get-buffer-create
                         (format " *opencode-acp-stderr<%s>*" default-directory)))
         (process (make-process
                   :name "opencode-acp"
                   :command command
                   :connection-type 'pipe
                   :noquery t
                   :stderr stderr-buffer)))
    ;; Install the ndjson process filter
    (set-process-filter process (opencode-acp--make-filter process))
    ;; Set sentinel for unexpected exits
    (set-process-sentinel
     process
     (lambda (proc event)
       (unless (string-prefix-p "open" event)
         (message "OpenCode ACP process %s: %s"
                  (process-name proc) (string-trim event)))))
    ;; Set coding system for clean binary I/O
    (set-process-coding-system process 'utf-8 'utf-8)
    ;; Send initialize request
    (opencode-acp-request
     process
     "initialize"
     `((protocolVersion . 1)
       (clientInfo . ((name . "emacs-opencode")
                      (version . "0.1")))
       (clientCapabilities . ((fs . ((readTextFile . t)
                                     (writeTextFile . t))))))
     :success (lambda (result)
                (process-put process 'opencode-acp-initialized t)
                (process-put process 'opencode-acp-capabilities
                             (alist-get 'agentCapabilities result))
                (when ready-callback
                  (funcall ready-callback process)))
     :error (lambda (err)
              (message "OpenCode ACP: initialize failed: %s"
                       (alist-get 'message err))))
    process))

(defun opencode-acp-stop (process)
  "Stop an ACP PROCESS by closing stdin and cleaning up."
  (when (and process (process-live-p process))
    ;; Close stdin to signal EOF
    (process-send-eof process)
    ;; Give it a moment to exit gracefully
    (run-at-time 2 nil
                 (lambda ()
                   (when (and process (process-live-p process))
                     (delete-process process))))
    ;; Clean up stderr buffer
    (when-let ((stderr-proc (get-process
                             (format "opencode-acp-stderr<%s>"
                                     (process-get process 'opencode-directory)))))
      (when-let ((buf (process-buffer stderr-proc)))
        (when (buffer-live-p buf)
          (kill-buffer buf))))
    ;; Cancel all pending request timers
    (maphash (lambda (_id entry)
               (when-let ((timer (nth 2 entry)))
                 (cancel-timer timer)))
             opencode-acp--pending-requests)
    (clrhash opencode-acp--pending-requests)))

(defun opencode-acp-reset ()
  "Reset ACP module state.  Useful for testing or reconnection."
  (setq opencode-acp--next-id 0)
  (clrhash opencode-acp--pending-requests))

(provide 'emacs-opencode-acp)

;;; emacs-opencode-acp.el ends here
