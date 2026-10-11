;;; emacs-opencode-session-handlers.el --- SSE event handlers  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-opencode-session-vars)
(require 'emacs-opencode-message)
(require 'emacs-opencode-client)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-session)
(require 'emacs-opencode-sse)

(declare-function opencode-session--update-session "emacs-opencode-session-mode")
(declare-function opencode-session--update-status "emacs-opencode-session-mode")
(declare-function opencode-session--render-header "emacs-opencode-session-header")
(declare-function opencode-session--update-message-part "emacs-opencode-session-mode")
(declare-function opencode-session--find-message "emacs-opencode-session-mode")
(declare-function opencode-session--adopt-model-from-message "emacs-opencode-session-mode")
(declare-function opencode-session--rename-buffer "emacs-opencode-session-mode")
(declare-function opencode-session--buffer-name "emacs-opencode-session-mode")
(declare-function opencode-session--sync-variant-selection "emacs-opencode-session-model")
(declare-function opencode-session--message-text "emacs-opencode-session-render")
(declare-function opencode-session--render-message "emacs-opencode-session-render")
(declare-function opencode-session--form-fields "emacs-opencode-session-render" (form))
(declare-function opencode-session--form-field-title "emacs-opencode-session-render" (field))
(declare-function opencode-session--render-forms "emacs-opencode-session-render")

(cl-defstruct (opencode-session--prompt-state
               (:constructor opencode-session--prompt-state-create))
  connection
  request-id
  status)

(defvar opencode-session--pending-prompts (make-hash-table :test #'eq)
  "Pending permission and form prompts grouped by connection.")

(defvar opencode-session--active-prompt nil
  "State for the OpenCode prompt currently using the minibuffer.")

(defvar-local opencode-session--minibuffer-prompt nil
  "OpenCode prompt state associated with this minibuffer.")

(defvar opencode-session--tool-names (make-hash-table :test #'eq)
  "Map connections to tool name registries.

Each connection maps tool call IDs to their tool names.  The V2
`session.tool.called'/`session.tool.success' events carry a tool
call ID but no name; the name arrives only in the preceding
`session.tool.input.started' event.")

(defun opencode-session--prompt-table (connection &optional create)
  "Return the pending prompt table for CONNECTION.
Create and register the table when CREATE is non-nil."
  (or (gethash connection opencode-session--pending-prompts)
      (when create
        (let ((table (make-hash-table :test #'equal)))
          (puthash connection table opencode-session--pending-prompts)
          table))))

(defun opencode-session--register-prompt (connection request-id)
  "Register REQUEST-ID as pending on CONNECTION.
Return its new state, or nil when it is already registered."
  (let ((table (opencode-session--prompt-table connection t)))
    (unless (gethash request-id table)
      (let ((state (opencode-session--prompt-state-create
                    :connection connection
                    :request-id request-id
                    :status 'pending)))
        (puthash request-id state table)
        state))))

(defun opencode-session--remove-prompt (state)
  "Remove pending prompt STATE from its connection registry."
  (let* ((connection (opencode-session--prompt-state-connection state))
         (request-id (opencode-session--prompt-state-request-id state))
         (table (opencode-session--prompt-table connection)))
    (when (and table (eq (gethash request-id table) state))
      (remhash request-id table)
      (when (zerop (hash-table-count table))
        (remhash connection opencode-session--pending-prompts)))))

(defun opencode-session--prompt-active-p (state)
  "Return non-nil when STATE owns the active minibuffer."
  (and (eq state opencode-session--active-prompt)
       (when-let* ((window (active-minibuffer-window)))
         (with-current-buffer (window-buffer window)
           (eq state opencode-session--minibuffer-prompt)))))

(defun opencode-session--run-prompt (state function)
  "Run prompt FUNCTION for STATE, cleaning up if it fails."
  (condition-case err
      (funcall function)
    (error
     (when (eq (opencode-session--prompt-state-status state) 'pending)
       (setf (opencode-session--prompt-state-status state) 'failed)
       (opencode-session--remove-prompt state))
     (signal (car err) (cdr err)))))

(defun opencode-session--prompt-request-id (data)
  "Return the prompt request ID from resolved-event DATA, if any."
  (or (alist-get 'requestID data)
      (alist-get 'requestId data)
      (alist-get 'id data)))

(defun opencode-session--handle-prompt-resolved (data meta)
  "Handle a prompt completion SSE DATA arriving with META."
  (let* ((payload (opencode-session--payload data))
         (request-id (opencode-session--prompt-request-id payload))
         (connection (plist-get meta :connection))
         (table (opencode-session--prompt-table connection))
         (state (and table (gethash request-id table))))
    (when state
      (setf (opencode-session--prompt-state-status state) 'resolved)
      (opencode-session--remove-prompt state)
      (when (opencode-session--prompt-active-p state)
        (message "OpenCode request answered in another client")
        (abort-recursive-edit)))))

;;; V2 event envelope helpers

(defun opencode-session--payload (data)
  "Return the V2 event payload from an event envelope DATA.

V2 events arrive as `{id, created, type, location, data, durable}';
handlers receive the whole envelope and read the payload from the
`data' key."
  (alist-get 'data data))

(defun opencode-session--in-session-buffer (session-id function)
  "Run FUNCTION in the live buffer for SESSION-ID, when there is one."
  (when-let* ((buffer (opencode-session--buffer-for-session session-id)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (funcall function)))))

;;; Session event handlers

(defun opencode-session--handle-session-upsert (data)
  "Handle session.created/session.updated SSE DATA."
  (let* ((info (opencode-session--payload data))
         (session-id (or (alist-get 'sessionID info)
                         (alist-get 'id info))))
    (opencode-session--in-session-buffer
     session-id
     (lambda ()
       (opencode-session--update-session info)))))

(defun opencode-session--handle-session-renamed (data)
  "Handle the session.renamed SSE DATA by retitling the buffer."
  (let* ((payload (opencode-session--payload data))
         (session-id (alist-get 'sessionID payload))
         (title (alist-get 'title payload)))
    (opencode-session--in-session-buffer
     session-id
     (lambda ()
       (when-let* ((session opencode-session--session))
         (let ((previous-name (opencode-session--buffer-name session)))
           (setf (opencode-session-title session) title)
           (setf (opencode-session-info session)
                 (cons (cons 'title title)
                       (assoc-delete-all 'title (opencode-session-info session))))
           (opencode-session--rename-buffer previous-name)
           (opencode-session--render-header)))))))

(defun opencode-session--handle-execution-started (data)
  "Handle the session.execution.started SSE DATA."
  (let* ((payload (opencode-session--payload data))
         (session-id (alist-get 'sessionID payload))
         (status (opencode-status-create :type "busy")))
    (opencode-session--update-status session-id status)))

(defun opencode-session--handle-execution-finished (data)
  "Handle the session.execution.succeeded/failed SSE DATA.

On failure, surface the error in the echo area only when no session
buffer is open (step failures render the error inline in the buffer)."
  (let* ((payload (opencode-session--payload data))
         (session-id (alist-get 'sessionID payload))
         (error-info (alist-get 'error payload))
         (status (opencode-status-create :type "idle")))
    (opencode-session--update-status session-id status)
    (when (and error-info
               (null (opencode-session--buffer-for-session session-id))
               (not (opencode-session--abort-error-p error-info)))
      (message "OpenCode: %s"
               (opencode-session--session-error-text error-info)))))

(defun opencode-session--abort-error-p (error-info)
  "Return non-nil when ERROR-INFO describes a user aborted message."
  (let ((name (or (alist-get 'type error-info) (alist-get 'name error-info))))
    (string= name "MessageAbortedError")))

(defun opencode-session--session-error-text (error-info)
  "Return user-facing text extracted from session ERROR-INFO.
Handles both the V1 `(name data.(message))' and V2
`(type message status)' shapes."
  (let* ((data (and (listp error-info) (alist-get 'data error-info)))
         (detail (or (and (listp data) (alist-get 'message data))
                     (and (listp error-info) (alist-get 'message error-info)))))
    (cond
     ((and (stringp detail) (not (string-empty-p detail))) detail)
     ((and (listp error-info)
           (stringp (alist-get 'name error-info))
           (not (string-empty-p (alist-get 'name error-info))))
      (alist-get 'name error-info))
     ((stringp error-info) error-info)
     (t "An error occurred"))))

;;; Message stream handlers

(defun opencode-session--ensure-assistant-message (payload)
  "Return the assistant message for step/text/tool PAYLOAD.
Creates and registers the message when missing, adopting the agent
and model from PAYLOAD when present."
  (let* ((session-id (alist-get 'sessionID payload))
         (message-id (alist-get 'assistantMessageID payload))
         (message (opencode-session--find-message message-id)))
    (unless message
      (setq message (opencode-message-create
                     :id message-id
                     :session-id session-id
                     :role "assistant"))
      (setq opencode-session--messages
            (append opencode-session--messages (list message))))
    (when-let* ((agent (alist-get 'agent payload)))
      (setf (opencode-message-agent message) agent))
    (when-let* ((model (alist-get 'model payload)))
      (setf (opencode-message-model-id message) (alist-get 'id model))
      (setf (opencode-message-provider-id message) (alist-get 'providerID model)))
    (opencode-session--adopt-model-from-message message)
    (opencode-session--render-header)
    message))

(defun opencode-session--stream-part-update (payload part &optional delta)
  "Apply streaming PART update for step PAYLOAD with optional DELTA.
Routes to the session buffer, ensuring the assistant message
exists first.  Does nothing when the session has no live buffer."
  (let ((session-id (alist-get 'sessionID payload)))
    (opencode-session--in-session-buffer
     session-id
     (lambda ()
       (opencode-session--ensure-assistant-message payload)
       (opencode-session--update-message-part part delta)))))

(defun opencode-session--stream-part-id (prefix ordinal)
  "Return a part ID for PREFIX and ORDINAL, e.g. \"text-0\"."
  (format "%s-%s" prefix ordinal))

(defun opencode-session--handle-text-started (data)
  "Handle the session.text.started SSE DATA."
  (let* ((payload (opencode-session--payload data))
         (part-id (opencode-session--stream-part-id
                   "text" (alist-get 'ordinal payload)))
         (part `((id . ,part-id)
                 (sessionID . ,(alist-get 'sessionID payload))
                 (messageID . ,(alist-get 'assistantMessageID payload))
                 (type . "text")
                 (text . ""))))
    (opencode-session--stream-part-update payload part nil)))

(defun opencode-session--handle-text-delta (data)
  "Handle the session.text.delta SSE DATA."
  (let* ((payload (opencode-session--payload data))
         (delta (alist-get 'delta payload))
         (part-id (opencode-session--stream-part-id
                   "text" (alist-get 'ordinal payload)))
         (part `((id . ,part-id)
                 (sessionID . ,(alist-get 'sessionID payload))
                 (messageID . ,(alist-get 'assistantMessageID payload))
                 (type . "text")
                 (text . ,delta))))
    (opencode-session--stream-part-update payload part delta)))

(defun opencode-session--handle-text-ended (data)
  "Handle the session.text.ended SSE DATA."
  (let* ((payload (opencode-session--payload data))
         (text (alist-get 'text payload))
         (part-id (opencode-session--stream-part-id
                   "text" (alist-get 'ordinal payload)))
         (part `((id . ,part-id)
                 (sessionID . ,(alist-get 'sessionID payload))
                 (messageID . ,(alist-get 'assistantMessageID payload))
                 (type . "text")
                 (text . ,text))))
    (opencode-session--stream-part-update payload part nil)))

(defun opencode-session--handle-reasoning-started (data)
  "Handle the session.reasoning.started SSE DATA."
  (let* ((payload (opencode-session--payload data))
         (part-id (opencode-session--stream-part-id
                   "reasoning" (alist-get 'ordinal payload)))
         (part `((id . ,part-id)
                 (sessionID . ,(alist-get 'sessionID payload))
                 (messageID . ,(alist-get 'assistantMessageID payload))
                 (type . "reasoning")
                 (text . ""))))
    (opencode-session--stream-part-update payload part nil)))

(defun opencode-session--handle-reasoning-delta (data)
  "Handle the session.reasoning.delta SSE DATA."
  (let* ((payload (opencode-session--payload data))
         (delta (alist-get 'delta payload))
         (part-id (opencode-session--stream-part-id
                   "reasoning" (alist-get 'ordinal payload)))
         (part `((id . ,part-id)
                 (sessionID . ,(alist-get 'sessionID payload))
                 (messageID . ,(alist-get 'assistantMessageID payload))
                 (type . "reasoning")
                 (text . ,delta))))
    (opencode-session--stream-part-update payload part delta)))

(defun opencode-session--handle-reasoning-ended (data)
  "Handle the session.reasoning.ended SSE DATA."
  (let* ((payload (opencode-session--payload data))
         (text (alist-get 'text payload))
         (part-id (opencode-session--stream-part-id
                   "reasoning" (alist-get 'ordinal payload)))
         (part `((id . ,part-id)
                 (sessionID . ,(alist-get 'sessionID payload))
                 (messageID . ,(alist-get 'assistantMessageID payload))
                 (type . "reasoning")
                 (text . ,text))))
    (opencode-session--stream-part-update payload part nil)))

;;; Tool stream handlers

(defun opencode-session--tool-name-registry (connection)
  "Return the tool name registry for CONNECTION, creating it as needed."
  (or (gethash connection opencode-session--tool-names)
      (let ((table (make-hash-table :test #'equal)))
        (puthash connection table opencode-session--tool-names)
        table)))

(defun opencode-session--remember-tool-name (connection tool-id name)
  "Remember that tool call TOOL-ID in CONNECTION is named NAME."
  (when (and connection tool-id name)
    (puthash tool-id name (opencode-session--tool-name-registry connection))))

(defun opencode-session--tool-name-for (connection tool-id)
  "Return the tool name remembered for TOOL-ID in CONNECTION."
  (when (and connection tool-id)
    (gethash tool-id (opencode-session--tool-name-registry connection))))

(defun opencode-session--handle-tool-input-started (data meta)
  "Handle the session.tool.input.started SSE DATA arriving with META."
  (let* ((payload (opencode-session--payload data))
         (tool-id (alist-get 'id payload))
         (name (alist-get 'name payload))
         (part `((id . ,tool-id)
                 (sessionID . ,(alist-get 'sessionID payload))
                 (messageID . ,(alist-get 'assistantMessageID payload))
                 (type . "tool")
                 (tool . ,name)
                 (state . ((status . "streaming"))))))
    (opencode-session--remember-tool-name (plist-get meta :connection) tool-id name)
    (opencode-session--stream-part-update payload part nil)))

(defun opencode-session--handle-tool-input-ended (data)
  "Handle the session.tool.input.ended SSE DATA.

The event carries the tool input as a raw JSON string in `text'."
  (let* ((payload (opencode-session--payload data))
         (tool-id (alist-get 'id payload))
         (text (alist-get 'text payload))
         (part `((id . ,tool-id)
                 (sessionID . ,(alist-get 'sessionID payload))
                 (messageID . ,(alist-get 'assistantMessageID payload))
                 (type . "tool")
                 (state . ((status . "streaming") (input . ,text))))))
    (opencode-session--stream-part-update payload part nil)))

(defun opencode-session--handle-tool-called (data)
  "Handle the session.tool.called SSE DATA.

The event carries the parsed tool INPUT alist, so the part moves
from streaming to running."
  (let* ((payload (opencode-session--payload data))
         (tool-id (alist-get 'id payload))
         (input (alist-get 'input payload))
         (part `((id . ,tool-id)
                 (sessionID . ,(alist-get 'sessionID payload))
                 (messageID . ,(alist-get 'assistantMessageID payload))
                 (type . "tool")
                 (state . ((status . "running") (input . ,input))))))
    (opencode-session--stream-part-update payload part nil)))

(defun opencode-session--handle-tool-progress (data)
  "Handle the session.tool.progress SSE DATA."
  (let* ((payload (opencode-session--payload data))
         (tool-id (alist-get 'id payload))
         (metadata (alist-get 'metadata payload))
         (part `((id . ,tool-id)
                 (sessionID . ,(alist-get 'sessionID payload))
                 (messageID . ,(alist-get 'assistantMessageID payload))
                 (type . "tool")
                 (state . ((status . "running") (metadata . ,metadata))))))
    (opencode-session--stream-part-update payload part nil)))

(defun opencode-session--tool-output-path-p (tool input)
  "Return non-nil when TOOL writes a file and INPUT carries a path."
  (and (member tool '("write" "edit" "apply_patch" "fork"))
       (let ((path (or (alist-get 'filePath input) (alist-get 'path input))))
         (and (stringp path) (not (string-empty-p path))))))

(defun opencode-session--handle-tool-success (data meta)
  "Handle the session.tool.success SSE DATA arriving with META.

The event carries the tool output in CONTENT; the state mapping
stores it as `output' via `opencode-session--normalize-part-state'.
After a successful file-writing tool, revert buffers showing the
edited path (V2 emits no `file.edited' event for tool edits)."
  (let* ((payload (opencode-session--payload data))
         (tool-id (alist-get 'id payload))
         (content (alist-get 'content payload))
         (metadata (alist-get 'metadata payload))
         (tool (opencode-session--tool-name-for (plist-get meta :connection) tool-id))
         (part `((id . ,tool-id)
                 (sessionID . ,(alist-get 'sessionID payload))
                 (messageID . ,(alist-get 'assistantMessageID payload))
                 (type . "tool")
                 (tool . ,tool)
                 (state . ((status . "completed")
                           (content . ,content)
                           (metadata . ,metadata)))))
         (input (and (listp metadata) (alist-get 'input metadata))))
    (opencode-session--stream-part-update payload part nil)
    (when (opencode-session--tool-output-path-p tool input)
      (dolist (path (list (or (alist-get 'filePath input)
                              (alist-get 'path input))))
        (opencode-session--maybe-revert-buffer path)))))

;;; Step lifecycle handlers

(defun opencode-session--handle-step-ended (data)
  "Handle the session.step.ended SSE DATA by recording finish data."
  (let* ((payload (opencode-session--payload data))
         (session-id (alist-get 'sessionID payload))
         (message-id (alist-get 'assistantMessageID payload)))
    (opencode-session--in-session-buffer
     session-id
     (lambda ()
       (when-let* ((message (opencode-session--find-message message-id)))
         (setf (opencode-message-finish message) (alist-get 'finish payload))
         (unless (opencode-message-time-completed message)
           (setf (opencode-message-time-completed message)
                 (alist-get 'created data)))
         (setf (opencode-message-info message)
               `((sessionID . ,session-id)
                 (finish . ,(alist-get 'finish payload))
                 (cost . ,(alist-get 'cost payload))
                 (tokens . ,(alist-get 'tokens payload)))))))))

(defun opencode-session--handle-step-failed (data)
  "Handle the session.step.failed SSE DATA by attaching the error."
  (let* ((payload (opencode-session--payload data))
         (session-id (alist-get 'sessionID payload))
         (message-id (alist-get 'assistantMessageID payload))
         (error-info (alist-get 'error payload)))
    (opencode-session--in-session-buffer
     session-id
     (lambda ()
       (when-let* ((message (opencode-session--find-message message-id)))
         (setf (opencode-message-error message) error-info)
         (opencode-session--render-message message))))))

;;; User inbox

(defun opencode-session--handle-inbox-enqueued (data)
  "Handle the session.inbox.enqueued SSE DATA by rendering the message.
The server creates the user message when a prompt is sent; this
event carries its text so the client can render it immediately."
  (let* ((payload (opencode-session--payload data))
         (session-id (alist-get 'sessionID payload))
         (message-id (or (alist-get 'inboxID payload)
                         (alist-get 'messageID payload)))
         (item (alist-get 'item payload))
         (type (alist-get 'type item))
         (text (alist-get 'text (alist-get 'payload item))))
    (when (and session-id message-id
               (member type '("user" "synthetic")))
      (opencode-session--in-session-buffer
       session-id
       (lambda ()
         (let ((message (opencode-session--find-message message-id)))
           (unless message
             (setq message (opencode-message-create
                            :id message-id
                            :session-id session-id
                            :role type))
             (setq opencode-session--messages
                   (append opencode-session--messages (list message))))
           (setf (opencode-message-text message) text)
           (opencode-session--render-message message)
           (opencode-session--render-header)))))))

;;; Agent / model selection

(defun opencode-session--handle-agent-selected (data)
  "Handle the session.agent.selected SSE DATA."
  (let* ((payload (opencode-session--payload data))
         (session-id (alist-get 'sessionID payload))
         (agent (alist-get 'agent payload)))
    (opencode-session--in-session-buffer
     session-id
     (lambda ()
       (setq-local opencode-session--agent agent)
       (opencode-session--render-header)))))

(defun opencode-session--handle-model-selected (data)
  "Handle the session.model.selected SSE DATA."
  (let* ((payload (opencode-session--payload data))
         (session-id (alist-get 'sessionID payload))
         (model (alist-get 'model payload)))
    (when (and model (stringp (alist-get 'id model)))
      (opencode-session--in-session-buffer
       session-id
       (lambda ()
         (setq-local opencode-session--provider-id (alist-get 'providerID model))
         (setq-local opencode-session--model-id (alist-get 'id model))
         (setq-local opencode-session--variant (alist-get 'variant model))
         (opencode-session--sync-variant-selection)
         (opencode-session--render-header))))))

;;; Subagent tracking

(defun opencode-session--track-subagent-tool (subagent-session-id part)
  "Track a tool PART from SUBAGENT-SESSION-ID and re-render the parent."
  (let* ((part-id (alist-get 'id part))
         (tool (alist-get 'tool part))
         (state (alist-get 'state part))
         (parent-info (opencode-session--subagent-parent subagent-session-id)))
    (when (and parent-info tool)
      (opencode-session--update-subagent-tool
       subagent-session-id part-id tool state)
      ;; Re-render the task tool part in the parent session buffer
      (let* ((parent-session-id (car parent-info))
             (parent-buffer (opencode-session--buffer-for-session parent-session-id)))
        (when (and parent-buffer (buffer-live-p parent-buffer))
          (with-current-buffer parent-buffer
            (opencode-session--rerender-task-part parent-info)))))))

(defun opencode-session--rerender-task-part (parent-info)
  "Re-render the task tool part identified by PARENT-INFO.
PARENT-INFO is a cons (PARENT-SESSION-ID . TASK-PART-ID)."
  (let ((task-part-id (cdr parent-info)))
    (when-let* ((message (opencode-session--find-message-by-part task-part-id)))
      (opencode-session--render-message message))))

(defun opencode-session--find-message-by-part (part-id)
  "Find the message containing PART-ID in the current buffer."
  (cl-find-if (lambda (message)
                (assoc part-id (opencode-message-parts message)))
              opencode-session--messages))

(defun opencode-session--maybe-register-subagent (part parent-session-id)
  "Register subagent mapping if PART is a task tool with a session ID.
PARENT-SESSION-ID is the session that owns the task tool part."
  (when (and (string= (alist-get 'type part) "tool")
             (string= (alist-get 'tool part) "task"))
    (let* ((state (alist-get 'state part))
           (input (alist-get 'input state))
           (metadata (alist-get 'metadata state))
           (subagent-session-id (or (alist-get 'sessionId metadata)
                                    (alist-get 'sessionId input)))
           (part-id (alist-get 'id part)))
      (when subagent-session-id
        (opencode-session--register-subagent
         subagent-session-id parent-session-id part-id)))))

;;; Permission handling

(defun opencode-session--permission-patterns (permission)
  "Return a list of resource strings from a V2 PERMISSION."
  (let ((resources (alist-get 'resources permission)))
    (cond
     ((vectorp resources) (append resources nil))
     ((listp resources) resources)
     (t nil))))

(defun opencode-session--permission-detail (permission)
  "Return a detail string for PERMISSION when available.

V2 permission requests carry an ACTION and RESOURCES, e.g. action
\"read\" with resources (\"foo.el\").  Returns nil when there is no
resource so the prompt label can fall back to the action name."
  (let* ((action (alist-get 'action permission))
         (resources (opencode-session--permission-patterns permission))
         (resource (car resources)))
    (cond
     ((and (stringp resource) (not (string-empty-p resource))
           (stringp action) (not (string-empty-p action)))
      (format "%s %s" action resource))
     ((and (stringp resource) (not (string-empty-p resource)))
      resource)
     (t nil))))

(defun opencode-session--permission-prompt-label (permission)
  "Return the minibuffer prompt label for PERMISSION."
  (let* ((action (alist-get 'action permission))
         (detail (opencode-session--permission-detail permission))
         (fallback (if action (format "use %s" action) "proceed")))
    (format "OpenCode wants to %s: " (or detail fallback))))

(defun opencode-session--prompt-permission (permission session-id connection state)
  "Prompt for PERMISSION in SESSION-ID and respond via CONNECTION.
STATE tracks whether another client resolves the request."
  (let* ((request-id (alist-get 'id permission))
         (choices '("Allow once" "Allow always" "Deny"))
         (prompt (opencode-session--permission-prompt-label permission))
         (selection
          (let ((opencode-session--active-prompt state))
            (condition-case nil
                (minibuffer-with-setup-hook
                    (lambda ()
                      (setq-local opencode-session--minibuffer-prompt state))
                  (completing-read prompt choices nil t))
              (quit (unless (eq (opencode-session--prompt-state-status state)
                                'resolved)
                      "Deny")))))
         (reply (cond
                  ((equal selection "Allow always") "always")
                  ((equal selection "Allow once") "once")
                  (t "reject"))))
    (unless connection
      (error "OpenCode session is not connected"))
    (unless request-id
      (error "OpenCode permission request is missing ID"))
    (unless (eq (opencode-session--prompt-state-status state) 'resolved)
      (setf (opencode-session--prompt-state-status state) 'answered)
      (opencode-session--remove-prompt state)
      (opencode-client-permission-reply
       connection
       session-id
       request-id
       reply
       :success (lambda (&rest _args)
                  (message "OpenCode permission reply sent"))
       :error (lambda (&rest _args)
                (message "OpenCode: failed to reply to permission request"))))))

(defun opencode-session--handle-permission-asked (data meta)
  "Handle the permission.asked SSE DATA arriving with META.
META is a plist carrying `:connection', the connection on which
the event arrived; the reply is always routed back to that
connection so multi-server setups respond to the correct server.
Requests from subagent sessions use their own SESSION-ID.  Defers
to a timer so `completing-read' does not block the process filter."
  (let* ((permission (opencode-session--payload data))
         (session-id (alist-get 'sessionID permission))
         (connection (plist-get meta :connection))
         (state (opencode-session--register-prompt
                 connection (alist-get 'id permission))))
    (when state
      (run-at-time 0 nil
       (lambda ()
         (when (eq (opencode-session--prompt-state-status state) 'pending)
           (opencode-session--run-prompt
            state
            (lambda ()
              (opencode-session--prompt-permission
               permission session-id connection state)))))))))

;;; Question handling
;;; Form-based questions

;; The V2 server asks structured questions by creating forms on a
;; session.  The SSE `form.created' event carries the full form, and
;; `form.replied' / `form.cancelled' settle it.  Fields are prompted
;; one at a time in the minibuffer and the collected answers are
;; posted back to /api/session/<id>/form/<formID>/reply.

(defun opencode-session--form-option-label (option)
  "Return the display label of form OPTION."
  (let ((label (alist-get 'label option))
        (value (alist-get 'value option)))
    (cond ((and (stringp label) (not (string-empty-p label))) label)
          ((stringp value) value)
          (t ""))))

(defun opencode-session--form-option-value (option)
  "Return the submitted value of form OPTION."
  (let ((value (alist-get 'value option))
        (label (alist-get 'label option)))
    (cond ((stringp value) value)
          ((and (stringp label) (not (string-empty-p label))) label)
          (t ""))))

(defun opencode-session--form-option-labels (options)
  "Return display labels for OPTIONS."
  (mapcar #'opencode-session--form-option-label options))

(defun opencode-session--form-option-for-label (options label)
  "Return the OPTION in OPTIONS whose label is LABEL, or nil."
  (cl-find-if (lambda (option)
                (string= (opencode-session--form-option-label option) label))
              options))

(defun opencode-session--form-condition-p (condition answers)
  "Return non-nil when WHEN CONDITION matches ANSWERS.

CONDITION is (key op value) with op one of \"eq\" or \"neq\";
ANSWERS is the answer alist collected so far, keyed by the string
field keys."
  (let* ((key (alist-get 'key condition))
         (op (alist-get 'op condition))
         (want (alist-get 'value condition))
         (got (alist-get key answers nil nil #'equal))
         (match (equal got want)))
    (if (string= op "neq") (not match) match)))

(defun opencode-session--form-field-visible-p (field answers)
  "Return non-nil when FIELD is shown given ANSWERS so far.
Hides fields marked hidden and fields whose `when' conditions do not
match ANSWERS."
  (and (not (eq (alist-get 'hidden field) t))
       (cl-every (lambda (condition)
                   (opencode-session--form-condition-p condition answers))
                 (opencode-session--normalize-items (alist-get 'when field)))))

(defun opencode-session--form-field-prompt (field)
  "Return the minibuffer prompt for FIELD."
  (let ((title (opencode-session--form-field-title field))
        (required (eq (alist-get 'required field) t)))
    (format "OpenCode: %s%s " title (if required " (required)" ""))))

(defun opencode-session--form-string-valid-p (field value)
  "Return non-nil when VALUE satisfies FIELD length and pattern rules.
A pattern that is not a valid Emacs regexp is ignored.  Matching is
case-sensitive to mirror the server's validation."
  (let* ((min-length (alist-get 'minLength field))
         (max-length (alist-get 'maxLength field))
         (pattern (alist-get 'pattern field))
         (length (length value)))
    (and (or (null min-length) (>= length min-length))
         (or (null max-length) (<= length max-length))
         (or (null pattern)
             (condition-case nil
                 (let ((case-fold-search nil))
                   (string-match-p pattern value))
               (invalid-regexp t))))))

(defun opencode-session--form-read-validated-string (field prompt &optional suffix initial)
  "Read a string answer for FIELD with PROMPT, retrying while invalid.
SUFFIX is appended to PROMPT and INITIAL seeds the minibuffer.
Required fields must be non-empty and values must pass FIELD's length
and pattern rules."
  (cl-loop
   for value = (read-string (concat prompt (or suffix "")) (or initial ""))
   repeat 8
   when (and (or (not (eq (alist-get 'required field) t))
                 (not (string-empty-p value)))
             (opencode-session--form-string-valid-p field value))
   return value
   finally (error "OpenCode: no valid answer for %s"
                  (opencode-session--form-field-title field))))

(defun opencode-session--form-read-string (field)
  "Read a string answer for FIELD via options or free text."
  (let* ((options (opencode-session--normalize-items (alist-get 'options field)))
         (custom (eq (alist-get 'custom field) t))
         (prompt (opencode-session--form-field-prompt field))
         (default (or (alist-get 'default field) "")))
    (if options
        (let* ((labels (append (opencode-session--form-option-labels options)
                               (when custom '("Custom..."))))
               (choice (completing-read prompt labels nil t nil nil default)))
          (if (and custom (string= choice "Custom..."))
              (opencode-session--form-read-validated-string field prompt "Other: " "")
            (opencode-session--form-option-value
             (opencode-session--form-option-for-label options choice))))
      (opencode-session--form-read-validated-string field prompt nil default))))

(defun opencode-session--form-number-valid-p (field value)
  "Return non-nil when VALUE is within FIELD's minimum and maximum."
  (let* ((minimum (alist-get 'minimum field))
         (maximum (alist-get 'maximum field)))
    (and (or (null minimum) (not (numberp minimum)) (>= value minimum))
         (or (null maximum) (not (numberp maximum)) (<= value maximum)))))

(defun opencode-session--form-read-number (field)
  "Read a numeric answer for FIELD, retrying while out of range."
  (let ((integer-p (string= (alist-get 'type field) "integer")))
    (cl-loop
     for value = (read-number (opencode-session--form-field-prompt field)
                              (or (alist-get 'default field) 0))
     repeat 8
     when (and (or (not integer-p) (integerp value))
               (opencode-session--form-number-valid-p field value))
     return value
     finally (error "OpenCode: no valid answer for %s"
                    (opencode-session--form-field-title field)))))

(defun opencode-session--form-read-boolean (field)
  "Read a yes/no answer for FIELD, returning t or `:json-false'.
The schema default is shown in the prompt but the user always types
y or n."
  (let* ((default (alist-get 'default field))
         (prompt (concat (opencode-session--form-field-prompt field)
                         (format "(default %s) " (if default "yes" "no")))))
    (if (y-or-n-p prompt) t :json-false)))

(defun opencode-session--form-multiselect-valid-p (field chosen)
  "Return non-nil when CHOSEN labels satisfy FIELD's min/max items."
  (let ((count (length chosen))
        (min-items (alist-get 'minItems field))
        (max-items (alist-get 'maxItems field)))
    (and (or (null min-items) (>= count min-items))
         (or (null max-items) (<= count max-items)))))

(defun opencode-session--form-read-multiselect (field)
  "Read a multiselect answer for FIELD, returning a vector of values."
  (let* ((options (opencode-session--normalize-items (alist-get 'options field)))
         (custom (eq (alist-get 'custom field) t))
         (labels (append (opencode-session--form-option-labels options)
                         (when custom '("Custom..."))))
         (prompt (opencode-session--form-field-prompt field)))
    (cl-loop
     for chosen = (completing-read-multiple prompt labels nil t)
     repeat 5
     when (opencode-session--form-multiselect-valid-p field chosen)
     return (apply #'vector
                   (mapcar (lambda (label)
                             (if (and custom (string= label "Custom..."))
                                 (opencode-session--form-read-validated-string
                                  field prompt "Other: " "")
                               (opencode-session--form-option-value
                                (opencode-session--form-option-for-label
                                 options label))))
                           chosen))
     finally (error "OpenCode: no valid answer for %s"
                    (opencode-session--form-field-title field)))))

(defun opencode-session--form-read-field (field answers)
  "Prompt for FIELD given ANSWERS so far, returning its value.
External fields are not prompted; they are acknowledged with a non-nil
value when the form is answered.  Returns nil for unknown field types."
  (ignore answers)
  (pcase (alist-get 'type field)
    ("string" (opencode-session--form-read-string field))
    ("number" (opencode-session--form-read-number field))
    ("integer" (opencode-session--form-read-number field))
    ("boolean" (opencode-session--form-read-boolean field))
    ("multiselect" (opencode-session--form-read-multiselect field))
    ("external" t)
    (_ nil)))

(defun opencode-session--form-answers (form)
  "Collect answers for FORM via minibuffer prompts.

Returns an alist of (KEY . VALUE) pairs with JSON-safe values: vectors
for multiselect and t or `:json-false' for booleans.  Hidden fields
and fields whose `when' conditions do not match earlier answers are
skipped."
  (let ((answers nil))
    (dolist (field (opencode-session--form-fields form))
      (when (opencode-session--form-field-visible-p field answers)
        (let ((value (opencode-session--form-read-field field answers)))
          (when value
            (push (cons (alist-get 'key field) value) answers)))))
    (nreverse answers)))

(defun opencode-session--mark-form-settled (session-id form-id status &optional answers self-answered)
  "Update the rendered form FORM-ID in SESSION-ID to STATUS.
ANSWERS is the answer alist for the answered status.  SELF-ANSWERED
marks the form as answered by this client.  Does nothing when the
session has no live buffer."
  (opencode-session--in-session-buffer
   session-id
   (lambda ()
     (when-let* ((form-state (opencode-session--find-form form-id)))
       (setf (opencode-session--form-state-status form-state) status)
       (setf (opencode-session--form-state-self-answered form-state)
             self-answered)
       (when answers
         (setf (opencode-session--form-state-answers form-state) answers))
       (opencode-session--render-forms)))))

(defun opencode-session--prompt-form (form session-id connection state)
  "Prompt for FORM in SESSION-ID and send a response via CONNECTION.
STATE tracks whether another client resolves the request.  Quitting a
field prompt with `\\[keyboard-quit]' cancels the form."
  (let* ((form-id (alist-get 'id form))
         (answers
          (let ((opencode-session--active-prompt state))
            (condition-case nil
                (minibuffer-with-setup-hook
                    (lambda ()
                      (setq-local opencode-session--minibuffer-prompt state))
                  (opencode-session--form-answers form))
              (quit (unless (eq (opencode-session--prompt-state-status state)
                                'resolved)
                      :cancel))))))
    (unless connection
      (error "OpenCode session is not connected"))
    (unless form-id
      (error "OpenCode form request is missing ID"))
    (unless (eq (opencode-session--prompt-state-status state) 'resolved)
      (setf (opencode-session--prompt-state-status state) 'answered)
      (opencode-session--remove-prompt state)
      (if (eq answers :cancel)
          (opencode-client-form-cancel
           connection session-id form-id
           :success (lambda (&rest _args)
                      (message "OpenCode: form cancelled")
                      (opencode-session--mark-form-settled
                       session-id form-id 'cancelled))
           :error (lambda (&rest _args)
                    (message "OpenCode: failed to cancel form")))
        (opencode-client-form-reply
         connection session-id form-id answers
         :success (lambda (&rest _args)
                    (message "OpenCode: form answer sent")
                    (opencode-session--mark-form-settled
                     session-id form-id 'answered answers t))
         :error (lambda (&rest _args)
                  (message "OpenCode: failed to reply to form")))))))

(defun opencode-session--handle-form-created (data meta)
  "Handle the form.created SSE DATA arriving with META.
META carries `:connection', the connection on which the event arrived.
Registers the form as a pending prompt, renders it in the session
buffer when one is open, and schedules a minibuffer prompt via a timer
so `completing-read' does not block the process filter."
  (let* ((payload (opencode-session--payload data))
         (form (alist-get 'form payload))
         (session-id (alist-get 'sessionID form))
         (connection (plist-get meta :connection))
         (state (opencode-session--register-prompt
                 connection (alist-get 'id form))))
    (when (and state session-id)
      (opencode-session--in-session-buffer
       session-id
       (lambda ()
         (push (opencode-session--form-state-create
                :form-id (alist-get 'id form)
                :session-id session-id
                :form form
                :status 'pending)
               opencode-session--forms)
         (opencode-session--render-forms)))
      (run-at-time 0 nil
       (lambda ()
         (when (eq (opencode-session--prompt-state-status state) 'pending)
           (opencode-session--run-prompt
            state
            (lambda ()
              (opencode-session--prompt-form
               form session-id connection state)))))))))

(defun opencode-session--handle-form-replied (data meta)
  "Handle the form.replied SSE DATA arriving with META.
Drops the pending prompt (aborting the minibuffer when another client
answered) and settles the rendered form with the event's answer."
  (let* ((payload (opencode-session--payload data))
         (form-id (alist-get 'id payload))
         (session-id (alist-get 'sessionID payload))
         (answer (alist-get 'answer payload)))
    (opencode-session--handle-prompt-resolved data meta)
    (opencode-session--in-session-buffer
     session-id
     (lambda ()
       (when-let* ((form-state (opencode-session--find-form form-id)))
         (setf (opencode-session--form-state-status form-state) 'answered)
         (when answer
           (setf (opencode-session--form-state-answers form-state) answer))
         (opencode-session--render-forms))))))

(defun opencode-session--handle-form-cancelled (data meta)
  "Handle the form.cancelled SSE DATA arriving with META.
Drops the pending prompt and settles the rendered form as cancelled."
  (let* ((payload (opencode-session--payload data))
         (form-id (alist-get 'id payload))
         (session-id (alist-get 'sessionID payload)))
    (opencode-session--handle-prompt-resolved data meta)
    (opencode-session--in-session-buffer
     session-id
     (lambda ()
       (when-let* ((form-state (opencode-session--find-form form-id)))
         (setf (opencode-session--form-state-status form-state) 'cancelled)
         (opencode-session--render-forms))))))

(defun opencode-session--sync-forms (connection session-id)
  "Prompt for any pending forms in SESSION-ID via CONNECTION.
Fetches the server's pending form list, catching forms that were
created before this client connected (for example a form left pending
while no session buffer was open)."
  (when (and connection session-id)
    (opencode-client-form-list
     connection session-id
     :success (lambda (&rest args)
                (let ((forms (plist-get args :data)))
                  (dolist (form (opencode-session--normalize-items forms))
                    (opencode-session--handle-form-created
                     `((data . ((form . ,form))))
                     (list :connection connection)))))
     :error (lambda (&rest _args) nil))))

;;; File revert handling

(defun opencode-session--connection-directories ()
  "Return directories for active OpenCode connections."
  (let (directories)
    (maphash (lambda (_session-id buffer)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when-let* ((connection opencode-session--connection)
                              (directory (opencode-connection-directory connection)))
                     (push directory directories)))))
             opencode-session--buffers)
    (delete-dups (delq nil directories))))

(defun opencode-session--normalize-file-path (path)
  "Normalize PATH for buffer lookup.

Returns nil when PATH is not a string."
  (when (stringp path)
    (let* ((expanded (expand-file-name path))
           (directories (opencode-session--connection-directories)))
      (cond
       ((file-name-absolute-p expanded)
        (if (file-exists-p expanded)
            (file-truename expanded)
          expanded))
       (directories
        (let ((candidate (cl-find-if
                          #'file-exists-p
                          (mapcar (lambda (directory)
                                    (expand-file-name path directory))
                                  directories))))
          (if candidate
              (file-truename candidate)
            (expand-file-name path (car directories)))))
       (t expanded)))))

(defun opencode-session--event-file-paths (data)
  "Return a list of file paths from SSE DATA."
  (let* ((payload (opencode-session--payload data))
         (file (alist-get 'file payload))
         (path (alist-get 'path payload))
         (file-path (cond
                      ((stringp file) file)
                      ((listp file) (or (alist-get 'path file)
                                        (alist-get 'name file)))))
         (paths (or (alist-get 'paths payload)
                    (alist-get 'files payload))))
    (cond
     ((and paths (vectorp paths)) (append paths nil))
     ((listp paths) paths)
     ((stringp path) (list path))
     ((stringp file-path) (list file-path))
     (t nil))))

(defun opencode-session--maybe-revert-buffer (path)
  "Revert buffers visiting PATH when safe."
  (let ((normalized (opencode-session--normalize-file-path path)))
    (when normalized
      (dolist (buffer (buffer-list))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when-let* ((buffer-path (buffer-file-name buffer)))
              (let ((normalized-buffer (opencode-session--normalize-file-path buffer-path)))
                (when (and normalized-buffer
                           (string= normalized normalized-buffer))
                  (if (buffer-modified-p)
                      (message "OpenCode: buffer has unsaved changes (%s)" (buffer-name buffer))
                    (revert-buffer :ignore-auto :noconfirm)
                    (message "OpenCode: reloaded %s" (buffer-name buffer))))))))))))

(defun opencode-session--handle-file-updated (data)
  "Handle SSE file update DATA by reverting buffers."
  (dolist (path (opencode-session--event-file-paths data))
    (opencode-session--maybe-revert-buffer path)))

;;; Handler registrations

(opencode-sse-define-handler session-created "session.created" (_event data _meta)
  (opencode-session--handle-session-upsert data))

(opencode-sse-define-handler session-updated "session.updated" (_event data _meta)
  (opencode-session--handle-session-upsert data))

(opencode-sse-define-handler session-renamed "session.renamed" (_event data _meta)
  (opencode-session--handle-session-renamed data))

(opencode-sse-define-handler session-execution-started
    "session.execution.started" (_event data _meta)
  (opencode-session--handle-execution-started data))

(opencode-sse-define-handler session-execution-succeeded
    "session.execution.succeeded" (_event data _meta)
  (opencode-session--handle-execution-finished data))

(opencode-sse-define-handler session-execution-failed
    "session.execution.failed" (_event data _meta)
  (opencode-session--handle-execution-finished data))

(opencode-sse-define-handler session-idle "session.idle" (_event data _meta)
  (let* ((payload (opencode-session--payload data))
         (session-id (alist-get 'sessionID payload)))
    (opencode-session--update-status session-id (opencode-status-create :type "idle"))))

(opencode-sse-define-handler session-step-ended "session.step.ended" (_event data _meta)
  (opencode-session--handle-step-ended data))

(opencode-sse-define-handler session-step-failed "session.step.failed" (_event data _meta)
  (opencode-session--handle-step-failed data))

(opencode-sse-define-handler session-text-started "session.text.started" (_event data _meta)
  (opencode-session--handle-text-started data))

(opencode-sse-define-handler session-text-delta "session.text.delta" (_event data _meta)
  (opencode-session--handle-text-delta data))

(opencode-sse-define-handler session-text-ended "session.text.ended" (_event data _meta)
  (opencode-session--handle-text-ended data))

(opencode-sse-define-handler session-reasoning-started
    "session.reasoning.started" (_event data _meta)
  (opencode-session--handle-reasoning-started data))

(opencode-sse-define-handler session-reasoning-delta
    "session.reasoning.delta" (_event data _meta)
  (opencode-session--handle-reasoning-delta data))

(opencode-sse-define-handler session-reasoning-ended
    "session.reasoning.ended" (_event data _meta)
  (opencode-session--handle-reasoning-ended data))

(opencode-sse-define-handler session-tool-input-started
    "session.tool.input.started" (_event data meta)
  (opencode-session--handle-tool-input-started data meta))

(opencode-sse-define-handler session-tool-input-ended
    "session.tool.input.ended" (_event data _meta)
  (opencode-session--handle-tool-input-ended data))

(opencode-sse-define-handler session-tool-called "session.tool.called" (_event data _meta)
  (opencode-session--handle-tool-called data))

(opencode-sse-define-handler session-tool-progress "session.tool.progress" (_event data _meta)
  (opencode-session--handle-tool-progress data))

(opencode-sse-define-handler session-tool-success "session.tool.success" (_event data meta)
  (opencode-session--handle-tool-success data meta))

(opencode-sse-define-handler session-inbox-enqueued "session.inbox.enqueued" (_event data _meta)
  (opencode-session--handle-inbox-enqueued data))

(opencode-sse-define-handler session-agent-selected
    "session.agent.selected" (_event data _meta)
  (opencode-session--handle-agent-selected data))

(opencode-sse-define-handler session-model-selected
    "session.model.selected" (_event data _meta)
  (opencode-session--handle-model-selected data))

(opencode-sse-define-handler permission-asked "permission.asked" (_event data meta)
  (opencode-session--handle-permission-asked data meta))

(opencode-sse-define-handler permission-replied "permission.replied" (_event data meta)
  (opencode-session--handle-prompt-resolved data meta))

(opencode-sse-define-handler form-created "form.created" (_event data meta)
  (opencode-session--handle-form-created data meta))

(opencode-sse-define-handler form-replied "form.replied" (_event data meta)
  (opencode-session--handle-form-replied data meta))

(opencode-sse-define-handler form-cancelled "form.cancelled" (_event data meta)
  (opencode-session--handle-form-cancelled data meta))

(opencode-sse-define-handler file-edited "file.edited" (_event data _meta)
  (opencode-session--handle-file-updated data))

(opencode-sse-define-handler file-watcher-updated "file.watcher.updated" (_event data _meta)
  (opencode-session--handle-file-updated data))

;; TODO: surface additional bus events the server publishes on the
;; `/api/event' SSE stream:
;;   - `tui.toast.show' — used by the MCP auth flow and external
;;     `/tui/toast' callers; would be nice to show in the echo area
;;     with a face based on the variant (info/success/warning/error).
;;   - `installation.updated' — server announcing a new opencode
;;     release; the TUI shows a confirm dialog and runs the upgrade.

(provide 'emacs-opencode-session-handlers)

;;; emacs-opencode-session-handlers.el ends here