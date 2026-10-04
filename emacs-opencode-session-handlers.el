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
(declare-function opencode-session--refresh-status "emacs-opencode-session-mode")
(declare-function opencode-session--set-title "emacs-opencode-session-mode")
(declare-function opencode-session--render-header "emacs-opencode-session-header")
(declare-function opencode-session--find-message "emacs-opencode-session-mode")
(declare-function opencode-session--add-message "emacs-opencode-session-mode")
(declare-function opencode-session--remove-messages "emacs-opencode-session-mode")
(declare-function opencode-session--move-message-to-end "emacs-opencode-session-mode")
(declare-function opencode-session--message-from-info "emacs-opencode-session-mode")
(declare-function opencode-session--tool-state "emacs-opencode-session-mode")
(declare-function opencode-session--carry-tool-output "emacs-opencode-session-mode")
(declare-function opencode-session--content-text "emacs-opencode-session-mode")
(declare-function opencode-session--error-message "emacs-opencode-session-mode")
(declare-function opencode-session--load-history "emacs-opencode-session-mode")
(declare-function opencode-session--inbox-message "emacs-opencode-session-mode")
(declare-function opencode-session--shell-state "emacs-opencode-session-mode")
(declare-function opencode-session--queue-event "emacs-opencode-session-mode")
(defvar opencode-session--replaying)
(declare-function opencode-session--fetch-info "emacs-opencode-session-mode")
(declare-function opencode-session--message-text "emacs-opencode-session-render")
(declare-function opencode-session--render-message "emacs-opencode-session-render")
(declare-function opencode-session--set-agent-from-server "emacs-opencode-session-model")
(declare-function opencode-session--set-model-from-server "emacs-opencode-session-model")
(declare-function opencode-session--store-agents "emacs-opencode-session-model")
(declare-function opencode-session--apply-default-agent "emacs-opencode-session-model")
(declare-function opencode-session--refresh-headers "emacs-opencode-session-model")

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

(defun opencode-session--resolve-prompt (connection request-id)
  "Mark REQUEST-ID on CONNECTION as answered by another client."
  (let* ((table (opencode-session--prompt-table connection))
         (state (and table (gethash request-id table))))
    (when state
      (setf (opencode-session--prompt-state-status state) 'resolved)
      (opencode-session--remove-prompt state)
      (when (opencode-session--prompt-active-p state)
        (message "OpenCode request answered in another client")
        (abort-recursive-edit)))))

(defun opencode-session--defer-prompt (state session-id connection function)
  "Run prompt FUNCTION for STATE from a timer.
FUNCTION runs in the buffer for SESSION-ID when there is one, else in any
buffer on CONNECTION, so `completing-read' never blocks the process
filter."
  (run-at-time
   0 nil
   (lambda ()
     (when (eq (opencode-session--prompt-state-status state) 'pending)
       (opencode-session--run-prompt
        state
        (lambda ()
          (let ((buffer (or (opencode-session--buffer-for-session session-id)
                            (opencode-session--any-live-session-buffer connection))))
            (if (and buffer (buffer-live-p buffer))
                (with-current-buffer buffer
                  (funcall function))
              (funcall function)))))))))

;;; Event plumbing

(defun opencode-session--event-data (event)
  "Return the payload of server EVENT."
  (alist-get 'data event))

(defun opencode-session--message-id-from-event (event)
  "Return the message ID the server derives from EVENT's ID."
  (concat "msg_" (string-remove-prefix "evt_" (alist-get 'id event))))

(defmacro opencode-session--with-session-buffer (session-id &rest body)
  "Run BODY in the live buffer for SESSION-ID, if there is one."
  (declare (indent 1))
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (opencode-session--buffer-for-session ,session-id)))
       (when (and ,buffer (buffer-live-p ,buffer))
         (with-current-buffer ,buffer
           ,@body)))))

(defun opencode-session--rerender (message)
  "Recompute MESSAGE's text and render it."
  (setf (opencode-message-text message) (opencode-session--message-text message))
  (opencode-session--render-message message))

(defun opencode-session--append-part (message part)
  "Append PART to MESSAGE."
  (setf (opencode-message-parts message)
        (append (opencode-message-parts message)
                (list (cons (opencode-message-part-id part) part)))))

(defun opencode-session--find-part-by-id (message part-id)
  "Return MESSAGE's part with PART-ID."
  (cdr (assoc part-id (opencode-message-parts message))))

(defun opencode-session--set-info (message key value)
  "Set KEY to VALUE in MESSAGE's server info alist."
  (let ((info (copy-alist (opencode-message-info message))))
    (setf (alist-get key info) value)
    (setf (opencode-message-info message) info)))

(defun opencode-session--status-from-retry (data)
  "Build a retry `opencode-status' from session.retry.scheduled DATA."
  (let ((attempt (alist-get 'attempt data))
        (text (opencode-session--error-message (alist-get 'error data)))
        (next (alist-get 'at data)))
    (opencode-status-create
     :type "retry"
     :attempt (and (numberp attempt) attempt)
     :message (and (stringp text) (not (string-empty-p text)) text)
     :next (and (numberp next) next))))

(defun opencode-session--interruption-error-p (error-info)
  "Return non-nil when ERROR-INFO records a user interruption."
  (let ((type (and (listp error-info) (alist-get 'type error-info))))
    (and (stringp type) (string-match-p "abort\\|interrupt\\|cancel" type))))

;;; Session lifecycle events

(defun opencode-session--handle-session-renamed (_event data &optional _meta)
  "Handle session.renamed event DATA."
  (let ((payload (opencode-session--event-data data)))
    (opencode-session--with-session-buffer (alist-get 'sessionID payload)
      (opencode-session--set-title (alist-get 'title payload)))))

(defun opencode-session--handle-agent-selected (_event data &optional _meta)
  "Handle session.agent.selected event DATA."
  (let ((payload (opencode-session--event-data data)))
    (opencode-session--with-session-buffer (alist-get 'sessionID payload)
      (opencode-session--set-agent-from-server (alist-get 'agent payload)))))

(defun opencode-session--handle-model-selected (_event data &optional _meta)
  "Handle session.model.selected event DATA."
  (let ((payload (opencode-session--event-data data)))
    (opencode-session--with-session-buffer (alist-get 'sessionID payload)
      (opencode-session--set-model-from-server (alist-get 'model payload)))))

(defun opencode-session--handle-usage-updated (_event data &optional _meta)
  "Handle session.usage.updated event DATA with the session's totals."
  (let ((payload (opencode-session--event-data data)))
    (opencode-session--with-session-buffer (alist-get 'sessionID payload)
      (when opencode-session--session
        (let ((info (copy-alist (opencode-session-info opencode-session--session))))
          (setf (alist-get 'cost info) (alist-get 'cost payload))
          (setf (alist-get 'tokens info) (alist-get 'tokens payload))
          (setf (opencode-session-info opencode-session--session) info))
        (opencode-session--render-header)))))

(defun opencode-session--handle-execution-started (_event data &optional _meta)
  "Handle session.execution.started event DATA."
  (opencode-session--update-status
   (alist-get 'sessionID (opencode-session--event-data data))
   (opencode-status-create :type "busy")))

(defun opencode-session--handle-execution-ended (event data &optional _meta)
  "Handle a session.execution EVENT that ends a run, with DATA."
  (let* ((payload (opencode-session--event-data data))
         (error-info (alist-get 'error payload)))
    (when (and (equal event "session.execution.failed")
               (not (opencode-session--interruption-error-p error-info)))
      (message "OpenCode: %s" (or (opencode-session--error-message error-info)
                                  "An error occurred")))
    (opencode-session--update-status
     (alist-get 'sessionID payload)
     (opencode-status-create :type "idle"))
    (opencode-session--with-session-buffer (alist-get 'sessionID payload)
      (when (and opencode-session--connection
                 (opencode-session--unsettled-tool-p))
        (opencode-session--load-history
         opencode-session--connection opencode-session--session (current-buffer))))))

(defun opencode-session--unsettled-tool-p ()
  "Return non-nil when a tool call in this buffer never reached a final state.
A run that ended with such a call missed events, so its transcript is stale."
  (cl-some (lambda (message)
             (cl-some (lambda (entry)
                        (and (equal (opencode-message-part-type (cdr entry)) "tool")
                             (member (alist-get 'status (opencode-message-part-state (cdr entry)))
                                     '("pending" "running"))))
                      (opencode-message-parts message)))
           opencode-session--messages))

(defun opencode-session--handle-retry-scheduled (_event data &optional _meta)
  "Handle session.retry.scheduled event DATA."
  (let ((payload (opencode-session--event-data data)))
    (opencode-session--update-status
     (alist-get 'sessionID payload)
     (opencode-session--status-from-retry payload))))

(defun opencode-session--clear-retry (session-id)
  "Return SESSION-ID to a plain busy status when it shows a retry."
  (opencode-session--with-session-buffer session-id
    (let ((status (and opencode-session--session
                       (opencode-session-status opencode-session--session))))
      (when (and (opencode-status-p status)
                 (equal (opencode-status-type status) "retry"))
        (opencode-session--update-status
         session-id (opencode-status-create :type "busy"))))))

;;; Inbox and transcript events

(defun opencode-session--handle-inbox-enqueued (_event data &optional _meta)
  "Handle session.inbox.enqueued event DATA by showing the queued prompt."
  (let* ((payload (opencode-session--event-data data))
         (item (alist-get 'item payload))
         (session-id (alist-get 'sessionID payload)))
    (opencode-session--with-session-buffer session-id
      (when-let* ((message (opencode-session--inbox-message
                            (append `((id . ,(alist-get 'inboxID payload))
                                      (time . ((created . ,(alist-get 'created data)))))
                                    item)
                            session-id)))
        (opencode-session--add-message message)))))

(defun opencode-session--handle-inbox-delivered (_event data &optional _meta)
  "Handle session.inbox.delivered event DATA."
  (let ((payload (opencode-session--event-data data)))
    (opencode-session--with-session-buffer (alist-get 'sessionID payload)
      (opencode-session--move-message-to-end (alist-get 'inboxID payload)))))

(defun opencode-session--handle-inbox-cancelled (_event data &optional _meta)
  "Handle session.inbox.cancelled event DATA."
  (let* ((payload (opencode-session--event-data data))
         (inbox-id (alist-get 'inboxID payload)))
    (opencode-session--with-session-buffer (alist-get 'sessionID payload)
      (opencode-session--remove-messages
       (lambda (message) (equal (opencode-message-id message) inbox-id))))))

(defun opencode-session--handle-revert-committed (_event data &optional _meta)
  "Handle session.revert.committed event DATA by dropping reverted messages."
  (let* ((payload (opencode-session--event-data data))
         (boundary (alist-get 'to payload)))
    (opencode-session--with-session-buffer (alist-get 'sessionID payload)
      (opencode-session--remove-messages
       (lambda (message)
         (not (string< (opencode-message-id message) boundary)))))))

(defun opencode-session--handle-synthetic (_event data &optional _meta)
  "Handle session.synthetic event DATA, which reports loaded instructions."
  (let* ((payload (opencode-session--event-data data))
         (session-id (alist-get 'sessionID payload)))
    (opencode-session--with-session-buffer session-id
      (when-let* ((message (opencode-session--message-from-info
                            `((id . ,(opencode-session--message-id-from-event data))
                              (type . "synthetic")
                              (description . ,(alist-get 'description payload))
                              (metadata . ,(alist-get 'metadata payload))
                              (time . ((created . ,(alist-get 'created data)))))
                            session-id)))
        (opencode-session--add-message message)))))

(defun opencode-session--handle-shell-started (_event data &optional _meta)
  "Handle session.shell.started event DATA for a user shell command."
  (let* ((payload (opencode-session--event-data data))
         (session-id (alist-get 'sessionID payload))
         (shell (alist-get 'shell payload)))
    (opencode-session--with-session-buffer session-id
      (opencode-session--add-message
       (opencode-session--message-from-info
        `((id . ,(opencode-session--message-id-from-event data))
          (type . "shell")
          (shellID . ,(alist-get 'id shell))
          (command . ,(alist-get 'command shell))
          (status . ,(alist-get 'status shell))
          (time . ((created . ,(alist-get 'created data)))))
        session-id)))))

(defun opencode-session--handle-shell-ended (_event data &optional _meta)
  "Handle session.shell.ended event DATA."
  (let* ((payload (opencode-session--event-data data))
         (shell (alist-get 'shell payload))
         (shell-id (alist-get 'id shell)))
    (opencode-session--with-session-buffer (alist-get 'sessionID payload)
      (when-let* ((message (cl-find-if
                            (lambda (message)
                              (equal (alist-get 'shellID (opencode-message-info message))
                                     shell-id))
                            opencode-session--messages))
                  (part (cdar (opencode-message-parts message))))
        (let ((output (alist-get 'output (alist-get 'output payload)))
              (previous (copy-opencode-message-part part)))
          (setf (opencode-message-part-state part)
                (opencode-session--shell-state
                 `((command . ,(alist-get 'command shell))
                   (status . ,(alist-get 'status shell))
                   (exit . ,(alist-get 'exit shell))
                   (output . ((output . ,output))))))
          (opencode-session--carry-tool-output previous part))
        (opencode-session--rerender message)))))

;;; Assistant step events

(defun opencode-session--handle-step-started (_event data &optional _meta)
  "Handle session.step.started event DATA by opening an assistant message."
  (let* ((payload (opencode-session--event-data data))
         (session-id (alist-get 'sessionID payload))
         (message-id (alist-get 'assistantMessageID payload)))
    (opencode-session--clear-retry session-id)
    (opencode-session--with-session-buffer session-id
      (let ((info `((id . ,message-id)
                    (type . "assistant")
                    (agent . ,(alist-get 'agent payload))
                    (model . ,(alist-get 'model payload))
                    (time . ((created . ,(alist-get 'started payload)))))))
        (if-let* ((existing (opencode-session--find-message message-id)))
            (progn
              (setf (opencode-message-error existing) nil)
              (opencode-session--set-info existing 'error nil)
              (opencode-session--rerender existing))
          (opencode-session--add-message
           (opencode-session--message-from-info info session-id)))))))

(defun opencode-session--handle-step-finished (_event data &optional _meta)
  "Handle session.step.ended or session.step.failed event DATA."
  (let ((payload (opencode-session--event-data data)))
    (opencode-session--with-session-buffer (alist-get 'sessionID payload)
      (when-let* ((message (opencode-session--find-message
                            (alist-get 'assistantMessageID payload))))
        (let ((error-info (alist-get 'error payload)))
          (setf (opencode-message-finish message)
                (or (alist-get 'finish payload) (and error-info "error")))
          (setf (opencode-message-time-completed message) (alist-get 'created data))
          (setf (opencode-message-error message) error-info)
          (opencode-session--set-info message 'error error-info)
          (when (alist-get 'tokens payload)
            (opencode-session--set-info message 'cost (alist-get 'cost payload))
            (opencode-session--set-info message 'tokens (alist-get 'tokens payload))))
        (opencode-session--rerender message)
        (opencode-session--render-header)))))

(defun opencode-session--with-assistant (payload function)
  "Call FUNCTION with the assistant message named in event PAYLOAD.
The message is re-rendered afterwards."
  (opencode-session--with-session-buffer (alist-get 'sessionID payload)
    (when-let* ((message (opencode-session--find-message
                          (alist-get 'assistantMessageID payload))))
      (funcall function message)
      (opencode-session--rerender message))))

(defun opencode-session--content-block (message type ordinal)
  "Return MESSAGE's content block of TYPE numbered ORDINAL.
Blocks of each type are numbered from zero in the order a step produces
them.  Without an ORDINAL, return the latest block of TYPE."
  (let ((blocks (cl-remove-if-not
                 (lambda (part) (equal (opencode-message-part-type part) type))
                 (mapcar #'cdr (opencode-message-parts message)))))
    (if (integerp ordinal)
        (nth ordinal blocks)
      (car (last blocks)))))

(defun opencode-session--handle-content-started (event data &optional _meta)
  "Handle a text or reasoning EVENT with DATA that starts a content block.
A block the message already has, such as one from a reloaded history, is
not added again."
  (let* ((payload (opencode-session--event-data data))
         (type (if (equal event "session.text.started") "text" "reasoning"))
         (ordinal (alist-get 'ordinal payload)))
    (opencode-session--with-assistant
     payload
     (lambda (message)
       (unless (and (integerp ordinal)
                    (opencode-session--content-block message type ordinal))
         (opencode-session--append-part
          message
          (opencode-message-part-create
           :id (format "%s:%d" (opencode-message-id message)
                       (length (opencode-message-parts message)))
           :session-id (alist-get 'sessionID payload)
           :message-id (opencode-message-id message)
           :type type
           :text "")))))))

(defun opencode-session--handle-content-text (event data &optional _meta)
  "Handle a text or reasoning delta or end EVENT with DATA."
  (let* ((payload (opencode-session--event-data data))
         (type (if (string-prefix-p "session.text." event) "text" "reasoning"))
         (delta (string-suffix-p ".delta" event)))
    (opencode-session--with-assistant
     payload
     (lambda (message)
       (when-let* ((part (opencode-session--content-block
                          message type (alist-get 'ordinal payload))))
         (setf (opencode-message-part-text part)
               (if delta
                   (concat (or (opencode-message-part-text part) "")
                           (alist-get 'delta payload))
                 (alist-get 'text payload))))))))

(defun opencode-session--track-tool-event (event payload)
  "Track the tool call in PAYLOAD for a subagent session without a buffer.
EVENT names the tool event."
  (let* ((session-id (alist-get 'sessionID payload))
         (parent-info (opencode-session--subagent-parent session-id)))
    (when (and parent-info
               (not (opencode-session--buffer-for-session session-id)))
      (let* ((call-id (alist-get 'id payload))
             (tracked (cl-find call-id (opencode-session--subagent-tools-for session-id)
                               :key (lambda (item) (alist-get 'part-id item))
                               :test #'equal))
             (tool (or (alist-get 'name payload) (alist-get 'tool tracked)))
             (state (alist-get 'state tracked)))
        (pcase event
          ("session.tool.input.started" (setq state '((status . "pending"))))
          ("session.tool.called"
           (setq state `((status . "running") (input . ,(alist-get 'input payload)))))
          ("session.tool.success"
           (setq state `((status . "completed") (input . ,(alist-get 'input state))
                         (metadata . ,(alist-get 'metadata payload)))))
          ("session.tool.failed"
           (setq state `((status . "error") (input . ,(alist-get 'input state))
                         (error . ,(opencode-session--error-message
                                    (alist-get 'error payload))))))
          (_ nil))
        (when tool
          (opencode-session--update-subagent-tool session-id call-id tool state)
          (opencode-session--with-session-buffer (car parent-info)
            (opencode-session--rerender-task-part parent-info)))))))

(defun opencode-session--handle-tool-event (event data &optional _meta)
  "Handle a session.tool EVENT with DATA."
  (let ((payload (opencode-session--event-data data)))
    (opencode-session--track-tool-event event payload)
    (opencode-session--with-assistant
     payload
     (lambda (message)
       (opencode-session--apply-tool-event message event payload)))
    (when (equal event "session.tool.success")
      (dolist (path (opencode-session--tool-event-file-paths payload))
        (opencode-session--maybe-revert-buffer path)))))

(defun opencode-session--apply-tool-event (message event payload)
  "Apply tool EVENT with PAYLOAD to MESSAGE's tool part."
  (let* ((call-id (alist-get 'id payload))
         (part (opencode-session--find-part-by-id message call-id))
         (state (and part (opencode-message-part-state part))))
    (if (equal event "session.tool.input.started")
        (unless part
          (opencode-session--append-part
           message
           (opencode-message-part-create
            :id call-id
            :session-id (alist-get 'sessionID payload)
            :message-id (opencode-message-id message)
            :type "tool"
            :tool (alist-get 'name payload)
            :state '((status . "pending")))))
      (when part
        (let ((previous (copy-opencode-message-part part))
              (status (alist-get 'status state)))
          (pcase event
            ("session.tool.called"
             (when (equal status "pending")
               (setf (opencode-message-part-state part)
                     `((status . "running") (input . ,(alist-get 'input payload))))))
            ("session.tool.progress"
             (when (equal status "running")
               (setf (opencode-message-part-state part)
                     `((status . "running") (input . ,(alist-get 'input state))
                       (metadata . ,(alist-get 'metadata payload))))))
            ((or "session.tool.success" "session.tool.failed")
             (when (member status '("pending" "running"))
               (setf (opencode-message-part-state part)
                     (append
                      (opencode-session--tool-state
                       `((status . ,(if (equal event "session.tool.success") "completed" "error"))
                         (input . ,(alist-get 'input state))
                         (metadata . ,(alist-get 'metadata payload))
                         (content . ,(alist-get 'content payload))
                         (error . ,(alist-get 'error payload)))))))))
          (opencode-session--carry-tool-output previous part)
          (opencode-session--maybe-register-subagent
           part (opencode-message-session-id message)))))))

;;; Compaction events

(defun opencode-session--compaction-message (session-id message-id reason)
  "Return a running compaction message MESSAGE-ID in SESSION-ID for REASON."
  (opencode-session--message-from-info
   `((id . ,message-id)
     (type . "compaction")
     (status . "running")
     (reason . ,reason))
   session-id))

(defun opencode-session--handle-compaction-started (_event data &optional _meta)
  "Handle session.compaction.started event DATA."
  (let* ((payload (opencode-session--event-data data))
         (session-id (alist-get 'sessionID payload)))
    (opencode-session--with-session-buffer session-id
      (opencode-session--add-message
       (opencode-session--compaction-message
        session-id
        (or (alist-get 'inputID payload)
            (opencode-session--message-id-from-event data))
        (alist-get 'reason payload))))))

(defun opencode-session--handle-compaction-finished (event data &optional _meta)
  "Handle the compaction EVENT with DATA that ends a compaction."
  (let* ((payload (opencode-session--event-data data))
         (session-id (alist-get 'sessionID payload))
         (status (if (equal event "session.compaction.failed") "failed" "completed")))
    (opencode-session--with-session-buffer session-id
      (let ((running (cl-find-if
                      (lambda (message)
                        (and (equal (opencode-message-role message) "compaction")
                             (equal (alist-get 'status (opencode-message-info message))
                                    "running")))
                      (reverse opencode-session--messages))))
        ;; A replayed end without a running compaction is already in the
        ;; loaded history.
        (unless (and opencode-session--replaying (null running))
          (let ((message (or running
                             (opencode-session--add-message
                              (opencode-session--compaction-message
                               session-id
                               (opencode-session--message-id-from-event data)
                               (alist-get 'reason payload)))))
                (error-text (opencode-session--error-message (alist-get 'error payload))))
            (opencode-session--set-info message 'status status)
            (let ((part (cdar (opencode-message-parts message))))
              (setf (opencode-message-part-metadata part)
                    `((status . ,status) (error . ,error-text))))
            (opencode-session--rerender message)))))))

;;; Subagent tracking

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
  "Register a subagent mapping when PART started a child session.
PARENT-SESSION-ID is the session that owns the tool part."
  (when (and (opencode-message-part-p part)
             (member (opencode-message-part-tool part) '("subagent" "task")))
    (let* ((metadata (alist-get 'metadata (opencode-message-part-state part)))
           (subagent-session-id (or (alist-get 'sessionID metadata)
                                    (alist-get 'sessionId metadata))))
      (when subagent-session-id
        (opencode-session--register-subagent
         subagent-session-id parent-session-id (opencode-message-part-id part))))))

;;; Permission handling

(defun opencode-session--permission-resources (permission)
  "Return the resource strings PERMISSION applies to."
  (opencode-session--normalize-items (alist-get 'resources permission)))

(defun opencode-session--permission-detail (permission)
  "Return a detail string for PERMISSION when available."
  (let* ((action (alist-get 'action permission))
         (metadata (alist-get 'metadata permission))
         (resources (opencode-session--permission-resources permission))
         (resource-text (and resources (string-join resources ", "))))
    (cond
     ((and (member action '("shell" "bash")) resource-text)
      (format "run %s" resource-text))
     ((and (member action '("subagent" "task")) resource-text)
      (format "start subagent %s" resource-text))
     ((and (equal action "webfetch") resource-text)
      (format "fetch %s" resource-text))
     ((and (equal action "websearch") resource-text)
      (format "search the web for %s" resource-text))
     ((and (equal action "external_directory") resource-text)
      (format "access external directory %s" resource-text))
     ((and (equal action "skill") resource-text)
      (format "use skill %s" resource-text))
     ((and action resource-text)
      (format "%s %s" action resource-text))
     ((and action (alist-get 'path metadata))
      (format "%s %s" action (alist-get 'path metadata)))
     (t nil))))

(defun opencode-session--permission-prompt-label (permission)
  "Return the minibuffer prompt label for PERMISSION."
  (let* ((action (alist-get 'action permission))
         (detail (opencode-session--permission-detail permission))
         (fallback (if action (format "use %s" action) "proceed")))
    (format "OpenCode wants to %s: " (or detail fallback))))

(defun opencode-session--prompt-permission (permission connection state)
  "Prompt for PERMISSION and send a response via CONNECTION.
STATE tracks whether another client resolves the request."
  (let* ((request-id (alist-get 'id permission))
         (session-id (alist-get 'sessionID permission))
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

(defun opencode-session--handle-permission-asked (_event data meta)
  "Handle the permission.asked event DATA arriving with META.
META is a plist carrying `:connection', the connection on which
the event arrived; the reply is always routed back to that
connection so multi-server setups respond to the correct server.

Falls back to any live session buffer on the same connection when
the session has no buffer, e.g. for subagent sessions."
  (let* ((permission (opencode-session--event-data data))
         (connection (plist-get meta :connection))
         (state (opencode-session--register-prompt
                 connection (alist-get 'id permission))))
    (when state
      (opencode-session--defer-prompt
       state (alist-get 'sessionID permission) connection
       (lambda ()
         (opencode-session--prompt-permission permission connection state))))))

(defun opencode-session--handle-permission-replied (_event data meta)
  "Handle permission.replied event DATA arriving with META."
  (opencode-session--resolve-prompt
   (plist-get meta :connection)
   (alist-get 'requestID (opencode-session--event-data data))))

;;; Form handling

(defconst opencode-session--other-label "Other"
  "Completion label for typing a custom answer to a form field.")

(defun opencode-session--form-field-label (field)
  "Return the minibuffer prompt label for form FIELD."
  (let ((title (alist-get 'title field))
        (description (alist-get 'description field)))
    (cond
     ((and title description) (format "OpenCode %s: %s " title description))
     (title (format "OpenCode %s: " title))
     (description (format "OpenCode: %s " description))
     (t (format "OpenCode %s: " (alist-get 'key field))))))

(defun opencode-session--form-field-active-p (field answer)
  "Return non-nil when FIELD's `when' conditions hold for ANSWER so far."
  (cl-every
   (lambda (condition)
     (let* ((key (intern (alist-get 'key condition)))
            (cell (assq key answer))
            (value (cdr cell))
            (expected (alist-get 'value condition))
            (value (if (eq value :json-false) nil value))
            (matches (if (vectorp value)
                         (and (member expected (append value nil)) t)
                       (equal value expected))))
       (and cell
            (if (equal (alist-get 'op condition) "neq") (not matches) matches))))
   (opencode-session--normalize-items (alist-get 'when field))))

(defun opencode-session--form-option-labels (field)
  "Return the option labels of FIELD, with a custom entry when allowed."
  (let ((labels (mapcar (lambda (option) (alist-get 'label option))
                        (opencode-session--normalize-items (alist-get 'options field)))))
    (if (alist-get 'custom field)
        (append labels (list opencode-session--other-label))
      labels)))

(defun opencode-session--form-option-value (field label)
  "Return the value of FIELD's option shown as LABEL."
  (or (alist-get 'value (cl-find label (opencode-session--normalize-items
                                       (alist-get 'options field))
                                 :key (lambda (option) (alist-get 'label option))
                                 :test #'equal))
      label))

(defun opencode-session--read-form-field (field)
  "Read a value for form FIELD in the minibuffer.
Returns the value, or nil to leave an optional field unanswered."
  (let ((prompt (opencode-session--form-field-label field))
        (default (alist-get 'default field)))
    (pcase (alist-get 'type field)
      ("string"
       (if (alist-get 'options field)
           (let ((selection (completing-read prompt (opencode-session--form-option-labels field)
                                             nil t nil nil default)))
             (if (equal selection opencode-session--other-label)
                 (read-string (concat prompt "(Other): "))
               (opencode-session--form-option-value field selection)))
         (let ((value (read-string prompt nil nil default)))
           (unless (and (string-empty-p value) (not (alist-get 'required field)))
             value))))
      ((or "number" "integer")
       (read-number prompt default))
      ("boolean"
       (if (y-or-n-p prompt) t :json-false))
      ("multiselect"
       (let* ((selection (completing-read-multiple
                          prompt (opencode-session--form-option-labels field) nil t))
              (values (mapcar (lambda (label) (opencode-session--form-option-value field label))
                              (remove opencode-session--other-label selection))))
         (when (member opencode-session--other-label selection)
           (setq values (append values (list (read-string (concat prompt "(Other): "))))))
         (vconcat values)))
      ("external"
       (when-let* ((url (alist-get 'url field)))
         (browse-url url)
         (read-string (format "%sPress RET when finished in the browser " prompt))
         nil)))))

(defun opencode-session--form-field-default (field)
  "Return FIELD's default answer, or nil when it has none.
A false boolean default is kept as `:json-false' so it is still sent."
  (let ((cell (assq 'default field)))
    (cond
     ((null cell) nil)
     ((and (null (cdr cell)) (equal (alist-get 'type field) "boolean")) :json-false)
     (t (cdr cell)))))

(defun opencode-session--read-form-answer (fields)
  "Read answers to form FIELDS and return them as an alist.
Hidden fields keep their defaults, and fields whose conditions do not hold
are skipped."
  (let (answer)
    (dolist (field (opencode-session--normalize-items fields))
      (let ((key (intern (alist-get 'key field))))
        (when (opencode-session--form-field-active-p field answer)
          (let ((value (if (alist-get 'hidden field)
                           (opencode-session--form-field-default field)
                         (opencode-session--read-form-field field))))
            (when value
              (setq answer (append answer (list (cons key value)))))))))
    answer))

(defun opencode-session--prompt-form (form connection state)
  "Prompt for FORM and send the answer via CONNECTION.
STATE tracks whether another client resolves the request.  Quitting the
minibuffer dismisses the form."
  (let* ((form-id (alist-get 'id form))
         (session-id (alist-get 'sessionID form))
         (answer
          (let ((opencode-session--active-prompt state))
            (condition-case nil
                (minibuffer-with-setup-hook
                    (lambda ()
                      (setq-local opencode-session--minibuffer-prompt state))
                  (opencode-session--read-form-answer (alist-get 'fields form)))
              (quit (unless (eq (opencode-session--prompt-state-status state)
                                'resolved)
                      :cancel))))))
    (unless connection
      (error "OpenCode session is not connected"))
    (unless form-id
      (error "OpenCode form is missing ID"))
    (unless (eq (opencode-session--prompt-state-status state) 'resolved)
      (setf (opencode-session--prompt-state-status state) 'answered)
      (opencode-session--remove-prompt state)
      (if (eq answer :cancel)
          (opencode-client-form-cancel
           connection session-id form-id
           :success (lambda (&rest _args)
                      (message "OpenCode question dismissed"))
           :error (lambda (&rest _args)
                    (message "OpenCode: failed to dismiss question")))
        (opencode-client-form-reply
         connection session-id form-id answer
         :success (lambda (&rest _args)
                    (message "OpenCode question reply sent"))
         :error (lambda (&rest _args)
                  (message "OpenCode: failed to reply to question")))))))

(defun opencode-session--handle-form-created (_event data meta)
  "Handle the form.created event DATA arriving with META.
Forms carry agent questions and MCP requests.  The answer is routed back
to the connection that asked."
  (let* ((form (alist-get 'form (opencode-session--event-data data)))
         (connection (plist-get meta :connection))
         (state (opencode-session--register-prompt connection (alist-get 'id form))))
    (when state
      (opencode-session--defer-prompt
       state (alist-get 'sessionID form) connection
       (lambda ()
         (opencode-session--prompt-form form connection state))))))

(defun opencode-session--handle-form-resolved (_event data meta)
  "Handle a form.replied or form.cancelled event DATA arriving with META."
  (opencode-session--resolve-prompt
   (plist-get meta :connection)
   (alist-get 'id (opencode-session--event-data data))))

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
       ((file-name-absolute-p path)
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

(defun opencode-session--tool-event-file-paths (payload)
  "Return the files a completed tool event PAYLOAD changed."
  (delq nil (mapcar (lambda (file) (alist-get 'file file))
                    (opencode-session--normalize-items
                     (alist-get 'files (alist-get 'metadata payload))))))

(defun opencode-session--event-file-paths (data)
  "Return a list of file paths from filesystem event DATA."
  (let ((file (alist-get 'file (opencode-session--event-data data))))
    (when (stringp file)
      (list file))))

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
                           (string= normalized normalized-buffer)
                           (not (verify-visited-file-modtime buffer)))
                  (if (buffer-modified-p)
                      (message "OpenCode: buffer has unsaved changes (%s)" (buffer-name buffer))
                    (revert-buffer :ignore-auto :noconfirm)
                    (message "OpenCode: reloaded %s" (buffer-name buffer))))))))))))

(defun opencode-session--handle-file-updated (_event data &optional _meta)
  "Handle filesystem event DATA by reverting buffers."
  (unless (equal (alist-get 'event (opencode-session--event-data data)) "unlink")
    (dolist (path (opencode-session--event-file-paths data))
      (opencode-session--maybe-revert-buffer path))))

;;; Catalog and connection events

(defun opencode-session--buffers-on-connection (connection)
  "Return live session buffers that use CONNECTION."
  (let (buffers)
    (maphash (lambda (_session-id buffer)
               (when (and (buffer-live-p buffer)
                          (eq (buffer-local-value 'opencode-session--connection buffer)
                              connection))
                 (push buffer buffers)))
             opencode-session--buffers)
    buffers))

(defun opencode-session--handle-server-connected (_event _data meta)
  "Resync with the server when the event stream connects, per META.
Events sent while no stream was open are lost, so reload the catalogs and
pending prompts, and after a reconnect each session's transcript and busy
state."
  (let ((connection (plist-get meta :connection)))
    (when (opencode-connection-p connection)
      ;; A server can answer catalog requests before it has loaded its
      ;; providers, and the change events sent before this stream opened
      ;; are lost, so reload the catalogs on every connect.
      (opencode-connection-invalidate-models connection)
      (opencode-connection-invalidate-commands connection)
      (opencode-session--refresh-headers connection)
      (when (plist-get meta :reconnect)
        (dolist (buffer (opencode-session--buffers-on-connection connection))
          (with-current-buffer buffer
            (when opencode-session--session
              (opencode-session--resync-session connection buffer))))
        (opencode-session--refresh-status connection))
      (opencode-session--prompt-pending-requests connection))))

(defun opencode-session--prompt-pending-requests (connection)
  "Prompt for permissions and forms already waiting on CONNECTION's server.
They are announced only once, so a client that connects later, or missed
the announcement while disconnected, asks the server for them."
  (let ((meta (list :connection connection)))
    (opencode-client-pending-permissions
     connection
     :success (lambda (&rest args)
                (dolist (permission (plist-get args :data))
                  (opencode-session--handle-permission-asked
                   "permission.asked" `((data . ,permission)) meta)))
     :error #'ignore)
    (opencode-client-pending-forms
     connection
     :success (lambda (&rest args)
                (dolist (form (plist-get args :data))
                  (opencode-session--handle-form-created
                   "form.created" `((data . ((form . ,form)))) meta)))
     :error #'ignore)))

(defun opencode-session--resync-session (connection buffer)
  "Reload the session info and transcript shown in BUFFER via CONNECTION."
  (opencode-session--fetch-info connection buffer)
  (opencode-session--load-history
   connection (buffer-local-value 'opencode-session--session buffer) buffer))

(defun opencode-session--handle-models-changed (_event _data meta)
  "Refresh the model catalog of META's connection after providers change."
  (when-let* ((connection (plist-get meta :connection)))
    (when (opencode-connection-p connection)
      (opencode-connection-invalidate-models connection)
      (opencode-session--refresh-headers connection))))

(defun opencode-session--handle-agents-changed (_event _data meta)
  "Refresh the agent list of META's connection."
  (when-let* ((connection (plist-get meta :connection)))
    (when (opencode-connection-p connection)
      (opencode-client-agents
       connection
       :success (lambda (&rest args)
                  (opencode-session--store-agents connection (plist-get args :data))
                  (dolist (buffer (opencode-session--buffers-on-connection connection))
                    (with-current-buffer buffer
                      (opencode-session--apply-default-agent connection))))
       :error #'ignore))))

(defun opencode-session--handle-commands-changed (_event _data meta)
  "Drop the cached command list of META's connection."
  (when-let* ((connection (plist-get meta :connection)))
    (when (opencode-connection-p connection)
      (opencode-connection-invalidate-commands connection))))

;;; Handler registrations

(defconst opencode-session--event-handlers
  '((opencode-session--handle-server-connected "server.connected")
    (opencode-session--handle-session-renamed "session.renamed")
    (opencode-session--handle-agent-selected "session.agent.selected")
    (opencode-session--handle-model-selected "session.model.selected")
    (opencode-session--handle-usage-updated "session.usage.updated")
    (opencode-session--handle-execution-started "session.execution.started")
    (opencode-session--handle-execution-ended "session.execution.succeeded"
                                              "session.execution.failed"
                                              "session.execution.interrupted")
    (opencode-session--handle-retry-scheduled "session.retry.scheduled")
    (opencode-session--handle-inbox-enqueued "session.inbox.enqueued")
    (opencode-session--handle-inbox-delivered "session.inbox.delivered")
    (opencode-session--handle-inbox-cancelled "session.inbox.cancelled")
    (opencode-session--handle-revert-committed "session.revert.committed")
    (opencode-session--handle-synthetic "session.synthetic")
    (opencode-session--handle-shell-started "session.shell.started")
    (opencode-session--handle-shell-ended "session.shell.ended")
    (opencode-session--handle-step-started "session.step.started")
    (opencode-session--handle-step-finished "session.step.ended" "session.step.failed")
    (opencode-session--handle-content-started "session.text.started"
                                              "session.reasoning.started")
    (opencode-session--handle-content-text "session.text.delta" "session.text.ended"
                                           "session.reasoning.delta"
                                           "session.reasoning.ended")
    (opencode-session--handle-tool-event "session.tool.input.started"
                                         "session.tool.called"
                                         "session.tool.progress"
                                         "session.tool.success"
                                         "session.tool.failed")
    (opencode-session--handle-compaction-started "session.compaction.started")
    (opencode-session--handle-compaction-finished "session.compaction.ended"
                                                  "session.compaction.failed")
    (opencode-session--handle-permission-asked "permission.asked")
    (opencode-session--handle-permission-replied "permission.replied")
    (opencode-session--handle-form-created "form.created")
    (opencode-session--handle-form-resolved "form.replied" "form.cancelled")
    (opencode-session--handle-file-updated "filesystem.changed")
    (opencode-session--handle-models-changed "model.updated" "provider.updated"
                                             "integration.updated")
    (opencode-session--handle-agents-changed "agent.updated")
    (opencode-session--handle-commands-changed "command.updated"))
  "Session event handlers and the event types each one handles.
Every handler is called with the event type, the event, and a META plist.")

(defun opencode-session--dispatch-event (event data meta)
  "Run the session handler for EVENT with DATA and META.
Session events for a buffer whose history is loading are held and
replayed once the history arrives."
  (when-let* ((handler (car (cl-find-if (lambda (entry) (member event (cdr entry)))
                                        opencode-session--event-handlers))))
    (let* ((session-id (and (string-prefix-p "session." event)
                            (alist-get 'sessionID (opencode-session--event-data data))))
           (buffer (and session-id (opencode-session--buffer-for-session session-id))))
      (unless (and buffer
                   (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (opencode-session--queue-event handler event data meta)))
        (funcall handler event data meta)))))

(dolist (entry opencode-session--event-handlers)
  (dolist (event (cdr entry))
    (opencode-sse-register-handler event #'opencode-session--dispatch-event)))

(provide 'emacs-opencode-session-handlers)

;;; emacs-opencode-session-handlers.el ends here
