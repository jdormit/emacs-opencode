;;; emacs-opencode-acp-handlers.el --- ACP notification and request handlers  -*- lexical-binding: t; -*-

;; Maps ACP JSON-RPC notifications and agent-to-client requests to the
;; existing session rendering infrastructure.

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-opencode-acp)
(require 'emacs-opencode-session-vars)
(require 'emacs-opencode-message)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-session)

;; Forward declarations for session-mode functions
(declare-function opencode-session--upsert-message "emacs-opencode-session-mode")
(declare-function opencode-session--update-message-part "emacs-opencode-session-mode")
(declare-function opencode-session--find-message "emacs-opencode-session-mode")
(declare-function opencode-session--message-text "emacs-opencode-session-render")
(declare-function opencode-session--render-message "emacs-opencode-session-render")
(declare-function opencode-session--render-header "emacs-opencode-session-header")
(declare-function opencode-session--update-session "emacs-opencode-session-mode")
(declare-function opencode-session--update-status "emacs-opencode-session-mode")
(declare-function opencode-session--message-from-info "emacs-opencode-session-mode")
(declare-function opencode-session--message-part-from-info "emacs-opencode-session-mode")
(declare-function opencode-session--maybe-revert-buffer "emacs-opencode-session-handlers")
(declare-function opencode-session--permission-prompt-label "emacs-opencode-session-handlers")
(declare-function opencode-session--start-task-poll "emacs-opencode-session-mode")
(declare-function opencode-session--stop-task-poll "emacs-opencode-session-mode")

;;; Internal state for streaming message assembly

(defvar opencode-acp--seen-permission-ids (make-hash-table :test 'equal)
  "Set of permission tool-call IDs claimed by the ACP handler.
Written by the ACP `session/request_permission' handler when it receives
a permission request for a main-session tool call.  Read by the HTTP
permission poll to skip permissions the ACP is already handling.
Sub-agent permissions never appear here because ACP drops events for
unregistered sessions, so the poll handles them exclusively.
Keyed by tool call ID (ACP `toolCallId' = HTTP `tool.callID').")

(defvar-local opencode-acp--current-message nil
  "The current assistant message being assembled during prompt execution.
This is an `opencode-message' struct that accumulates parts as ACP
`session/update' notifications arrive.")

(defvar-local opencode-acp--current-text-part-id nil
  "Part ID of the current text part in the streaming message.")

(defvar-local opencode-acp--current-reasoning-part-id nil
  "Part ID of the current reasoning part in the streaming message.")

(defvar-local opencode-acp--part-counter 0
  "Counter for generating unique part IDs during streaming.")

;;; Helper: find session buffer from ACP notification params

(defun opencode-acp--session-buffer (session-id)
  "Return the live session buffer for SESSION-ID, or nil."
  (when-let ((buffer (opencode-session--buffer-for-session session-id)))
    (when (buffer-live-p buffer)
      buffer)))

;;; Helper: ensure a current streaming message exists

(defun opencode-acp--ensure-current-message (session-id)
  "Ensure there is a current assistant message for SESSION-ID.
Creates one if needed.  Must be called in the session buffer."
  (unless opencode-acp--current-message
    (let* ((msg-id (format "acp-msg-%s-%d" session-id
                           (cl-incf opencode-acp--part-counter)))
           (message (opencode-message-create
                     :id msg-id
                     :session-id session-id
                     :role "assistant")))
      (setq opencode-acp--current-message message)
      (setq opencode-acp--current-text-part-id nil)
      (setq opencode-acp--current-reasoning-part-id nil)
      (setq opencode-session--messages
            (append opencode-session--messages (list message)))))
  opencode-acp--current-message)

(defun opencode-acp--finalize-current-message ()
  "Finalize the current streaming message.
Called when the prompt response arrives."
  (setq opencode-acp--current-message nil)
  (setq opencode-acp--current-text-part-id nil)
  (setq opencode-acp--current-reasoning-part-id nil))

;;; session/update notification handler — the main demux point

(opencode-acp-define-notification-handler session-update
    "session/update" (_method params)
  (let* ((session-id (alist-get 'sessionId params))
         (update (alist-get 'update params))
         (variant (alist-get 'sessionUpdate update)))
    (when-let ((buffer (opencode-acp--session-buffer session-id)))
      (with-current-buffer buffer
        (pcase variant
          ("agent_message_chunk"
           (opencode-acp--handle-agent-message-chunk session-id update))
          ("agent_thought_chunk"
           (opencode-acp--handle-agent-thought-chunk session-id update))
          ("user_message_chunk"
           (opencode-acp--handle-user-message-chunk session-id update))
          ("tool_call"
           (opencode-acp--handle-tool-call session-id update))
          ("tool_call_update"
           (opencode-acp--handle-tool-call-update session-id update))
          ("usage_update"
           (opencode-acp--handle-usage-update session-id update))
          ("session_info_update"
           (opencode-acp--handle-session-info-update session-id update))
          ("available_commands_update"
           (opencode-acp--handle-commands-update update))
          ("current_mode_update"
           (opencode-acp--handle-mode-update update))
          ("plan"
           nil)  ; TODO: plan UI
          ("config_option_update"
           nil)  ; ignored for now
          (_
           (message "OpenCode ACP: unknown session update variant: %s" variant)))))))

;;; agent_message_chunk — streaming LLM text output

(defun opencode-acp--handle-agent-message-chunk (session-id update)
  "Handle an agent_message_chunk UPDATE for SESSION-ID."
  (let* ((content (alist-get 'content update))
         (text (alist-get 'text content))
         (message (opencode-acp--ensure-current-message session-id)))
    (when (and text (stringp text))
      ;; Find or create the text part
      (unless opencode-acp--current-text-part-id
        (let ((part-id (format "acp-text-%d" (cl-incf opencode-acp--part-counter))))
          (setq opencode-acp--current-text-part-id part-id)
          (let ((part (opencode-message-part-create
                       :id part-id
                       :session-id session-id
                       :message-id (opencode-message-id message)
                       :type "text"
                       :text "")))
            (setf (opencode-message-parts message)
                  (append (opencode-message-parts message)
                          (list (cons part-id part)))))))
      ;; Append delta text
      (when-let* ((entry (assoc opencode-acp--current-text-part-id
                                (opencode-message-parts message)))
                  (part (cdr entry))
                  ((opencode-message-part-p part)))
        (setf (opencode-message-part-text part)
              (concat (or (opencode-message-part-text part) "") text))
        (setf (opencode-message-text message)
              (opencode-session--message-text message))
        (opencode-session--render-message message)))))

;;; agent_thought_chunk — streaming reasoning tokens

(defun opencode-acp--handle-agent-thought-chunk (session-id update)
  "Handle an agent_thought_chunk UPDATE for SESSION-ID."
  (let* ((content (alist-get 'content update))
         (text (alist-get 'text content))
         (message (opencode-acp--ensure-current-message session-id)))
    (when (and text (stringp text))
      (unless opencode-acp--current-reasoning-part-id
        (let ((part-id (format "acp-reasoning-%d"
                               (cl-incf opencode-acp--part-counter))))
          (setq opencode-acp--current-reasoning-part-id part-id)
          (let ((part (opencode-message-part-create
                       :id part-id
                       :session-id session-id
                       :message-id (opencode-message-id message)
                       :type "reasoning"
                       :text "")))
            (setf (opencode-message-parts message)
                  (append (opencode-message-parts message)
                          (list (cons part-id part)))))))
      (when-let* ((entry (assoc opencode-acp--current-reasoning-part-id
                                (opencode-message-parts message)))
                  (part (cdr entry))
                  ((opencode-message-part-p part)))
        (setf (opencode-message-part-text part)
              (concat (or (opencode-message-part-text part) "") text))
        (setf (opencode-message-text message)
              (opencode-session--message-text message))
        (opencode-session--render-message message)))))

;;; user_message_chunk — replayed user messages (during loadSession)

(defun opencode-acp--handle-user-message-chunk (session-id update)
  "Handle a user_message_chunk UPDATE for SESSION-ID.
These arrive during session replay via `loadSession'."
  (let* ((content (alist-get 'content update))
         (text (alist-get 'text content)))
    (when (and text (stringp text))
      ;; Each user_message_chunk creates a simple user message
      (let* ((msg-id (format "acp-user-%d" (cl-incf opencode-acp--part-counter)))
             (part-id (format "acp-user-text-%d" opencode-acp--part-counter))
             (message (opencode-message-create
                       :id msg-id
                       :session-id session-id
                       :role "user"))
             (part (opencode-message-part-create
                    :id part-id
                    :session-id session-id
                    :message-id msg-id
                    :type "text"
                    :text text)))
        (setf (opencode-message-parts message) (list (cons part-id part)))
        (setf (opencode-message-text message) text)
        (setq opencode-session--messages
              (append opencode-session--messages (list message)))
        ;; Finalize any previous assistant message
        (opencode-acp--finalize-current-message)
        (opencode-session--render-message message)))))

;;; tool_call — new tool call created

(defun opencode-acp--handle-tool-call (session-id update)
  "Handle a tool_call UPDATE for SESSION-ID."
  (let* ((message (opencode-acp--ensure-current-message session-id))
         (call-id (alist-get 'toolCallId update))
         (tool-name (alist-get 'title update))
         (status (or (alist-get 'status update) "pending"))
         (raw-input (alist-get 'rawInput update))
         (locations (alist-get 'locations update))
         ;; Build a part info alist compatible with message-part-from-info
         (part-info `((id . ,call-id)
                      (sessionID . ,session-id)
                      (messageID . ,(opencode-message-id message))
                      (type . "tool")
                      (tool . ,tool-name)
                      (state . ((status . ,status)
                                (input . ,raw-input)))
                      ,@(when locations
                          `((metadata . ((locations . ,locations))))))))
    ;; Create the part and add it to the message
    (let ((part (opencode-session--message-part-from-info part-info))
          (entry (cons call-id nil)))
      (setcdr entry part)
      (setf (opencode-message-parts message)
            (append (opencode-message-parts message) (list entry)))
      ;; Reset the current text part so subsequent agent_message_chunk
      ;; deltas create a new text part AFTER this tool call, preserving
      ;; correct interleaving of text and tool parts.
      (setq opencode-acp--current-text-part-id nil)
      (setf (opencode-message-text message)
            (opencode-session--message-text message))
      (opencode-session--render-message message)
      ;; For task tool calls, start polling to discover the sub-agent
      ;; session ID and track its tool calls.
      (when (and (stringp tool-name) (string= tool-name "task")
                 opencode-session--connection)
        (opencode-session--start-task-poll
         opencode-session--connection session-id call-id)))))

;;; tool_call_update — tool call status/output change

(defun opencode-acp--handle-tool-call-update (session-id update)
  "Handle a tool_call_update UPDATE for SESSION-ID."
  (let* ((message (opencode-acp--ensure-current-message session-id))
         (call-id (alist-get 'toolCallId update))
         (status (alist-get 'status update))
         (tool-name (alist-get 'title update))
         (raw-input (alist-get 'rawInput update))
         (raw-output (alist-get 'rawOutput update))
         (content (alist-get 'content update)))
    (when-let* ((entry (assoc call-id (opencode-message-parts message)))
                (part (cdr entry))
                ((opencode-message-part-p part)))
      ;; Update tool identifier only if the part doesn't already have one.
      ;; The ACP "title" field carries the tool name (e.g., "bash") for
      ;; pending/in_progress states, but carries a human-readable
      ;; description (e.g., "emacs-opencode-acp.el") for completed state.
      ;; We preserve the tool identifier and store the descriptive title
      ;; separately in the state alist.
      (when (and tool-name (not (opencode-message-part-tool part)))
        (setf (opencode-message-part-tool part) tool-name))
      ;; Update state
      (let* ((old-state (opencode-message-part-state part))
             (new-state (copy-alist (or old-state '())))
             ;; Store the descriptive title if different from the tool id
             (existing-tool (opencode-message-part-tool part)))
        (when (and tool-name existing-tool
                   (not (string= tool-name existing-tool)))
          (setf (alist-get 'title new-state) tool-name))
        ;; Map ACP status to OpenCode state status
        (let ((oc-status (pcase status
                           ("in_progress" "running")
                           ("completed" "completed")
                           ("failed" "error")
                           (_ status))))
          (when oc-status
            (setf (alist-get 'status new-state) oc-status)))
        (when raw-input
          (setf (alist-get 'input new-state) raw-input))
        ;; Extract output text from content blocks
        (when content
          (let ((output-text
                 (mapconcat
                  (lambda (block)
                    (when (string= (alist-get 'type block) "content")
                      (alist-get 'text (alist-get 'content block))))
                  (if (vectorp content) (append content nil) content)
                  "")))
            (when (and output-text (not (string-empty-p output-text)))
              (setf (alist-get 'output new-state) output-text))))
        (when (and raw-output (alist-get 'error raw-output))
          (setf (alist-get 'error new-state) (alist-get 'error raw-output)))
        (when (and raw-output (alist-get 'metadata raw-output))
          (setf (alist-get 'metadata new-state) (alist-get 'metadata raw-output)))
        (setf (opencode-message-part-state part) new-state))
      ;; Re-render
      (setf (opencode-message-text message)
            (opencode-session--message-text message))
      (opencode-session--render-message message)
      ;; For completed task tool calls, register subagent if not yet done
      ;; and stop the task poll.
      (when (and (string= (or status "") "completed")
                 (string= (or (opencode-message-part-tool part) "") "task"))
        (when-let ((metadata (alist-get 'metadata
                                        (alist-get 'rawOutput update))))
          (when-let ((sub-session-id (alist-get 'sessionId metadata)))
            (unless (opencode-session--subagent-parent sub-session-id)
              (opencode-session--register-subagent
               sub-session-id session-id call-id))))
        ;; Do one final poll then stop
        (when opencode-session--connection
          (opencode-session--stop-task-poll call-id))))))

;;; usage_update — context window and cost info

(defun opencode-acp--handle-usage-update (_session-id update)
  "Handle a usage_update UPDATE."
  ;; Store usage in a buffer-local for the header to pick up
  (when-let ((session opencode-session--session))
    (let ((cost-info (alist-get 'cost update))
          (used (alist-get 'used update))
          (size (alist-get 'size update)))
      ;; Store as summary-like structure for header compatibility
      (let ((summary (or (opencode-session-summary session) '())))
        (when used
          (setf (alist-get 'tokensUsed summary) used))
        (when size
          (setf (alist-get 'tokensTotal summary) size))
        (when cost-info
          (setf (alist-get 'cost summary) (alist-get 'amount cost-info)))
        (setf (opencode-session-summary session) summary)))
    (opencode-session--render-header)))

;;; session_info_update — title/metadata changes

(defun opencode-acp--handle-session-info-update (_session-id update)
  "Handle a session_info_update UPDATE."
  (when-let ((session opencode-session--session))
    (when-let ((title (alist-get 'title update)))
      (setf (opencode-session-title session) title)
      ;; Rename buffer if needed
      (let ((new-name (format "*OpenCode: %s*"
                              (string-trim title))))
        (unless (string= (buffer-name) new-name)
          (rename-buffer new-name t))))
    (opencode-session--render-header)))

;;; available_commands_update — cache commands on connection

(defun opencode-acp--handle-commands-update (update)
  "Handle an available_commands_update UPDATE."
  (when opencode-session--connection
    (let ((commands (alist-get 'availableCommands update)))
      (setf (opencode-connection-commands opencode-session--connection)
            commands))))

;;; current_mode_update — agent mode changed

(defun opencode-acp--handle-mode-update (update)
  "Handle a current_mode_update UPDATE."
  (when-let ((mode-id (alist-get 'currentModeId update)))
    (setq-local opencode-session--agent mode-id)
    (opencode-session--render-header)))

;;; Agent-to-client request handlers

;; session/request_permission — prompt user for approval

(opencode-acp-define-request-handler permission-request
    "session/request_permission" (params)
  (let* ((options (alist-get 'options params))
         (tool-call (alist-get 'toolCall params))
         (tool-call-id (alist-get 'toolCallId tool-call)))
    ;; Claim this permission so the HTTP poll skips it.
    ;; The poll only handles sub-agent permissions that ACP drops;
    ;; for main-session permissions the ACP handler always fires.
    (when tool-call-id
      (puthash tool-call-id t opencode-acp--seen-permission-ids))
    (let* (;; Build a descriptive prompt from the tool call info
           (title (or (alist-get 'title tool-call) ""))
           (kind (or (alist-get 'kind tool-call) ""))
           (locations (alist-get 'locations tool-call))
           (location-path (when (and locations (listp locations) (car locations))
                            (alist-get 'path (car locations))))
           ;; Build prompt text
           (prompt (format "OpenCode: %s%s%s "
                           (if (string-empty-p kind) "permission" kind)
                           (if (string-empty-p title)
                               ""
                             (format " — %s" title))
                           (if location-path
                               (format " [%s]" location-path)
                             "")))
           ;; Build choices from the options the agent sent
           (choices (mapcar (lambda (opt)
                              (or (alist-get 'name opt)
                                  (alist-get 'optionId opt)
                                  "Unknown"))
                            (if (vectorp options) (append options nil) options)))
           (option-ids (mapcar (lambda (opt)
                                 (alist-get 'optionId opt))
                               (if (vectorp options) (append options nil) options)))
           (selection (condition-case nil
                          (completing-read prompt choices nil t)
                        (quit nil)))  ; nil signals user cancelled
           ;; Map the selected display name back to the optionId
           (selected-idx (when selection
                           (cl-position selection choices :test #'string=)))
           (selected-id (when selected-idx (nth selected-idx option-ids))))
      ;; Return the response in ACP RequestPermissionResponse format
      (if selected-id
          `((outcome . ((outcome . "selected")
                        (optionId . ,selected-id))))
        `((outcome . ((outcome . "cancelled"))))))))

;; fs/write_text_file — write file to disk, revert Emacs buffer

(opencode-acp-define-request-handler write-text-file
    "fs/write_text_file" (params)
  (let ((path (alist-get 'path params))
        (content (alist-get 'content params)))
    (when (and path content)
      ;; Write the file
      (with-temp-buffer
        (insert content)
        (write-region (point-min) (point-max) path nil 'quiet))
      ;; Revert any Emacs buffer visiting this file
      (require 'emacs-opencode-session-handlers)
      (opencode-session--maybe-revert-buffer path))
    ;; Return empty success result
    nil))

;; fs/read_text_file — read file from disk

(opencode-acp-define-request-handler read-text-file
    "fs/read_text_file" (params)
  (let ((path (alist-get 'path params)))
    (if (and path (file-exists-p path))
        `((content . ,(with-temp-buffer
                        (insert-file-contents path)
                        (buffer-string))))
      (error "File not found: %s" path))))

(provide 'emacs-opencode-acp-handlers)

;;; emacs-opencode-acp-handlers.el ends here
