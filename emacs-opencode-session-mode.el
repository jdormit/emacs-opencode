;;; emacs-opencode-session-mode.el --- OpenCode session buffer  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-message)
(require 'emacs-opencode-session)
(require 'emacs-opencode-sse)
(require 'emacs-opencode-client)

(defgroup emacs-opencode nil
  "Emacs client for the OpenCode server."
  :group 'applications)

(defface opencode-session-user-face
  '((t :inherit default))
  "Face used for user messages."
  :group 'emacs-opencode)

(defface opencode-session-assistant-face
  '((t :inherit default))
  "Face used for assistant messages."
  :group 'emacs-opencode)

(defface opencode-session-header-face
  '((t :inherit default :weight bold))
  "Face used for session header text."
  :group 'emacs-opencode)

(defface opencode-session-status-face
  '((t :inherit shadow))
  "Face used for session status text."
  :group 'emacs-opencode)

(defface opencode-session-tool-face
  '((t :inherit shadow))
  "Face used for tool call lines."
  :group 'emacs-opencode)

(defvar opencode-session--buffers (make-hash-table :test 'equal)
  "Registry mapping session IDs to buffers.")

(defvar opencode-session-send-input-hook nil
  "Hook run when input is submitted.

Each function receives SESSION and INPUT as arguments.")

(defvar-local opencode-session--session nil
  "Session object for the current buffer.")

(defvar-local opencode-session--messages nil
  "List of message objects for the current buffer.")

(defvar-local opencode-session--connection nil
  "Connection used for the current session buffer.")

(defvar-local opencode-session--input-start-marker nil
  "Marker indicating the start of the input region.")

(defvar-local opencode-session--input-marker nil
  "Marker indicating the end of the input region.")

(defvar-local opencode-session--agent nil
  "Selected agent name for the current session buffer.")

(defvar-local opencode-session--agent-index nil
  "Index of the selected agent in the available agents list.")

(defvar opencode-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'opencode-session-send-input)
    (define-key map (kbd "C-c C-a") #'opencode-session-select-agent)
    (define-key map (kbd "C-c C-n") #'opencode-session-next-agent)
    (define-key map (kbd "C-c C-p") #'opencode-session-previous-agent)
    (define-key map (kbd "C-c C-r") #'opencode-session-refresh-agents)
    (define-key map (kbd "S-TAB") #'opencode-session-previous-agent)
    (define-key map (kbd "<backtab>") #'opencode-session-previous-agent)
    (define-key map (kbd "RET") #'newline)
    (define-key map [remap self-insert-command] #'opencode-session-self-insert)
    (define-key map [remap yank] #'opencode-session-yank)
    (define-key map [remap delete-backward-char] #'opencode-session-delete-backward)
    (define-key map [remap backward-delete-char-untabify] #'opencode-session-delete-backward)
    map)
  "Keymap for `opencode-session-mode`.")

(define-derived-mode opencode-session-mode text-mode "OpenCode-Session"
  "Major mode for OpenCode session buffers."
  (use-local-map opencode-session-mode-map)
  (when (and (bound-and-true-p evil-mode)
             (fboundp 'evil-define-key))
    (evil-define-key '(normal insert) (current-local-map)
      (kbd "TAB") #'opencode-session-next-agent
      (kbd "S-TAB") #'opencode-session-previous-agent
      (kbd "<backtab>") #'opencode-session-previous-agent))
  (setq-local buffer-read-only nil)
  (setq-local opencode-session--messages nil)
  (setq-local opencode-session--agent nil)
  (setq-local opencode-session--agent-index nil)
  (opencode-session--ensure-markers))

(defun opencode-session-open (session &optional connection)
  "Open a session buffer for SESSION and return it.

When CONNECTION is provided, load existing session messages." 
  (let* ((name (opencode-session--buffer-name session))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (opencode-session-mode)
      (setq-local opencode-session--session session)
      (setq-local opencode-session--connection connection)
      (opencode-session--register-buffer session buffer)
      (opencode-session--render-buffer)
      (when connection
        (opencode-session--ensure-agents connection)))
    (when (and connection (opencode-session-id session))
      (opencode-session--load-history connection session buffer))
    (pop-to-buffer buffer)
    buffer))

(defun opencode-session-send-input ()
  "Send the current input region content."
  (interactive)
  (let ((input (opencode-session--current-input)))
    (if (string-empty-p (string-trim input))
        (message "OpenCode input is empty")
      (unless (and opencode-session--connection opencode-session--session)
        (error "OpenCode session is not connected"))
      (opencode-session--send-input opencode-session--connection
                                    opencode-session--session
                                    input)
      (opencode-session--clear-input)
      (message "OpenCode message submitted"))))

(defun opencode-session-self-insert (n)
  "Insert N characters into the session input area."
  (interactive "p")
  (opencode-session--goto-input)
  (self-insert-command n))

(defun opencode-session-yank (arg)
  "Yank ARG into the session input area."
  (interactive "P")
  (opencode-session--goto-input)
  (yank arg))

(defun opencode-session-delete-backward (arg)
  "Delete ARG characters backward inside the input area."
  (interactive "p")
  (opencode-session--goto-input)
  (backward-delete-char-untabify arg))

(defun opencode-session--ensure-markers ()
  "Ensure input markers exist."
  (unless opencode-session--input-start-marker
    (setq-local opencode-session--input-start-marker (copy-marker (point-max))))
  (unless opencode-session--input-marker
    (setq-local opencode-session--input-marker (copy-marker (point-max) t))))

(defun opencode-session--buffer-name (session)
  "Return a buffer name for SESSION."
  (let ((title (string-trim (or (opencode-session-title session) ""))))
    (format "*opencode-session<%s>*"
            (if (string-empty-p title)
                (or (opencode-session-slug session)
                    (opencode-session-id session)
                    "session")
              title))))

(defun opencode-session--register-buffer (session buffer)
  "Register BUFFER for SESSION."
  (when-let ((session-id (opencode-session-id session)))
    (puthash session-id buffer opencode-session--buffers)))

(defun opencode-session--buffer-for-session (session-id)
  "Return the session buffer for SESSION-ID, if any."
  (gethash session-id opencode-session--buffers))

(defun opencode-session--render-buffer ()
  "Render the session buffer contents."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (opencode-session--ensure-markers)
  (opencode-session--render-header)
  (opencode-session--render-messages)
  (opencode-session--ensure-input-region))

(defun opencode-session--render-header ()
  "Render the header line for the session."
  (let* ((title (or (opencode-session-title opencode-session--session)
                    "OpenCode Session"))
         (status (or (opencode-session-status opencode-session--session)
                     "idle"))
         (agent opencode-session--agent)
         (agent-label (when (and agent (not (string-empty-p agent)))
                        (format "[%s]" agent))))
    (setq header-line-format
          (concat (propertize title 'face 'opencode-session-header-face)
                  (when agent-label
                    (concat " "
                            (propertize agent-label 'face 'opencode-session-status-face)))
                  " "
                  (propertize (format "[%s]" status)
                              'face 'opencode-session-status-face)))))

(defun opencode-session--render-messages ()
  "Render all messages for the session."
  (dolist (message opencode-session--messages)
    (opencode-session--render-message message)))

(defun opencode-session--render-message (message)
  "Render MESSAGE into the buffer."
  (let ((text (opencode-session--message-text message)))
    (opencode-session--replace-message message text nil)))

(defun opencode-session--replace-message (message text face)
  "Replace MESSAGE region with TEXT using FACE."
  (let ((start (opencode-message-start-marker message))
        (end (opencode-message-end-marker message)))
    (if (and start end)
        (opencode-session--replace-message-region start end text face)
      (opencode-session--insert-message message text face))))

(defun opencode-session--replace-message-region (start end text face)
  "Replace text between START and END with TEXT and FACE."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (marker-position start))
      (delete-region (marker-position start) (marker-position end))
      (let ((new-start (point)))
        (insert text)
        (let ((new-end (point)))
          (set-marker start new-start)
          (set-marker end new-end)
          (opencode-session--apply-message-properties new-start new-end face))))))

(defun opencode-session--insert-message (message text face)
  "Insert MESSAGE with TEXT and FACE at the end of the log."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (marker-position opencode-session--input-start-marker))
      (let ((start (point)))
        (insert text)
        (let ((end (point)))
          (setf (opencode-message-start-marker message) (copy-marker start))
          (setf (opencode-message-end-marker message) (copy-marker end)))
        (insert "\n\n")
        (set-marker opencode-session--input-start-marker (point))
        (set-marker opencode-session--input-marker (point))
        (opencode-session--apply-message-properties start (point) face)))))

(defun opencode-session--apply-message-properties (start end face)
  "Apply message properties from START to END with FACE."
  (let ((properties '(read-only t front-sticky t rear-nonsticky t)))
    (add-text-properties start end (if face (append properties `(face ,face)) properties))))

(defun opencode-session--message-text (message)
  "Return the renderable text for MESSAGE."
  (let ((parts (opencode-message-parts message)))
    (if (and parts (listp parts))
        (opencode-session--render-message-parts message parts)
      (or (opencode-message-text message) ""))))

(defun opencode-session--render-message-parts (message parts)
  "Render PARTS for MESSAGE into a string."
  (let ((segments (delq nil (mapcar (lambda (part)
                                      (opencode-session--render-message-part message (cdr part)))
                                    parts))))
    (string-join segments "\n")))

(defun opencode-session--render-message-part (message part)
  "Render a single message PART for MESSAGE."
  (let ((part-type (opencode-message-part-type part)))
    (cond
     ((string= part-type "text")
      (let ((text (or (opencode-message-part-text part) ""))
            (face (opencode-session--role-face message)))
        (when (string-empty-p text)
          (setq text ""))
        (propertize text 'face face)))
     ((string= part-type "tool")
      (opencode-session--tool-part-line part))
     (t nil))))

(defun opencode-session--tool-part-line (part)
  "Render a tool call PART as a single line."
  (let* ((state (opencode-message-part-state part))
         (input (alist-get 'input state))
         (description (or (alist-get 'description input)
                          (alist-get 'title state)
                          (opencode-message-part-tool part)
                          "tool"))
         (status (or (alist-get 'status state) "pending")))
    (propertize (format "%s [%s]" description status)
                'face 'opencode-session-tool-face)))

(defun opencode-session--role-face (message)
  "Return the face for MESSAGE role."
  (let ((role (opencode-message-role message)))
    (if (string= role "user")
        'opencode-session-user-face
      'opencode-session-assistant-face)))

(defun opencode-session--ensure-input-region ()
  "Ensure the input marker sits at the end of the buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (set-marker opencode-session--input-marker (point)))
  (opencode-session--goto-input))

(defun opencode-session--goto-input ()
  "Move point to the input region."
  (when opencode-session--input-marker
    (let ((input-pos (marker-position opencode-session--input-marker)))
      (when (< (point) input-pos)
        (goto-char input-pos)))))

(defun opencode-session--current-input ()
  "Return current input contents as a string."
  (if opencode-session--input-marker
      (buffer-substring-no-properties (marker-position opencode-session--input-start-marker)
                                      (marker-position opencode-session--input-marker))
    ""))

(defun opencode-session--send-input (connection session input)
  "Send INPUT to SESSION using CONNECTION.

Restores INPUT when the request fails."
  (let ((session-id (opencode-session-id session))
        (payload `(("type" . "text") ("text" . ,input)))
        (agent opencode-session--agent))
    (opencode-client-session-prompt-async
     connection
     session-id
     (list payload)
     :agent agent
     :success (lambda (&rest _args)
                (message "OpenCode: message queued"))
     :error (lambda (&rest _args)
              (opencode-session--restore-input input)
              (message "OpenCode: failed to send message")))))

(defun opencode-session--restore-input (input)
  "Restore INPUT into the input area."
  (let ((inhibit-read-only t))
    (goto-char (marker-position opencode-session--input-marker))
    (insert input))
  (opencode-session--goto-input))

(defun opencode-session--clear-input ()
  "Clear the input region."
  (let ((inhibit-read-only t))
    (delete-region (marker-position opencode-session--input-start-marker)
                   (marker-position opencode-session--input-marker)))
  (opencode-session--goto-input))

(defun opencode-session--find-message (message-id)
  "Return the message with MESSAGE-ID, if any."
  (cl-find message-id opencode-session--messages
           :key #'opencode-message-id
           :test #'string=))

(defun opencode-session--upsert-message (info)
  "Update message list using INFO."
  (let* ((message-id (alist-get 'id info))
         (message (opencode-session--find-message message-id)))
    (if message
        (opencode-session--update-message message info)
      (setq message (opencode-session--message-from-info info))
      (setq opencode-session--messages
            (append opencode-session--messages (list message))))))

(defun opencode-session--update-message (message info)
  "Update MESSAGE fields from INFO."
  (let* ((time (alist-get 'time info))
         (created (alist-get 'created time))
         (completed (alist-get 'completed time)))
    (setf (opencode-message-session-id message) (alist-get 'sessionID info))
    (setf (opencode-message-role message) (alist-get 'role info))
    (setf (opencode-message-parent-id message) (alist-get 'parentID info))
    (setf (opencode-message-model-id message) (alist-get 'modelID info))
    (setf (opencode-message-provider-id message) (alist-get 'providerID info))
    (setf (opencode-message-mode message) (alist-get 'mode info))
    (setf (opencode-message-agent message) (alist-get 'agent info))
    (setf (opencode-message-path message) (alist-get 'path info))
    (setf (opencode-message-time-created message) created)
    (setf (opencode-message-time-completed message) completed)
    (setf (opencode-message-finish message) (alist-get 'finish info))
    (setf (opencode-message-summary message) (alist-get 'summary info))
    (setf (opencode-message-info message) info)))

(defun opencode-session--message-from-info (info)
  "Create a message object from INFO."
  (when info
    (let ((message (opencode-message-create :id (alist-get 'id info))))
      (opencode-session--update-message message info)
      message)))

(defun opencode-session--update-message-part (part delta)
  "Update message part from PART with optional DELTA."
  (let* ((message-id (alist-get 'messageID part))
         (message (opencode-session--find-message message-id)))
    (when message
      (let* ((part-id (alist-get 'id part))
             (existing (assoc part-id (opencode-message-parts message)))
             (entry (or existing (cons part-id nil)))
             (data (opencode-session--message-part-from-info part))
             (previous (cdr entry)))
        (setcdr entry data)
        (when (and delta (opencode-message-part-p previous)
                   (string= (opencode-message-part-type data) "text"))
          (setf (opencode-message-part-text data)
                (concat (opencode-message-part-text previous) delta)))
        (if existing
            (setf (opencode-message-parts message)
                  (cl-subst entry existing (opencode-message-parts message)))
          (setf (opencode-message-parts message)
                (append (opencode-message-parts message) (list entry))))
        (setf (opencode-message-text message)
              (opencode-session--message-text message))
        (opencode-session--render-message message)))))

(defun opencode-session--message-part-from-info (info)
  "Create a message part object from INFO."
  (let* ((time (alist-get 'time info))
         (start (alist-get 'start time))
         (end (alist-get 'end time)))
    (opencode-message-part-create
     :id (alist-get 'id info)
     :session-id (alist-get 'sessionID info)
     :message-id (alist-get 'messageID info)
     :type (alist-get 'type info)
     :text (alist-get 'text info)
     :metadata (alist-get 'metadata info)
     :time-start start
     :time-end end
     :snapshot (alist-get 'snapshot info)
     :reason (alist-get 'reason info)
     :cost (alist-get 'cost info)
     :tokens (alist-get 'tokens info)
     :tool (alist-get 'tool info)
     :state (alist-get 'state info))))

(defun opencode-session--update-session (info)
  "Update the buffer session from INFO."
  (let* ((time (alist-get 'time info))
         (created (alist-get 'created time))
         (updated (alist-get 'updated time))
         (previous-name (and opencode-session--session
                             (opencode-session--buffer-name opencode-session--session))))
    (unless opencode-session--session
      (setq opencode-session--session (opencode-session-create :id (alist-get 'id info))))
    (setf (opencode-session-slug opencode-session--session) (alist-get 'slug info))
    (setf (opencode-session-version opencode-session--session) (alist-get 'version info))
    (setf (opencode-session-project-id opencode-session--session) (alist-get 'projectID info))
    (setf (opencode-session-directory opencode-session--session) (alist-get 'directory info))
    (setf (opencode-session-title opencode-session--session) (alist-get 'title info))
    (setf (opencode-session-time-created opencode-session--session) created)
    (setf (opencode-session-time-updated opencode-session--session) updated)
    (setf (opencode-session-summary opencode-session--session) (alist-get 'summary info))
    (setf (opencode-session-info opencode-session--session) info)
    (opencode-session--rename-buffer previous-name)
    (opencode-session--render-header)))

(defun opencode-session--rename-buffer (previous-name)
  "Rename the current buffer when session metadata changes.

PREVIOUS-NAME is the previous buffer name to compare against."
  (when opencode-session--session
    (let ((new-name (opencode-session--buffer-name opencode-session--session)))
      (when (and previous-name
                 (not (string= previous-name new-name))
                 (string= (buffer-name) previous-name))
        (rename-buffer new-name t)))))

(defun opencode-session--update-status (session-id status)
  "Update STATUS for SESSION-ID."
  (when-let ((buffer (opencode-session--buffer-for-session session-id)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when opencode-session--session
          (setf (opencode-session-status opencode-session--session) status)
          (opencode-session--render-header))))))

(defun opencode-session--handle-session-created (_event data)
  "Handle the session.created SSE DATA."
  (let* ((info (alist-get 'info (alist-get 'properties data)))
         (session-id (alist-get 'id info)))
    (when-let ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--update-session info))))))

(defun opencode-session--handle-session-updated (_event data)
  "Handle the session.updated SSE DATA."
  (let* ((info (alist-get 'info (alist-get 'properties data)))
         (session-id (alist-get 'id info)))
    (when-let ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--update-session info))))))

(defun opencode-session--handle-session-status (_event data)
  "Handle the session.status SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (status-info (alist-get 'status properties))
         (status (alist-get 'type status-info)))
    (opencode-session--update-status session-id status)))

(defun opencode-session--normalize-agents (data)
  "Normalize agent list DATA into a list of names."
  (let* ((agents (cond
                  ((vectorp data) (append data nil))
                  ((listp data) data)
                  (t nil)))
         (primary (cl-remove-if-not (lambda (agent)
                                      (or (stringp agent)
                                          (and (string= (alist-get 'mode agent) "primary")
                                               (not (alist-get 'hidden agent)))))
                                    agents))
         (names (mapcar (lambda (agent)
                          (cond
                           ((stringp agent) agent)
                           ((listp agent) (or (alist-get 'id agent)
                                              (alist-get 'name agent)))
                           (t nil)))
                        primary)))
    (delq nil names)))

(defun opencode-session--maybe-fetch-agents (connection)
  "Fetch and cache agents for CONNECTION when needed." 
  (unless (opencode-connection-agents connection)
    (opencode-client-agents
     connection
     :success (lambda (&rest args)
                (let* ((data (plist-get args :data))
                       (agents (opencode-session--normalize-agents data)))
                  (setf (opencode-connection-agents connection) agents)
                  (opencode-session--apply-default-agent connection)))
     :error (lambda (&rest _args)
              (message "OpenCode: failed to load agents")))))

(defun opencode-session--apply-default-agent (connection)
  "Apply the default agent for the current session buffer." 
  (when (and (eq connection opencode-session--connection)
             (not opencode-session--agent))
    (let ((agents (opencode-connection-agents connection)))
      (when (and agents (listp agents))
        (setq-local opencode-session--agent (car agents))
        (setq-local opencode-session--agent-index 0)
        (opencode-session--render-header)))))

(defun opencode-session--ensure-agents (connection)
  "Ensure agent list is available for CONNECTION." 
  (if (opencode-connection-agents connection)
      (opencode-session--apply-default-agent connection)
    (opencode-session--maybe-fetch-agents connection)))

(defun opencode-session--refresh-agents (connection)
  "Refresh the cached agent list for CONNECTION." 
  (setf (opencode-connection-agents connection) nil)
  (opencode-session--maybe-fetch-agents connection))

(defun opencode-session--available-agents ()
  "Return available agents for the current session buffer." 
  (when opencode-session--connection
    (opencode-connection-agents opencode-session--connection)))

(defun opencode-session--set-agent (agent index)
  "Set the current session agent to AGENT at INDEX." 
  (setq-local opencode-session--agent agent)
  (setq-local opencode-session--agent-index index)
  (opencode-session--render-header)
  (message "OpenCode agent: %s" agent))

(defun opencode-session-select-agent (agent)
  "Select AGENT for the current session buffer." 
  (interactive
   (progn
     (unless opencode-session--connection
       (error "OpenCode session is not connected"))
     (opencode-session--ensure-agents opencode-session--connection)
     (let ((agents (opencode-session--available-agents)))
       (unless agents
         (error "OpenCode agents not available"))
       (list (completing-read "OpenCode agent: " agents nil t
                              (or opencode-session--agent (car agents)))))))
  (let* ((agents (opencode-session--available-agents))
         (index (and agents (cl-position agent agents :test #'string=))))
    (if (and index agents)
        (opencode-session--set-agent agent index)
      (message "OpenCode: unknown agent %s" agent))))

(defun opencode-session--cycle-agent (step)
  "Cycle the current agent by STEP positions." 
  (unless opencode-session--connection
    (error "OpenCode session is not connected"))
  (opencode-session--ensure-agents opencode-session--connection)
  (let ((agents (opencode-session--available-agents)))
    (unless agents
      (error "OpenCode agents not available"))
    (let* ((count (length agents))
           (current (or opencode-session--agent-index 0))
           (next (mod (+ current step) count)))
      (opencode-session--set-agent (nth next agents) next))))

(defun opencode-session-next-agent ()
  "Select the next available agent." 
  (interactive)
  (opencode-session--cycle-agent 1))

(defun opencode-session-previous-agent ()
  "Select the previous available agent." 
  (interactive)
  (opencode-session--cycle-agent -1))

(defun opencode-session-refresh-agents ()
  "Refresh the available agents list for the session." 
  (interactive)
  (unless opencode-session--connection
    (error "OpenCode session is not connected"))
  (opencode-session--refresh-agents opencode-session--connection))

(defun opencode-session--handle-session-idle (_event data)
  "Handle the session.idle SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties)))
    (opencode-session--update-status session-id "idle")))

(defun opencode-session--handle-message-updated (_event data)
  "Handle the message.updated SSE DATA."
  (let* ((info (alist-get 'info (alist-get 'properties data)))
         (session-id (alist-get 'sessionID info)))
    (when-let ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--upsert-message info))))))

(defun opencode-session--handle-message-part-updated (_event data)
  "Handle the message.part.updated SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (part (alist-get 'part properties))
         (session-id (alist-get 'sessionID part))
         (delta (alist-get 'delta properties)))
    (when-let ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (member (alist-get 'type part) '("text" "tool"))
            (opencode-session--update-message-part part delta)))))))

(defun opencode-session--connection-directories ()
  "Return directories for active OpenCode connections."
  (let (directories)
    (maphash (lambda (_session-id buffer)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when-let ((connection opencode-session--connection)
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
  (let* ((properties (alist-get 'properties data))
         (file (alist-get 'file properties))
         (path (alist-get 'path properties))
         (file-path (cond
                     ((stringp file) file)
                     ((listp file) (or (alist-get 'path file)
                                       (alist-get 'name file)))))
         (paths (or (alist-get 'paths properties)
                    (alist-get 'files properties))))
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
            (when-let ((buffer-path (buffer-file-name buffer)))
              (let ((normalized-buffer (opencode-session--normalize-file-path buffer-path)))
                (when (and normalized-buffer
                           (string= normalized normalized-buffer))
                  (if (buffer-modified-p)
                      (message "OpenCode: buffer has unsaved changes (%s)" (buffer-name buffer))
                    (revert-buffer :ignore-auto :noconfirm)
                    (message "OpenCode: reloaded %s" (buffer-name buffer))))))))))))

(defun opencode-session--handle-file-updated (_event data)
  "Handle SSE file update DATA by reverting buffers."
  (dolist (path (opencode-session--event-file-paths data))
    (opencode-session--maybe-revert-buffer path)))

(defun opencode-session--load-history (connection session buffer)
  "Load existing messages for SESSION using CONNECTION into BUFFER."
  (opencode-client-session-messages
   connection
   (opencode-session-id session)
   :success (lambda (&rest args)
              (let* ((data (plist-get args :data))
                     (items (cond
                             ((listp data) data)
                             ((vectorp data) (append data nil))
                             (t nil))))
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (setq opencode-session--messages nil)
                    (dolist (item items)
                      (opencode-session--hydrate-message item))
                    (opencode-session--render-buffer)))))
   :error (lambda (&rest _args)
            (message "OpenCode: failed to load session history"))))

(defun opencode-session--hydrate-message (item)
  "Add a message ITEM returned from the API." 
  (let* ((info (alist-get 'info item))
         (parts (alist-get 'parts item))
         (message (opencode-session--message-from-info info)))
    (when message
      (setf (opencode-message-parts message)
            (opencode-session--hydrate-parts parts))
      (setf (opencode-message-text message)
            (opencode-session--message-text message))
      (setq opencode-session--messages
            (append opencode-session--messages (list message))))))

(defun opencode-session--hydrate-parts (parts)
  "Hydrate PARTS into an alist of message parts." 
  (let (result)
    (dolist (part (opencode-session--normalize-items parts))
      (let* ((part-id (alist-get 'id part))
             (data (opencode-session--message-part-from-info part))
             (existing (assoc part-id result)))
        (if existing
            (setcdr existing data)
          (push (cons part-id data) result))))
    (nreverse result)))

(defun opencode-session--normalize-items (items)
  "Normalize ITEMS to a list when vector or list." 
  (cond
   ((vectorp items) (append items nil))
   ((listp items) items)
   (t nil)))

(opencode-sse-define-handler session-created "session.created" (_event data)
  (opencode-session--handle-session-created _event data))

(opencode-sse-define-handler session-updated "session.updated" (_event data)
  (opencode-session--handle-session-updated _event data))

(opencode-sse-define-handler session-status "session.status" (_event data)
  (opencode-session--handle-session-status _event data))

(opencode-sse-define-handler session-idle "session.idle" (_event data)
  (opencode-session--handle-session-idle _event data))

(opencode-sse-define-handler message-updated "message.updated" (_event data)
  (opencode-session--handle-message-updated _event data))

(opencode-sse-define-handler message-part-updated "message.part.updated" (_event data)
  (opencode-session--handle-message-part-updated _event data))

(opencode-sse-define-handler file-edited "file.edited" (_event data)
  (opencode-session--handle-file-updated _event data))

(opencode-sse-define-handler file-watcher-updated "file.watcher.updated" (_event data)
  (opencode-session--handle-file-updated _event data))

(provide 'emacs-opencode-session-mode)

;;; emacs-opencode-session-mode.el ends here
