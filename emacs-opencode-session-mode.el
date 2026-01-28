;;; emacs-opencode-session-mode.el --- OpenCode session buffer  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
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

(defvar-local opencode-session--input-marker nil
  "Marker indicating the start of the input region.")

(defvar-local opencode-session--header-marker nil
  "Marker indicating the end of the header region.")

(defvar opencode-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'opencode-session-send-input)
    (define-key map (kbd "RET") #'newline)
    (define-key map [remap self-insert-command] #'opencode-session-self-insert)
    (define-key map [remap yank] #'opencode-session-yank)
    (define-key map [remap delete-backward-char] #'opencode-session-delete-backward)
    (define-key map [remap backward-delete-char-untabify] #'opencode-session-delete-backward)
    map)
  "Keymap for `opencode-session-mode`."
  )

(define-derived-mode opencode-session-mode text-mode "OpenCode-Session"
  "Major mode for OpenCode session buffers."
  (setq-local buffer-read-only nil)
  (setq-local opencode-session--messages nil)
  (opencode-session--ensure-markers))

(defun opencode-session-open (session &optional connection)
  "Open a session buffer for SESSION and return it.

When CONNECTION is provided, load existing session messages." 
  (let* ((slug (or (opencode-session-slug session)
                   (opencode-session-id session)
                   "session"))
         (name (format "*opencode-session<%s>*" slug))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (opencode-session-mode)
      (setq-local opencode-session--session session)
      (setq-local opencode-session--connection connection)
      (opencode-session--register-buffer session buffer)
      (opencode-session--render-buffer))
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
  "Ensure input and header markers exist."
  (unless opencode-session--input-marker
    (setq-local opencode-session--input-marker (copy-marker (point-max) t)))
  (unless opencode-session--header-marker
    (setq-local opencode-session--header-marker (copy-marker (point-min)))))

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
         (header (concat (propertize title 'face 'opencode-session-header-face)
                         " "
                         (propertize (format "[%s]" status)
                                     'face 'opencode-session-status-face)
                         "\n")))
    (opencode-session--replace-header header)))

(defun opencode-session--replace-header (text)
  "Replace the header region with TEXT." 
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when opencode-session--header-marker
        (delete-region (point-min) (marker-position opencode-session--header-marker)))
      (let ((start (point)))
        (insert text)
        (add-text-properties start (point)
                             '(read-only t front-sticky t rear-nonsticky t))))))

(defun opencode-session--render-messages ()
  "Render all messages for the session." 
  (dolist (message opencode-session--messages)
    (opencode-session--render-message message)))

(defun opencode-session--render-message (message)
  "Render MESSAGE into the buffer." 
  (let ((text (opencode-session--message-text message))
        (face (opencode-session--role-face message)))
    (opencode-session--replace-message message text face)))

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
        (insert "\n\n")
        (let ((new-end (point)))
          (set-marker start new-start)
          (set-marker end new-end)
          (add-text-properties new-start new-end
                               `(read-only t face ,face front-sticky t rear-nonsticky t)))))))

(defun opencode-session--insert-message (message text face)
  "Insert MESSAGE with TEXT and FACE at the end of the log." 
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (marker-position opencode-session--input-marker))
      (let ((start (point)))
        (insert text)
        (let ((end (point)))
          (setf (opencode-message-start-marker message) (copy-marker start))
          (setf (opencode-message-end-marker message) (copy-marker end))
          (set-marker opencode-session--input-marker end))
        (insert "\n\n")
        (add-text-properties start (point)
                             `(read-only t face ,face front-sticky t rear-nonsticky t))))))

(defun opencode-session--message-text (message)
  "Return the renderable text for MESSAGE." 
  (let ((parts (opencode-message-parts message)))
    (if (and parts (listp parts))
        (mapconcat
         (lambda (part)
           (or (opencode-message-part-text (cdr part)) ""))
         (cl-remove-if-not
          (lambda (part)
            (string= (opencode-message-part-type (cdr part)) "text"))
          parts)
         "")
      (or (opencode-message-text message) ""))))

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
      (buffer-substring-no-properties (marker-position opencode-session--input-marker)
                                      (point-max))
    ""))

(defun opencode-session--send-input (connection session input)
  "Send INPUT to SESSION using CONNECTION.

Restores INPUT when the request fails." 
  (let ((session-id (opencode-session-id session))
        (payload `(("type" . "text") ("text" . ,input))))
    (opencode-client-session-prompt-async
     connection
     session-id
     (list payload)
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
    (delete-region (marker-position opencode-session--input-marker) (point-max)))
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
        (when (and delta (opencode-message-part-p previous))
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
     :tokens (alist-get 'tokens info))))

(defun opencode-session--update-session (info)
  "Update the buffer session from INFO." 
  (let* ((time (alist-get 'time info))
         (created (alist-get 'created time))
         (updated (alist-get 'updated time)))
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
    (opencode-session--render-header)))

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
          (when (string= (alist-get 'type part) "text")
            (opencode-session--update-message-part part delta)))))))

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

(provide 'emacs-opencode-session-mode)

;;; emacs-opencode-session-mode.el ends here
