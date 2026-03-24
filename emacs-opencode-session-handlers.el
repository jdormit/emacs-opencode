;;; emacs-opencode-session-handlers.el --- Session event handlers  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-opencode-session-vars)
(require 'emacs-opencode-message)
(require 'emacs-opencode-client)
(require 'emacs-opencode-connection)

(declare-function opencode-session--update-session "emacs-opencode-session-mode")
(declare-function opencode-session--update-status "emacs-opencode-session-mode")
(declare-function opencode-session--render-header "emacs-opencode-session-header")
(declare-function opencode-session--upsert-message "emacs-opencode-session-mode")
(declare-function opencode-session--update-message-part "emacs-opencode-session-mode")
(declare-function opencode-session--find-message "emacs-opencode-session-mode")
(declare-function opencode-session--message-text "emacs-opencode-session-render")
(declare-function opencode-session--render-message "emacs-opencode-session-render")

;;; Session event handlers

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

(defun opencode-session--session-error-text (error-info)
  "Return user-facing text extracted from session ERROR-INFO."
  (let* ((data (and (listp error-info) (alist-get 'data error-info)))
         (detail (and (listp data) (alist-get 'message data))))
    (cond
     ((and (stringp detail) (not (string-empty-p detail))) detail)
     ((and (listp error-info)
           (stringp (alist-get 'name error-info))
           (not (string-empty-p (alist-get 'name error-info))))
      (alist-get 'name error-info))
     ((stringp error-info) error-info)
     (t "An error occurred"))))

(defun opencode-session--handle-session-error (_event data)
  "Handle the session.error SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (error-info (alist-get 'error properties))
         (error-name (and (listp error-info) (alist-get 'name error-info))))
    (unless (and (stringp error-name)
                 (string= error-name "MessageAbortedError"))
      (message "OpenCode: %s" (opencode-session--session-error-text error-info))
      (when session-id
        (when-let ((buffer (opencode-session--buffer-for-session session-id)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (opencode-session--render-header))))))))

;;; Message event handlers

(defun opencode-session--handle-message-updated (_event data)
  "Handle the message.updated SSE DATA."
  (let* ((info (alist-get 'info (alist-get 'properties data)))
         (session-id (alist-get 'sessionID info)))
    (when-let ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--upsert-message info))))))

(defun opencode-session--handle-message-part-updated (_event data)
  "Handle the message.part.updated SSE DATA.
Routes events to the owning session buffer.  When the session has
no buffer but is a registered subagent, update the subagent tool
tracking and re-render the parent task tool part."
  (let* ((properties (alist-get 'properties data))
         (part (alist-get 'part properties))
         (session-id (alist-get 'sessionID part))
         (delta (alist-get 'delta properties)))
    (if-let ((buffer (opencode-session--buffer-for-session session-id)))
        ;; Normal case: session has a buffer
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (member (alist-get 'type part) '("text" "tool" "reasoning"))
              (opencode-session--update-message-part part delta))))
      ;; Subagent case: track tool calls and re-render parent
      (when (and (string= (alist-get 'type part) "tool")
                 (opencode-session--subagent-parent session-id))
        (opencode-session--track-subagent-tool session-id part)))))

(defun opencode-session--handle-message-part-delta (_event data)
  "Handle the message.part.delta SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (message-id (alist-get 'messageID properties))
         (part-id (alist-get 'partID properties))
         (field (alist-get 'field properties))
         (delta (alist-get 'delta properties)))
    (when (and (string= field "text")
               (stringp delta))
      (when-let ((buffer (opencode-session--buffer-for-session session-id)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when-let ((message (opencode-session--find-message message-id)))
              (when-let* ((entry (assoc part-id (opencode-message-parts message)))
                          (part (cdr entry))
                          ((opencode-message-part-p part)))
                (setf (opencode-message-part-text part)
                      (concat (or (opencode-message-part-text part) "") delta))
                (setf (opencode-message-text message)
                      (opencode-session--message-text message))
                (opencode-session--render-message message)))))))))

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
    (when-let ((message (opencode-session--find-message-by-part task-part-id)))
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
           (metadata (alist-get 'metadata state))
           (subagent-session-id (alist-get 'sessionId metadata))
           (part-id (alist-get 'id part)))
      (when subagent-session-id
        (opencode-session--register-subagent
         subagent-session-id parent-session-id part-id)))))

;;; Permission handling

(defun opencode-session--permission-patterns (permission)
  "Return a list of pattern strings from PERMISSION."
  (let ((patterns (alist-get 'patterns permission)))
    (cond
     ((vectorp patterns) (append patterns nil))
     ((listp patterns) patterns)
     (t nil))))

(defun opencode-session--permission-detail (permission)
  "Return a detail string for PERMISSION when available."
  (let* ((kind (alist-get 'permission permission))
         (metadata (alist-get 'metadata permission))
         (patterns (opencode-session--permission-patterns permission))
         (pattern (car patterns)))
    (cond
     ((and (string= kind "read") (alist-get 'filePath metadata))
      (format "read %s" (alist-get 'filePath metadata)))
     ((and (string= kind "edit") (alist-get 'filepath metadata))
      (format "edit %s" (alist-get 'filepath metadata)))
     ((and (string= kind "glob") (alist-get 'pattern metadata))
      (format "glob %s" (alist-get 'pattern metadata)))
     ((and (string= kind "grep") (alist-get 'pattern metadata))
      (format "grep %s" (alist-get 'pattern metadata)))
     ((and (string= kind "list") (alist-get 'path metadata))
      (format "list %s" (alist-get 'path metadata)))
     ((and (string= kind "bash") (alist-get 'command metadata))
      (if-let ((description (alist-get 'description metadata)))
          (format "%s (%s)" description (alist-get 'command metadata))
        (format "%s" (alist-get 'command metadata))))
     ((and (string= kind "task") (alist-get 'subagent_type metadata))
      (format "task %s" (alist-get 'subagent_type metadata)))
     ((and (string= kind "webfetch") (alist-get 'url metadata))
      (format "web search %s" (alist-get 'url metadata)))
     ((and (member kind '("websearch" "codesearch")) (alist-get 'query metadata))
      (format "%s %s" (capitalize kind) (alist-get 'query metadata)))
     ((and (string= kind "external_directory") pattern)
      (format "access external directory %s" pattern))
     (pattern
      (format "%s" pattern))
     (t nil))))

(defun opencode-session--permission-prompt-label (permission)
  "Return the minibuffer prompt label for PERMISSION."
  (let* ((kind (alist-get 'permission permission))
         (detail (opencode-session--permission-detail permission))
         (fallback (if kind (format "use %s" kind) "proceed")))
    (format "OpenCode wants to %s: " (or detail fallback))))

(defun opencode-session--prompt-permission (permission)
  "Prompt for PERMISSION and send a response."
  (let* ((request-id (alist-get 'id permission))
         (session-id (alist-get 'sessionID permission))
         (choices '("Allow once" "Allow always" "Deny"))
         (prompt (opencode-session--permission-prompt-label permission))
         (selection (condition-case nil
                        (completing-read prompt choices nil t)
                      (quit "Deny")))
         (reply (cond
                 ((string= selection "Allow always") "always")
                 ((string= selection "Allow once") "once")
                 (t "reject"))))
    (unless opencode-session--connection
      (error "OpenCode session is not connected"))
    (unless request-id
      (error "OpenCode permission request is missing ID"))
    (opencode-client-permission-reply
     opencode-session--connection
     request-id
     reply
     :success (lambda (&rest _args)
                (message "OpenCode permission reply sent"))
     :error (lambda (&rest _args)
              (message "OpenCode: failed to reply to permission request")))))

(defun opencode-session--handle-permission-asked (_event data)
  "Handle the permission.asked SSE DATA.
Falls back to any live session buffer when SESSION-ID is unknown,
e.g. for permission requests originating from subagent sessions.
Defers to a timer so `completing-read' does not block the process filter."
  (let* ((permission (alist-get 'properties data))
         (session-id (alist-get 'sessionID permission)))
    (run-at-time 0 nil
     (lambda ()
       (when-let ((buffer (or (opencode-session--buffer-for-session session-id)
                              (opencode-session--any-live-session-buffer))))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (opencode-session--prompt-permission permission))))))))

;;; Question handling

(defun opencode-session--question-list (questions)
  "Normalize QUESTIONS into a list."
  (cond
   ((vectorp questions) (append questions nil))
   ((listp questions) questions)
   (t nil)))

(defun opencode-session--question-options (question)
  "Return option labels for QUESTION."
  (let ((options (alist-get 'options question)))
    (mapcar (lambda (option) (alist-get 'label option))
            (opencode-session--normalize-items options))))

(defun opencode-session--question-multiple-p (question)
  "Return non-nil if QUESTION allows multiple answers."
  (eq (alist-get 'multiple question) t))

(defun opencode-session--question-custom-p (question)
  "Return non-nil if QUESTION allows custom answers."
  (let ((custom (alist-get 'custom question :missing)))
    (not (or (eq custom :json-false)
             (eq custom json-false)
             (eq custom nil)))))

(defun opencode-session--question-prompt-label (question)
  "Return the minibuffer prompt label for QUESTION."
  (let ((header (alist-get 'header question))
        (text (alist-get 'question question)))
    (if (and header (not (string-empty-p header)))
        (format "OpenCode %s: %s " header text)
      (format "OpenCode: %s " text))))

(defun opencode-session--question-read-custom (prompt)
  "Read a custom answer using PROMPT."
  (read-string (concat prompt "(Other): ")))

(defun opencode-session--question-read-single (question)
  "Prompt for a single answer to QUESTION.

Returns a list containing one answer string."
  (let* ((prompt (opencode-session--question-prompt-label question))
         (options (opencode-session--question-options question))
         (custom (opencode-session--question-custom-p question))
         (choices (if custom (append options '("Other")) options))
         (selection (completing-read prompt choices nil t)))
    (if (and custom (string= selection "Other"))
        (list (opencode-session--question-read-custom prompt))
      (list selection))))

(defun opencode-session--question-read-multiple (question)
  "Prompt for multiple answers to QUESTION.

Returns a list of answer strings."
  (let* ((prompt (opencode-session--question-prompt-label question))
         (options (opencode-session--question-options question))
         (custom (opencode-session--question-custom-p question))
         (choices (if custom (append options '("Other")) options))
         (selection (completing-read-multiple prompt choices nil t)))
    (if (and custom (member "Other" selection))
        (let ((custom-answer (opencode-session--question-read-custom prompt)))
          (append (remove "Other" selection) (list custom-answer)))
      selection)))

(defun opencode-session--question-answers (questions)
  "Return answers for QUESTIONS via minibuffer prompts."
  (mapcar (lambda (question)
            (if (opencode-session--question-multiple-p question)
                (opencode-session--question-read-multiple question)
              (opencode-session--question-read-single question)))
          questions))

(defun opencode-session--prompt-question (payload)
  "Prompt for question PAYLOAD and send a response."
  (let* ((request-id (alist-get 'id payload))
         (session-id (alist-get 'sessionID payload))
         (questions (opencode-session--question-list (alist-get 'questions payload)))
         (answers (condition-case nil
                      (opencode-session--question-answers questions)
                    (quit :reject))))
    (unless opencode-session--connection
      (error "OpenCode session is not connected"))
    (unless request-id
      (error "OpenCode question request is missing ID"))
    (if (eq answers :reject)
        (opencode-client-question-reject
         opencode-session--connection
         request-id
         :success (lambda (&rest _args)
                    (message "OpenCode question rejected"))
         :error (lambda (&rest _args)
                  (message "OpenCode: failed to reject question")))
      (opencode-client-question-reply
       opencode-session--connection
       request-id
       answers
       :success (lambda (&rest _args)
                  (message "OpenCode question reply sent"))
       :error (lambda (&rest _args)
                (message "OpenCode: failed to reply to question"))))))

(defun opencode-session--handle-question-asked (_event data)
  "Handle the question.asked SSE DATA.
Falls back to any live session buffer when SESSION-ID is unknown,
e.g. for questions originating from subagent sessions.
Defers to a timer so `completing-read' does not block the process filter."
  (let* ((question (alist-get 'properties data))
         (session-id (alist-get 'sessionID question))
         (request-id (alist-get 'id question)))
    (run-at-time 0 nil
     (lambda ()
       (message "OpenCode: question.asked for %s (session %s)" request-id session-id)
       (when-let ((buffer (or (opencode-session--buffer-for-session session-id)
                              (opencode-session--any-live-session-buffer))))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (message "OpenCode: prompting question %s in %s" request-id (buffer-name))
             (opencode-session--prompt-question question))))))))

;;; File revert handling

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

(provide 'emacs-opencode-session-handlers)

;;; emacs-opencode-session-handlers.el ends here
