;;; emacs-opencode-session-mode.el --- OpenCode session buffer  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-opencode-session-vars)
(require 'emacs-opencode-session-render)
(require 'emacs-opencode-session-header)
(require 'emacs-opencode-session-fontify)
(require 'emacs-opencode-session-model)
(require 'emacs-opencode-session-handlers)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-message)
(require 'emacs-opencode-session)
(require 'emacs-opencode-client)

(declare-function opencode-run-server "emacs-opencode" (directory &optional on-ready))
(declare-function opencode-session--maybe-register-subagent "emacs-opencode-session-handlers")
(declare-function opencode-acp-request "emacs-opencode-acp")
(declare-function opencode-acp-notify "emacs-opencode-acp")
(declare-function opencode-acp--finalize-current-message "emacs-opencode-acp-handlers")
(declare-function opencode-client-session-get "emacs-opencode-client")

(defvar opencode-acp--part-counter)
(defvar opencode-acp--seen-permission-ids)

(defvar-local opencode-session--command-reconcile-timer nil
  "Timer for polling the server's user message after a slash command.")

(defvar-local opencode-session--status-poll-timer nil
  "Timer for polling session status during HTTP-initiated prompts.")

(defvar-local opencode-session--task-poll-timers nil
  "Alist of active sub-agent poll timers: ((toolCallId . timer) ...).")

(defvar-local opencode-session--question-poll-timer nil
  "Timer for polling questions and permissions during prompt execution.")

(defvar-local opencode-session--poll-failure-count 0
  "Consecutive HTTP polling failure count.
When this reaches `opencode-session--poll-failure-limit', all polling
timers for the buffer are stopped.")

(defconst opencode-session--poll-failure-limit 5
  "Stop polling after this many consecutive HTTP failures.")

(defcustom opencode-session-input-prompt "❯ "
  "Prompt string shown before the session input area."
  :type 'string
  :group 'emacs-opencode)

(defcustom opencode-session-completion-providers
  '(opencode-session--complete-agent
    opencode-session--complete-command)
  "Completion providers for `opencode-session-mode` input.

Each function is called with point at the current input position and should
return a completion-at-point result or nil. Providers are tried in order until
one returns a completion result."
  :type '(repeat function)
  :group 'emacs-opencode)

(defface opencode-session-input-prompt-face
  '((t :inherit font-lock-constant-face))
  "Face used for the session input prompt."
  :group 'emacs-opencode)

(defvar opencode-session-send-input-hook nil
  "Hook run when input is submitted.

Each function receives SESSION and INPUT as arguments.")

(defvar opencode-command-arguments-history nil
  "History list for OpenCode command arguments.")

(defvar-local opencode-session--input-prompt-overlay nil
  "Overlay used to display the input prompt.")

;;; Keymap and mode definition

(defvar opencode-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'opencode-session-send-input)
    (define-key map (kbd "C-c C-a") #'opencode-session-select-agent)
    (define-key map (kbd "C-c C-n") #'opencode-session-next-agent)
    (define-key map (kbd "C-c C-p") #'opencode-session-previous-agent)
    (define-key map (kbd "C-c C-r") #'opencode-session-refresh-agents)
    (define-key map (kbd "C-c C-k") #'opencode-session-interrupt)
    (define-key map (kbd "C-c C-l") #'opencode-session-select-model)
    (define-key map (kbd "C-c C-v") #'opencode-session-select-variant)
    (define-key map (kbd "C-c C-]") #'opencode-session-next-variant)
    (define-key map (kbd "C-c C-[") #'opencode-session-previous-variant)
    (define-key map (kbd "C-c C-o") #'opencode-command)
    (define-key map (kbd "S-TAB") #'opencode-session-previous-agent)
    (define-key map (kbd "<backtab>") #'opencode-session-previous-agent)
    (define-key map (kbd "RET") #'newline)
    (define-key map (kbd "C-<tab>") #'completion-at-point)
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
             (fboundp 'evil-define-key*))
    (evil-define-key* '(normal insert) (current-local-map)
      (kbd "TAB") #'opencode-session-next-agent
      (kbd "S-TAB") #'opencode-session-previous-agent
      (kbd "<backtab>") #'opencode-session-previous-agent))
  (setq-local font-lock-defaults '(opencode-session--font-lock-keywords t))
  (setq-local font-lock-multiline t)
  (setq-local buffer-read-only nil)
  (setq-local opencode-session--messages nil)
  (setq-local opencode-session--agent nil)
  (setq-local opencode-session--provider-id nil)
  (setq-local opencode-session--model-id nil)
  (setq-local opencode-session--variant nil)
  (opencode-session--ensure-markers)
  (add-hook 'completion-at-point-functions
            #'opencode-session--completion-at-point
            nil
            t))

;;; Session lifecycle

(defun opencode-session-open (session &optional connection on-history-loaded)
  "Open a session buffer for SESSION and return it.

When CONNECTION is provided, load existing session messages. If
ON-HISTORY-LOADED is non-nil, call it with BUFFER after the history
request completes."
  (let* ((name (opencode-session--buffer-name session))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (opencode-session-mode)
      (setq-local opencode-session--session session)
      (setq-local opencode-session--connection connection)
      (when opencode-session--connection
        (setq-local default-directory
                    (opencode-connection-directory opencode-session--connection)))
      (opencode-session--register-buffer session buffer)
      (when connection
        (opencode-session--ensure-agents connection))
      (opencode-session--render-buffer))
    (if (and connection (opencode-session-id session))
        (opencode-session--load-history connection session buffer on-history-loaded)
      (when on-history-loaded
        (funcall on-history-loaded buffer)))
    (pop-to-buffer buffer)
    buffer))

;;; Connection management

(defun opencode-session--ensure-connection (callback)
  "Ensure the current session buffer has a live connection.

When the connection is alive, call CALLBACK immediately with the
connection.  When the connection is dead or missing, start a new
server for the session directory, update the buffer-local
connection, resume the session on the new ACP server, and then
call CALLBACK with the new connection."
  (if (and opencode-session--connection
           (opencode-connection-alive-p opencode-session--connection))
      (funcall callback opencode-session--connection)
    (let ((directory (or (and opencode-session--session
                              (opencode-session-directory opencode-session--session))
                         (and opencode-session--connection
                              (opencode-connection-directory
                               opencode-session--connection))))
          (session-id (and opencode-session--session
                          (opencode-session-id opencode-session--session)))
          (buffer (current-buffer)))
      (unless directory
        (error "OpenCode session has no associated directory"))
      (message "OpenCode: reconnecting...")
      (opencode-run-server
       directory
       (lambda (connection)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq-local opencode-session--connection connection)
             (opencode-session--ensure-agents connection)
             ;; Resume session on the new ACP server so it's registered.
             (if session-id
                 (opencode-acp-request
                  (opencode-connection-process connection)
                  "session/resume"
                  (let ((params (make-hash-table :test 'equal)))
                    (puthash "sessionId" session-id params)
                    (puthash "cwd" (directory-file-name directory) params)
                    (puthash "mcpServers" [] params)
                    params)
                  :success (lambda (_result)
                             (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (message "OpenCode: reconnected")
                                 (funcall callback connection))))
                  :error (lambda (err)
                           (message "OpenCode: failed to resume session: %s"
                                    (alist-get 'message err))))
               (message "OpenCode: reconnected")
               (funcall callback connection)))))))))

;;; Input handling

;;;###autoload
(defun opencode-session-insert-input (input)
  "Insert INPUT into the session input area."
  (unless (derived-mode-p 'opencode-session-mode)
    (error "Not in an OpenCode session buffer"))
  (opencode-session--ensure-markers)
  (opencode-session--ensure-input-region)
  (let ((inhibit-read-only t))
    (delete-region (marker-position opencode-session--input-start-marker)
                   (marker-position opencode-session--input-marker))
    (goto-char (marker-position opencode-session--input-marker))
    (insert input))
  (opencode-session--goto-input))

(defun opencode-session-send-input ()
  "Send the current input region content."
  (interactive)
  (let ((input (opencode-session--current-input)))
    (if (string-empty-p (string-trim input))
        (message "OpenCode input is empty")
      (unless opencode-session--session
        (error "OpenCode session is not connected"))
      (let ((buffer (current-buffer))
            (classified (opencode-session--classify-input input)))
        (opencode-session--ensure-connection
         (lambda (connection)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (pcase (car classified)
                 ('command
                  (opencode-session--maybe-send-command connection
                                                        opencode-session--session
                                                        input))
                 ('shell
                  (opencode-session--send-shell connection
                                                opencode-session--session
                                                (cdr classified))
                  (opencode-session--clear-input)
                  (message "OpenCode shell command submitted"))
                 ('message
                   (opencode-session--clear-input)
                   (opencode-session--send-input connection
                                                 opencode-session--session
                                                 input)))))))))))


;;;###autoload
(defun opencode-command ()
  "Prompt for an OpenCode command and send it to the current session."
  (interactive)
  (unless (derived-mode-p 'opencode-session-mode)
    (error "Not in an OpenCode session buffer"))
  (unless opencode-session--session
    (error "OpenCode session is not connected"))
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((session-id (opencode-session-id opencode-session--session))
                 (agent opencode-session--agent)
                 (model (opencode-session--selected-model-string))
                 (variant opencode-session--variant))
             (opencode-client-commands
              connection
              :success (lambda (&rest args)
                         (let* ((data (plist-get args :data))
                                (items (opencode-session--command-items data))
                                (names (opencode-session--command-names items)))
                           (unless names
                             (error "No OpenCode commands available"))
                           (let* ((command (completing-read "OpenCode command: " names nil t))
                                  (arguments (read-from-minibuffer
                                              "OpenCode command args (optional): "
                                              nil nil nil
                                              'opencode-command-arguments-history)))
                             (opencode-client-session-command
                              connection
                              session-id
                              command
                              arguments
                              :agent agent
                              :variant variant
                              :model model
                              :success (lambda (&rest _args)
                                         (message "OpenCode command queued"))
                              :error (lambda (&rest _args)
                                       (message "OpenCode: failed to send command"))))))
              :error (lambda (&rest _args)
                       (error "Failed to fetch OpenCode commands"))))))))))

(defun opencode-session-self-insert (n)
  "Insert N characters into the session input area."
  (interactive "p")
  (opencode-session--maybe-goto-input)
  (self-insert-command n))

(defun opencode-session-yank (arg)
  "Yank ARG into the session input area."
  (interactive "P")
  (opencode-session--maybe-goto-input)
  (yank arg))

(defun opencode-session-delete-backward (arg)
  "Delete ARG characters backward inside the input area."
  (interactive "p")
  (opencode-session--maybe-goto-input)
  (backward-delete-char-untabify arg))

(defun opencode-session-interrupt ()
  "Interrupt the active prompt for the current session."
  (interactive)
  (require 'emacs-opencode-acp)
  (unless opencode-session--session
    (error "OpenCode session is not connected"))
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((session-id (opencode-session-id opencode-session--session)))
             (unless session-id
               (error "OpenCode session ID is missing"))
             ;; ACP cancel is a notification (no response)
             (opencode-acp-notify
              (opencode-connection-process connection)
              "session/cancel"
              `((sessionId . ,session-id)))
             (message "OpenCode: interrupt requested"))))))))

;;; Input area management

(defun opencode-session--ensure-markers ()
  "Ensure input markers exist."
  (unless opencode-session--input-start-marker
    (setq-local opencode-session--input-start-marker (copy-marker (point-max))))
  (unless opencode-session--input-marker
    (setq-local opencode-session--input-marker (copy-marker (point-max) t))))

(defun opencode-session--ensure-input-region ()
  "Ensure the input marker sits at the end of the buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (set-marker opencode-session--input-marker (point)))
  (opencode-session--ensure-input-prompt)
  (opencode-session--goto-input))

(defun opencode-session--ensure-input-prompt ()
  "Ensure the input prompt overlay is up to date."
  (when opencode-session--input-start-marker
    (let ((prompt opencode-session-input-prompt))
      (if (or (null prompt) (string-empty-p prompt))
          (when (overlayp opencode-session--input-prompt-overlay)
            (delete-overlay opencode-session--input-prompt-overlay)
            (setq opencode-session--input-prompt-overlay nil))
        (let ((pos (marker-position opencode-session--input-start-marker)))
          (unless (overlayp opencode-session--input-prompt-overlay)
            (setq opencode-session--input-prompt-overlay (make-overlay pos pos)))
          (move-overlay opencode-session--input-prompt-overlay pos pos)
          (overlay-put opencode-session--input-prompt-overlay
                       'before-string
                       (propertize prompt 'face 'opencode-session-input-prompt-face)))))))

(defun opencode-session--goto-input ()
  "Move point to the input region."
  (when opencode-session--input-marker
    (let ((input-pos (marker-position opencode-session--input-marker)))
      (when (< (point) input-pos)
        (goto-char input-pos)))))

(defun opencode-session--maybe-goto-input ()
  "Move point to input when outside the input markers."
  (if (and opencode-session--input-start-marker
           opencode-session--input-marker)
      (let ((start (marker-position opencode-session--input-start-marker))
            (end (marker-position opencode-session--input-marker)))
        (when (or (< (point) start)
                  (> (point) end))
          (opencode-session--goto-input)))
    (opencode-session--goto-input)))

(defun opencode-session--current-input ()
  "Return current input contents as a string."
  (if opencode-session--input-marker
      (buffer-substring-no-properties (marker-position opencode-session--input-start-marker)
                                      (marker-position opencode-session--input-marker))
    ""))

(defun opencode-session--clear-input ()
  "Clear the input region."
  (let ((inhibit-read-only t))
    (delete-region (marker-position opencode-session--input-start-marker)
                   (marker-position opencode-session--input-marker)))
  (opencode-session--goto-input))

(defun opencode-session--restore-input (input)
  "Restore INPUT into the input area."
  (let ((inhibit-read-only t))
    (goto-char (marker-position opencode-session--input-marker))
    (insert input))
  (opencode-session--goto-input))

;;; Sending messages

(defun opencode-session--selected-model ()
  "Return the selected model as a cons (PROVIDER-ID . MODEL-ID) or nil."
  (when (and opencode-session--provider-id opencode-session--model-id)
    (cons opencode-session--provider-id opencode-session--model-id)))

(defun opencode-session--selected-model-string ()
  "Return the selected model as a \"provider/model\" string or nil."
  (when (and opencode-session--provider-id opencode-session--model-id)
    (format "%s/%s" opencode-session--provider-id opencode-session--model-id)))

(defun opencode-session--extract-agent-mentions (input)
  "Extract @-agent mentions from INPUT.
Returns a list of agent name strings found in INPUT that match
known completable agents.  Each mention must be preceded by
whitespace or appear at the start of the string."
  (let ((agents (opencode-session--available-completable-agents))
        (mentions nil)
        (start 0))
    (when agents
      (while (string-match "\\(?:^\\|[[:space:]]\\)@\\([a-zA-Z0-9_-]+\\)" input start)
        (let ((name (match-string 1 input)))
          (when (member name agents)
            (push name mentions)))
        (setq start (match-end 0))))
    (delete-dups (nreverse mentions))))

(defun opencode-session--build-message-parts (input)
  "Build the message parts list for INPUT.
Returns a list of part alists including a text part and any @-agent
parts extracted from the input."
  (let ((text-part `(("type" . "text") ("text" . ,input)))
        (agent-names (opencode-session--extract-agent-mentions input))
        (parts nil))
    (push text-part parts)
    (dolist (name agent-names)
      (push `(("type" . "agent") ("name" . ,name)) parts))
    (nreverse parts)))

(defun opencode-session--send-input (connection session input)
  "Send INPUT to SESSION using CONNECTION via ACP.

When @-agent mentions are present, sends via HTTP prompt_async (which
supports the per-prompt agent parameter) and polls session status for
completion.  Otherwise sends via ACP session/prompt.
Restores INPUT when the request fails."
  (require 'emacs-opencode-acp)
  (require 'emacs-opencode-acp-handlers)
  (let* ((session-id (opencode-session-id session))
         (buffer (current-buffer))
         (agent-names (opencode-session--extract-agent-mentions input))
         (mentioned-agent (car agent-names)))
    ;; Create a synthetic user message in the buffer
    (let* ((msg-id (format "user-%s-%d" session-id
                           (cl-incf opencode-acp--part-counter)))
           (part-id (format "user-text-%s" msg-id))
           (message (opencode-message-create
                     :id msg-id
                     :session-id session-id
                     :role "user"))
           (part (opencode-message-part-create
                  :id part-id
                  :session-id session-id
                  :message-id msg-id
                  :type "text"
                  :text input)))
      (setf (opencode-message-parts message) (list (cons part-id part)))
      (setf (opencode-message-text message) input)
      (setq opencode-session--messages
            (append opencode-session--messages (list message)))
      (opencode-session--render-message message))
    ;; Mark busy
    (opencode-session--update-status session-id "running")
    ;; Start question polling
    (opencode-session--start-question-polling connection buffer)
    ;; Dispatch: ACP prompt for normal messages, HTTP for @agent mentions
    (if mentioned-agent
        (opencode-session--send-input-http
         connection session-id input mentioned-agent buffer)
      (opencode-session--send-input-acp
       connection session-id input buffer))))

(defun opencode-session--send-input-acp (connection session-id input buffer)
  "Send INPUT via ACP session/prompt for SESSION-ID.
BUFFER is the session buffer.  CONNECTION is the server connection."
  (let* ((acp-parts (let ((p (make-hash-table :test 'equal)))
                      (puthash "type" "text" p)
                      (puthash "text" input p)
                      (vector p)))
         (params (let ((p (make-hash-table :test 'equal)))
                   (puthash "sessionId" session-id p)
                   (puthash "prompt" acp-parts p)
                   p)))
    (opencode-acp-request
     (opencode-connection-process connection)
     "session/prompt"
     params
     :success (lambda (_result)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (opencode-session--stop-question-polling)
                     (opencode-session--stop-all-task-polls)
                     (opencode-acp--finalize-current-message)
                     (opencode-session--update-status session-id "idle")
                     (opencode-session--refresh-session-metadata connection session-id)
                     (opencode-session--stop-command-reconcile))))
     :error (lambda (err)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (opencode-session--stop-question-polling)
                  (opencode-session--stop-all-task-polls)
                  (opencode-session--stop-command-reconcile)
                  (opencode-acp--finalize-current-message)
                  (opencode-session--update-status session-id "idle")
                  (opencode-session--restore-input input)
                  (message "OpenCode: prompt failed: %s"
                           (alist-get 'message err))))))))


(defun opencode-session--send-input-http (connection session-id input agent buffer)
  "Send INPUT via HTTP prompt_async with AGENT for SESSION-ID.
Used for @-agent mentions since ACP doesn't support per-prompt agent
selection.  Streaming updates still arrive via ACP notifications.
Polls session status to detect turn completion.
BUFFER is the session buffer.  CONNECTION is the server connection."
  (let ((parts `[((type . "text") (text . ,input))]))
    (opencode-client-session-prompt-async
     connection
     session-id
     parts
     :agent agent
     :success (lambda (&rest _args)
                ;; Fire-and-forget succeeded — start polling for completion
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (opencode-session--start-status-poll
                     connection session-id buffer))))
     :error (lambda (&rest _args)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (opencode-session--stop-question-polling)
                  (opencode-session--update-status session-id "idle")
                  (opencode-session--restore-input input)
                  (message "OpenCode: failed to send prompt via HTTP")))))))

(defun opencode-session--classify-input (input)
  "Classify INPUT and return (TYPE . PAYLOAD).

TYPE is one of the symbols `command', `shell', or `message'.
PAYLOAD is the text to send: for `command' and `message' it is the
original INPUT; for `shell' the leading \"!\" is stripped."
  (cond
   ((string-prefix-p "/" input) (cons 'command input))
   ((string-prefix-p "!" input) (cons 'shell (substring input 1)))
   (t (cons 'message input))))

(defun opencode-session--send-shell (connection session command)
  "Send shell COMMAND to SESSION using CONNECTION.

Restores the original input (with leading !) when the request fails."
  (let ((session-id (opencode-session-id session))
        (agent opencode-session--agent)
        (model (opencode-session--selected-model)))
    (opencode-client-session-shell
     connection
     session-id
     command
     :agent agent
     :model model
     :success (lambda (&rest _args)
                (message "OpenCode: shell command queued"))
     :error (lambda (&rest _args)
              (opencode-session--restore-input (concat "!" command))
              (message "OpenCode: failed to send shell command")))))

(defun opencode-session--maybe-send-command (connection session input)
  "Send slash command INPUT to SESSION using CONNECTION.

ACP handles slash commands within the prompt method — if the text starts
with /, ACP parses it as a command.  Falls back to a normal prompt when
INPUT does not match an available command."
  (let ((buffer (current-buffer)))
    (opencode-client-commands
     connection
     :success (lambda (&rest _args)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    ;; ACP handles /commands within session/prompt, so
                    ;; we send them as regular prompts either way.
                    (opencode-session--clear-input)
                    (opencode-session--send-input connection session input)
                    ;; Start polling for the expanded command text.
                    ;; The server creates the user message with expanded
                    ;; text immediately; we poll until we see it differ
                    ;; from the raw /command text.
                    (opencode-session--start-command-reconcile
                     connection
                     (opencode-session-id session)
                     input buffer))))
     :error (lambda (&rest _args)
              (error "Failed to fetch OpenCode commands")))))

;;; Buffer naming and registration

(defun opencode-session--buffer-name (session)
  "Return a buffer name for SESSION."
  (let ((title (string-trim (or (opencode-session-title session) ""))))
    (format "*OpenCode: %s*"
            (if (string-empty-p title)
                (or (opencode-session-slug session)
                    (opencode-session-id session)
                    "session")
              title))))

(defun opencode-session--register-buffer (session buffer)
  "Register BUFFER for SESSION."
  (when-let ((session-id (opencode-session-id session)))
    (puthash session-id buffer opencode-session--buffers)
    (opencode-session--maybe-start-spinner)))

(defun opencode-session--rename-buffer (previous-name)
  "Rename the current buffer when session metadata changes.

PREVIOUS-NAME is the previous buffer name to compare against."
  (when opencode-session--session
    (let ((new-name (opencode-session--buffer-name opencode-session--session)))
      (when (and previous-name
                 (not (string= previous-name new-name))
                 (string= (buffer-name) previous-name))
        (rename-buffer new-name t)))))

;;; Rendering orchestration

(defun opencode-session--render-buffer ()
  "Render the session buffer contents."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (opencode-session--ensure-markers)
  (opencode-session--render-header)
  (opencode-session--render-messages)
  (opencode-session--ensure-input-region))

;;; Message state management

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
            (append opencode-session--messages (list message))))
    (when message
      (opencode-session--adopt-model-from-message message)
      (opencode-session--render-header))))

(defun opencode-session--adopt-model-from-message (message)
  "Adopt provider/model from MESSAGE for header display."
  (when (and (opencode-message-p message)
             (stringp (opencode-message-provider-id message))
             (stringp (opencode-message-model-id message))
             (not (string-empty-p (opencode-message-provider-id message)))
             (not (string-empty-p (opencode-message-model-id message))))
    (setq-local opencode-session--provider-id (opencode-message-provider-id message))
    (setq-local opencode-session--model-id (opencode-message-model-id message))
    (opencode-session--sync-variant-selection)))

(defun opencode-session--update-message (message info)
  "Update MESSAGE fields from INFO."
  (let* ((time (alist-get 'time info))
         (model (alist-get 'model info))
         (created (alist-get 'created time))
         (completed (alist-get 'completed time))
         (provider-id (or (alist-get 'providerID info)
                          (alist-get 'providerID model)))
         (model-id (or (alist-get 'modelID info)
                       (alist-get 'modelID model))))
    (setf (opencode-message-session-id message) (alist-get 'sessionID info))
    (setf (opencode-message-role message) (alist-get 'role info))
    (setf (opencode-message-parent-id message) (alist-get 'parentID info))
    (setf (opencode-message-model-id message) model-id)
    (setf (opencode-message-provider-id message) provider-id)
    (setf (opencode-message-mode message) (alist-get 'mode info))
    (setf (opencode-message-agent message) (alist-get 'agent info))
    (setf (opencode-message-path message) (alist-get 'path info))
    (setf (opencode-message-time-created message) created)
    (setf (opencode-message-time-completed message) completed)
    (setf (opencode-message-finish message) (alist-get 'finish info))
    (setf (opencode-message-error message) (alist-get 'error info))
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
         (session-id (alist-get 'sessionID part))
         (message (opencode-session--find-message message-id)))
    ;; Register subagent mapping when we see a task tool with a sessionId
    (opencode-session--maybe-register-subagent part session-id)
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
     :synthetic (alist-get 'synthetic info)
     :ignored (alist-get 'ignored info)
     :time-start start
     :time-end end
     :snapshot (alist-get 'snapshot info)
     :reason (alist-get 'reason info)
     :cost (alist-get 'cost info)
     :tokens (alist-get 'tokens info)
     :tool (alist-get 'tool info)
     :state (alist-get 'state info))))

;;; Session state management

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

(defun opencode-session--update-status (session-id status)
  "Update STATUS for SESSION-ID."
  (when-let ((buffer (opencode-session--buffer-for-session session-id)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when opencode-session--session
          (setf (opencode-session-status opencode-session--session) status)
          (opencode-session--render-header)
          (opencode-session--maybe-start-spinner)
          (opencode-session--maybe-stop-spinner))))))

(defun opencode-session--refresh-session-metadata (connection session-id)
  "Fetch session metadata via HTTP and update the buffer.
Uses the embedded HTTP server to get the canonical session data
including title, timestamps, etc."
  (opencode-client-session-get
   connection
   session-id
   :success (lambda (&rest args)
              (let ((data (plist-get args :data)))
                (when-let ((buffer (opencode-session--buffer-for-session session-id)))
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (opencode-session--update-session data))))))
   :error (lambda (&rest _args)
            ;; Non-fatal — just means we don't get the title update
            nil)))

(defun opencode-session--start-command-reconcile (connection session-id raw-input buffer)
  "Start polling for the expanded slash command user message.
CONNECTION is the server connection.  SESSION-ID is the session.
RAW-INPUT is the original /command text.  BUFFER is the session buffer.
Polls every 500ms until the server's user message differs from RAW-INPUT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (opencode-session--stop-command-reconcile)
      (setq opencode-session--command-reconcile-timer
            (run-at-time 0.5 0.5
                         #'opencode-session--poll-command-reconcile
                         connection session-id raw-input buffer)))))

(defun opencode-session--stop-command-reconcile ()
  "Stop the command reconcile polling timer."
  (when opencode-session--command-reconcile-timer
    (cancel-timer opencode-session--command-reconcile-timer)
    (setq opencode-session--command-reconcile-timer nil)))

(defun opencode-session--poll-command-reconcile (connection session-id raw-input buffer)
  "Poll for the expanded user message and update BUFFER when found.
Fetches the latest message from SESSION-ID via CONNECTION.  If the
server's user message text differs from RAW-INPUT, update the synthetic
message and stop polling."
  (if (not (and (buffer-live-p buffer)
                (opencode-connection-alive-p connection)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--stop-command-reconcile)))
  (opencode-client-session-messages
   connection
   session-id
   :limit 2
   :success (lambda (&rest args)
              (let* ((data (plist-get args :data))
                     (items (cond
                              ((listp data) data)
                              ((vectorp data) (append data nil))
                              (t nil)))
                     ;; Find the most recent user message from the server
                     (user-item (cl-find-if
                                 (lambda (item)
                                   (let ((info (alist-get 'info item)))
                                     (string= (alist-get 'role info) "user")))
                                 (reverse items))))
                (when (and user-item (buffer-live-p buffer))
                  (let* ((info (alist-get 'info user-item))
                         (parts-data (alist-get 'parts user-item))
                         ;; Extract the text from the server's user message
                         (server-parts (opencode-session--normalize-items parts-data))
                         (server-text
                          (mapconcat
                           (lambda (p)
                             (or (alist-get 'text p) ""))
                           (seq-filter
                            (lambda (p) (string= (alist-get 'type p) "text"))
                            server-parts)
                           "")))
                    ;; Only update if the text differs from the raw command
                    (when (and (not (string-empty-p server-text))
                               (not (string= (string-trim server-text)
                                             (string-trim raw-input))))
                      (with-current-buffer buffer
                        ;; Stop polling — we found the expanded text
                        (opencode-session--stop-command-reconcile)
                        ;; Find the last synthetic user message
                        (when-let ((synthetic-msg
                                    (cl-find-if
                                     (lambda (msg)
                                       (and (string= (opencode-message-role msg) "user")
                                            (string-prefix-p "user-"
                                                             (opencode-message-id msg))))
                                     (reverse opencode-session--messages))))
                          ;; Replace its parts and text with the server's version
                          (let ((new-parts (opencode-session--hydrate-parts parts-data)))
                            (setf (opencode-message-parts synthetic-msg) new-parts)
                            (setf (opencode-message-text synthetic-msg)
                                  (opencode-session--message-text synthetic-msg))
                            (opencode-session--render-message synthetic-msg)))))))))
   :error (lambda (&rest _args) nil))))

;;; Command completion

(defun opencode-session--command-items (data)
  "Normalize command list DATA into a list."
  (cond
   ((vectorp data) (append data nil))
   ((listp data) data)
   (t nil)))

(defun opencode-session--command-names (items)
  "Return command names for ITEMS."
  (delq nil (mapcar (lambda (item)
                      (when (listp item)
                        (alist-get 'name item)))
                    items)))

(defun opencode-session--completion-in-input-p ()
  "Return non-nil when point is within the session input region."
  (when (and opencode-session--input-start-marker
             opencode-session--input-marker)
    (let ((start (marker-position opencode-session--input-start-marker))
          (end (marker-position opencode-session--input-marker))
          (pos (point)))
      (and (<= start pos) (<= pos end)))))

(defun opencode-session--completion-at-point ()
  "Return completion data for the session input area."
  (when (opencode-session--completion-in-input-p)
    (let ((providers opencode-session-completion-providers)
          (result nil))
      (while (and providers (not result))
        (setq result (funcall (car providers)))
        (setq providers (cdr providers)))
      result)))

(defun opencode-session--command-completion-bounds ()
  "Return bounds for a leading slash command completion.

Returns a cons cell (START . END) or nil when the input is not a slash command."
  (when (and opencode-session--input-start-marker
             opencode-session--input-marker)
    (let ((start (marker-position opencode-session--input-start-marker))
          (end (marker-position opencode-session--input-marker))
          (pos (point)))
      (when (and (<= start pos) (<= pos end))
        (save-excursion
          (goto-char start)
          (skip-chars-forward " \t" end)
          (when (and (< (point) end) (eq (char-after) ?/))
            (forward-char 1)
            (let ((command-start (point)))
              (skip-chars-forward "^ \t\n" end)
              (let ((command-end (point)))
                (when (and (<= command-start pos) (<= pos command-end))
                  (cons command-start command-end))))))))))

(defun opencode-session--fetch-commands (connection)
  "Fetch and cache available commands for CONNECTION."
  (let ((session-buffer (current-buffer)))
    (opencode-connection-ensure-commands
     connection
     (lambda (_items)
       (when (buffer-live-p session-buffer)
         (with-current-buffer session-buffer
           (when (eq opencode-session--connection connection)
             (completion-at-point))))))))

(defun opencode-session--complete-command ()
  "Return completion data for leading slash commands."
  (when-let ((bounds (opencode-session--command-completion-bounds)))
    (let* ((start (car bounds))
           (end (cdr bounds))
           (connection opencode-session--connection))
      (when connection
        (let ((commands (opencode-connection-commands connection)))
          (cond
           ((eq commands :loading) nil)
           ((null commands)
            (opencode-session--fetch-commands connection)
            (message "OpenCode: loading commands")
            nil)
           (t
            (let* ((items (opencode-session--command-items commands))
                   (names (opencode-session--command-names items)))
              (when (and names (listp names))
                (list start end names
                      :exclusive 'no
                      :company-prefix-length 0))))))))))

;;; Agent @-mention completion

(defun opencode-session--agent-completion-bounds ()
  "Return bounds for an @-agent completion.

Returns a cons cell (START . END) where START is the position after
the `@' trigger and END is the end of the partial agent name, or nil
when point is not in a valid @-mention context.  The `@' must be
preceded by whitespace or the start of the input region."
  (when (and opencode-session--input-start-marker
             opencode-session--input-marker)
    (let ((input-start (marker-position opencode-session--input-start-marker))
          (input-end (marker-position opencode-session--input-marker))
          (pos (point)))
      (when (and (<= input-start pos) (<= pos input-end))
        (save-excursion
          ;; Scan backward from point for the nearest `@'
          (let ((scan pos)
                (found nil))
            (while (and (> scan input-start) (not found))
              (setq scan (1- scan))
              (let ((ch (char-after scan)))
                (cond
                 ;; Hit whitespace before finding `@' — no valid trigger
                 ((memq ch '(?\s ?\t ?\n))
                  (setq scan input-start)) ; stop scanning
                 ;; Found `@'
                 ((eq ch ?@)
                  ;; Verify the character before `@' is whitespace or start
                  (let ((before-at (1- scan)))
                    (when (or (<= scan input-start)
                              (memq (char-after before-at) '(?\s ?\t ?\n)))
                      (setq found scan)))))))
            (when found
              (cons (1+ found) pos))))))))

(defun opencode-session--fetch-completable-agents (connection)
  "Fetch and cache agents for CONNECTION, then re-trigger completion."
  (let ((session-buffer (current-buffer)))
    (opencode-client-agents
     connection
     :success (lambda (&rest args)
                (let* ((data (plist-get args :data))
                       (raw (opencode-session--normalize-agent-data data))
                       (agents (opencode-session--normalize-agents data)))
                  (setf (opencode-connection-agents connection) agents)
                  (setf (opencode-connection-agents-raw connection) raw)
                  (when (buffer-live-p session-buffer)
                    (with-current-buffer session-buffer
                      (opencode-session--apply-default-agent connection)
                      (completion-at-point)))))
     :error (lambda (&rest _args)
              (message "OpenCode: failed to load agents")))))

(defun opencode-session--complete-agent ()
  "Return completion data for @-agent mentions."
  (when-let ((bounds (opencode-session--agent-completion-bounds)))
    (let* ((start (car bounds))
           (end (cdr bounds))
           (connection opencode-session--connection))
      (when connection
        (let ((raw (opencode-connection-agents-raw connection)))
          (cond
           ;; Agents not cached yet — trigger a fetch
           ((null raw)
            (opencode-session--fetch-completable-agents connection)
            (message "OpenCode: loading agents")
            nil)
           ;; Agents available — return completion candidates
           (t
            (let ((names (opencode-session--completable-agent-names raw)))
              (when names
                (list start end names
                      :exclusive 'no
                      :company-prefix-length 0))))))))))

(defun opencode-session--parse-command-input (input)
  "Return (COMMAND ARGUMENTS) parsed from INPUT.

COMMAND is nil when INPUT is not a slash command."
  (if (and (string-prefix-p "/" input)
           (string-match "^/\\([^ ]+\\)\\(?: \\(.*\\)\\)?$" input))
      (let ((command (match-string 1 input))
            (arguments (or (match-string 2 input) "")))
        (list command arguments))
    (list nil "")))

;;; Session status polling (for HTTP-initiated prompts)

(declare-function opencode-client-session-status "emacs-opencode-client")

(defun opencode-session--start-status-poll (connection session-id buffer)
  "Start polling session status for SESSION-ID on CONNECTION.
When the session becomes idle, finalize the turn in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (opencode-session--stop-status-poll)
      (setq opencode-session--poll-failure-count 0)
      (setq opencode-session--status-poll-timer
            (run-at-time 1 1
                         #'opencode-session--poll-status
                         connection session-id buffer)))))

(defun opencode-session--stop-status-poll ()
  "Stop session status polling."
  (when opencode-session--status-poll-timer
    (cancel-timer opencode-session--status-poll-timer)
    (setq opencode-session--status-poll-timer nil)))

(defun opencode-session--poll-status (connection session-id buffer)
  "Check if SESSION-ID is idle on CONNECTION and finalize the turn in BUFFER."
  (if (not (and (buffer-live-p buffer)
                (opencode-connection-alive-p connection)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--stop-status-poll)))
  (opencode-client-session-status
   connection
   :success (lambda (&rest args)
              (opencode-session--poll-record-success buffer)
              (let* ((data (plist-get args :data))
                     ;; Session IDs become symbols after JSON parsing
                     (status-entry (or (alist-get (intern session-id) data)
                                       (alist-get session-id data nil nil #'string=)))
                     (status-type (alist-get 'type status-entry)))
                ;; Session is idle when its entry is absent or type is "idle"
                (when (or (null status-entry)
                          (string= status-type "idle"))
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (opencode-session--stop-status-poll)
                      (opencode-session--stop-question-polling)
                      (opencode-session--stop-all-task-polls)
                      (opencode-acp--finalize-current-message)
                      (opencode-session--update-status session-id "idle")
                      (opencode-session--refresh-session-metadata
                       connection session-id)
                      (opencode-session--stop-command-reconcile))))))
   :error (lambda (&rest _args)
            (opencode-session--poll-record-failure buffer)))))

;;; Sub-agent tool call polling
;;
;; When a task tool call starts, we poll to discover the sub-agent
;; session ID (Layer 1), then poll the sub-agent's messages for tool
;; calls (Layer 2).  This feeds into the existing subagent tracking
;; infrastructure which renders tool calls under the parent task part.

(defun opencode-session--start-task-poll (connection parent-session-id tool-call-id)
  "Start polling to discover and track a sub-agent for TOOL-CALL-ID.
CONNECTION is the server connection.  PARENT-SESSION-ID is the parent
session.  Begins with Layer 1 (discover sub-agent session ID)."
  ;; Don't double-poll for the same tool call
  (unless (assoc tool-call-id opencode-session--task-poll-timers)
    (let* ((buffer (current-buffer))
           (timer (run-at-time
                   1 1
                   #'opencode-session--poll-task-discovery
                   connection parent-session-id tool-call-id buffer)))
      (push (cons tool-call-id timer) opencode-session--task-poll-timers))))

(defun opencode-session--stop-task-poll (tool-call-id)
  "Stop the sub-agent poll for TOOL-CALL-ID."
  (when-let ((entry (assoc tool-call-id opencode-session--task-poll-timers)))
    (cancel-timer (cdr entry))
    (setq opencode-session--task-poll-timers
          (assoc-delete-all tool-call-id opencode-session--task-poll-timers))))

(defun opencode-session--stop-all-task-polls ()
  "Stop all active sub-agent poll timers."
  (dolist (entry opencode-session--task-poll-timers)
    (cancel-timer (cdr entry)))
  (setq opencode-session--task-poll-timers nil))

(defun opencode-session--poll-task-discovery
    (connection parent-session-id tool-call-id buffer)
  "Layer 1: Poll parent session messages to find the sub-agent session ID.
Scans the latest messages in PARENT-SESSION-ID for a task tool part
with callID matching TOOL-CALL-ID.  When the sub-agent session ID is
found in the part's metadata, registers the subagent mapping and
transitions to Layer 2 polling."
  (if (not (and (buffer-live-p buffer)
                (opencode-connection-alive-p connection)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--stop-task-poll tool-call-id)))
    (opencode-client-session-messages
     connection
     parent-session-id
     :limit 2
     :success
     (lambda (&rest args)
       (opencode-session--poll-record-success buffer)
       (let* ((data (plist-get args :data))
              (items (opencode-session--normalize-items data)))
         (catch 'found
           (dolist (item items)
             (let ((parts (opencode-session--normalize-items
                           (alist-get 'parts item))))
               (dolist (part parts)
                 (when (and (string= (alist-get 'type part) "tool")
                            (string= (alist-get 'tool part) "task")
                            (equal (alist-get 'callID part) tool-call-id))
                   (let* ((state (alist-get 'state part))
                          (metadata (alist-get 'metadata state))
                          (sub-session-id (alist-get 'sessionId metadata))
                           ;; Use callID (not id) to match the ACP-created
                           ;; message part key (which is the toolCallId)
                           (part-id (alist-get 'callID part)))
                     (when sub-session-id
                       (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            ;; Register the subagent mapping
                            (opencode-session--register-subagent
                             sub-session-id parent-session-id part-id)
                            ;; Update the part's state metadata so the
                            ;; renderer can find the subagent session ID
                            (when-let ((msg (opencode-session--find-message-by-part part-id)))
                              (when-let ((pentry (assoc part-id (opencode-message-parts msg))))
                                (let ((prt (cdr pentry)))
                                  (when (opencode-message-part-p prt)
                                    (let ((st (or (opencode-message-part-state prt) '())))
                                      (setf (alist-get 'metadata st)
                                            `((sessionId . ,sub-session-id)))
                                      (setf (opencode-message-part-state prt) st))))))
                            ;; Stop Layer 1, start Layer 2
                            (opencode-session--stop-task-poll tool-call-id)
                           (let ((timer
                                  (run-at-time
                                   0 1
                                   #'opencode-session--poll-subagent-tools
                                   connection sub-session-id buffer)))
                             (push (cons tool-call-id timer)
                                   opencode-session--task-poll-timers))))
                        (throw 'found t))))))))))
     :error (lambda (&rest _args)
              (opencode-session--poll-record-failure buffer)))))

(defun opencode-session--poll-subagent-tools
    (connection subagent-session-id buffer)
  "Layer 2: Poll sub-agent messages and feed tool parts into tracking.
Fetches messages from SUBAGENT-SESSION-ID and updates the subagent tool
tracking data, then re-renders the parent task part in BUFFER."
  (if (not (and (buffer-live-p buffer)
                (opencode-connection-alive-p connection)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          ;; Find and stop the timer for this subagent
          (dolist (entry opencode-session--task-poll-timers)
            (when (timerp (cdr entry))
              ;; We can't easily match by subagent-session-id since the
              ;; key is tool-call-id, so just let the guard above handle it
              nil))))
    (opencode-client-session-messages
     connection
     subagent-session-id
     :success
     (lambda (&rest args)
       (opencode-session--poll-record-success buffer)
       (let* ((data (plist-get args :data))
              (items (opencode-session--normalize-items data))
              (updated nil))
         ;; Scan all messages for tool parts
         (dolist (item items)
           (let ((parts (opencode-session--normalize-items
                         (alist-get 'parts item))))
             (dolist (part parts)
               (when (string= (alist-get 'type part) "tool")
                 (let ((part-id (alist-get 'id part))
                       (tool (alist-get 'tool part))
                       (state (alist-get 'state part)))
                   (opencode-session--update-subagent-tool
                    subagent-session-id part-id tool state)
                   (setq updated t))))))
         ;; Re-render the parent task part if we found any tools
         (when (and updated (buffer-live-p buffer))
           (with-current-buffer buffer
             (when-let ((parent-info
                         (opencode-session--subagent-parent
                          subagent-session-id)))
               (opencode-session--rerender-task-part parent-info))))))
     :error (lambda (&rest _args)
              (opencode-session--poll-record-failure buffer)))))

;;; History loading

(defun opencode-session--load-history (connection session buffer &optional on-history-loaded)
  "Load existing messages for SESSION using CONNECTION into BUFFER.

Call ON-HISTORY-LOADED with BUFFER after the request completes."
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
                    (opencode-session--render-buffer))
                  (when on-history-loaded
                    (funcall on-history-loaded buffer)))))
   :error (lambda (&rest _args)
            (message "OpenCode: failed to load session history")
            (when on-history-loaded
              (funcall on-history-loaded buffer)))))

(defun opencode-session--hydrate-message (item)
  "Add a message ITEM returned from the API."
  (let* ((info (alist-get 'info item))
         (parts (alist-get 'parts item))
         (session-id (alist-get 'sessionID info))
         (message (opencode-session--message-from-info info)))
    (when message
      (setf (opencode-message-parts message)
            (opencode-session--hydrate-parts parts))
      ;; Register subagent mappings for any task tool parts
      (dolist (raw-part (opencode-session--normalize-items parts))
        (opencode-session--maybe-register-subagent raw-part session-id))
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

;;; Question and permission polling
;;
;; During prompt execution, we poll the HTTP server for pending questions
;; and permissions.  Questions are an HTTP-only feature (ACP doesn't
;; handle them).  Permissions from the main session are handled via the
;; ACP `session/request_permission' handler, but sub-agent permissions
;; are dropped by ACP (it only tracks sessions it created).  The poll
;; catches those sub-agent permissions.

(defvar-local opencode-session--seen-questions nil
  "Set of question IDs already prompted during this session.")

(defvar-local opencode-session--seen-permissions nil
  "Set of permission IDs already prompted during this session.")

(defun opencode-session--start-question-polling (connection buffer)
  "Start polling for pending questions and permissions on CONNECTION.
BUFFER is the session buffer where prompts will appear."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (opencode-session--stop-question-polling)
      (setq opencode-session--poll-failure-count 0)
      (setq opencode-session--question-poll-timer
            (run-at-time 1 1
                         #'opencode-session--poll-pending
                         connection buffer)))))

(defun opencode-session--stop-question-polling ()
  "Stop question and permission polling in the current buffer."
  (when opencode-session--question-poll-timer
    (cancel-timer opencode-session--question-poll-timer)
    (setq opencode-session--question-poll-timer nil)))

(defun opencode-session--poll-pending (connection buffer)
  "Poll for pending questions and permissions on CONNECTION.
Prompts the user in BUFFER."
  (if (not (and (buffer-live-p buffer)
                (opencode-connection-alive-p connection)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--stop-question-polling)))
    (opencode-session--poll-questions connection buffer)
    (opencode-session--poll-permissions connection buffer)))

(defun opencode-session--poll-record-failure (buffer)
  "Record a polling failure in BUFFER and stop polling if limit reached."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cl-incf opencode-session--poll-failure-count)
      (when (>= opencode-session--poll-failure-count
               opencode-session--poll-failure-limit)
        (opencode-session--stop-question-polling)
        (opencode-session--stop-status-poll)
        (opencode-session--stop-all-task-polls)
        (message "OpenCode: polling stopped after %d consecutive failures"
                 opencode-session--poll-failure-limit)))))

(defun opencode-session--poll-record-success (buffer)
  "Reset the polling failure counter in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq opencode-session--poll-failure-count 0))))

(defun opencode-session--poll-questions (connection buffer)
  "Poll for pending questions on CONNECTION and prompt in BUFFER."
  (opencode-client-questions
   connection
   :success (lambda (&rest args)
              (opencode-session--poll-record-success buffer)
              (let ((data (plist-get args :data)))
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (let ((questions (cond
                                      ((vectorp data) (append data nil))
                                      ((listp data) data)
                                      (t nil))))
                      (dolist (question questions)
                        (let ((qid (alist-get 'id question)))
                          (when (and qid
                                     (not (member qid opencode-session--seen-questions)))
                            (push qid opencode-session--seen-questions)
                            ;; Defer to avoid blocking the HTTP callback
                            (run-at-time 0 nil
                                         (lambda ()
                                           (when (buffer-live-p buffer)
                                             (with-current-buffer buffer
                                               (opencode-session--prompt-question
                                                question)))))))))))))
   :error (lambda (&rest _args)
            (opencode-session--poll-record-failure buffer))))

(declare-function opencode-client-permissions "emacs-opencode-client")
(declare-function opencode-client-permission-reply "emacs-opencode-client")

(defun opencode-session--poll-permissions (connection buffer)
  "Poll for pending permissions on CONNECTION and prompt in BUFFER.
Skips permissions already handled by the ACP `session/request_permission'
handler (identified by matching tool call IDs)."
  (opencode-client-permissions
   connection
   :success (lambda (&rest args)
              (opencode-session--poll-record-success buffer)
              (let ((data (plist-get args :data)))
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (let ((permissions (cond
                                        ((vectorp data) (append data nil))
                                        ((listp data) data)
                                        (t nil))))
                      (dolist (perm permissions)
                        (let* ((pid (alist-get 'id perm))
                               (tool (alist-get 'tool perm))
                               (call-id (alist-get 'callID tool)))
                          ;; Skip if already seen by this poll
                          (when (and pid
                                     (not (member pid opencode-session--seen-permissions)))
                            ;; Skip if already handled by ACP permission handler
                            ;; (dedup via tool call ID).  Only the ACP handler
                            ;; writes to the seen table; the poll only reads.
                            (unless (and call-id
                                         (gethash call-id opencode-acp--seen-permission-ids))
                              (push pid opencode-session--seen-permissions)
                              ;; Defer to avoid blocking the HTTP callback
                              (let ((perm-copy perm))
                                (run-at-time 0 nil
                                             (lambda ()
                                               (when (buffer-live-p buffer)
                                                 (with-current-buffer buffer
                                                   (opencode-session--prompt-polled-permission
                                                    connection perm-copy)))))))))))))))
   :error (lambda (&rest _args)
            (opencode-session--poll-record-failure buffer))))

(defun opencode-session--prompt-polled-permission (connection perm)
  "Prompt the user for permission PERM and reply via HTTP on CONNECTION."
  (let* ((pid (alist-get 'id perm))
         (permission-type (alist-get 'permission perm))
         (metadata (alist-get 'metadata perm))
         (tool (alist-get 'tool perm))
         (call-id (alist-get 'callID tool))
         ;; Build a descriptive prompt
         (filepath (when (listp metadata) (alist-get 'filepath metadata)))
         (prompt (format "OpenCode: %s%s (Allow once, Allow always, Deny): "
                         (or permission-type "permission")
                         (if filepath
                             (format " [%s]" filepath)
                           "")))
         (choices '("Allow once" "Allow always" "Deny"))
         (selection (condition-case nil
                        (completing-read prompt choices nil t)
                      (quit "Deny")))
         (reply (cond
                 ((string= selection "Allow always") "always")
                 ((string= selection "Allow once") "allow")
                 (t "reject"))))
    ;; Send reply via HTTP
    (opencode-client-permission-reply
     connection
     pid
     reply
     :success (lambda (&rest _args)
                (message "OpenCode: permission %s — %s" pid reply))
     :error (lambda (&rest _args)
              (message "OpenCode: failed to reply to permission %s" pid)))))

(provide 'emacs-opencode-session-mode)

;;; emacs-opencode-session-mode.el ends here
