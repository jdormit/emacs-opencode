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
(declare-function opencode-session--set-info "emacs-opencode-session-handlers")

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
    (define-key map (kbd "C-c C-t") #'opencode-session-toggle-reasoning)
    (define-key map (kbd "C-c C-f") #'opencode-session-fork)
    (define-key map (kbd "C-c C-x") #'opencode-session-compact)
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
  (setq-local font-lock-extra-managed-props '(opencode-bold-italic opencode-bold))
  (setq-local buffer-read-only nil)
  (setq-local opencode-session--messages nil)
  (setq-local opencode-session--agent nil)
  (setq-local opencode-session--provider-id nil)
  (setq-local opencode-session--model-id nil)
  (setq-local opencode-session--variant nil)
  (setq-local opencode-session--show-reasoning opencode-session-show-reasoning)
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
      (when (or (opencode-session-directory session)
                (and connection (opencode-connection-directory connection)))
        (setq-local default-directory
                    (file-name-as-directory
                     (or (opencode-session-directory session)
                         (opencode-connection-directory connection)))))
      (opencode-session--register-buffer session buffer)
      (when-let* ((info (opencode-session-info session)))
        (opencode-session--update-session info))
      (when connection
        (opencode-session--ensure-agents connection)
        (opencode-session--refresh-status connection)
        (unless (opencode-session-info session)
          (opencode-session--fetch-info connection buffer)))
      (opencode-session--render-buffer))
    (if (and connection (opencode-session-id session))
        (opencode-session--load-history connection session buffer on-history-loaded)
      (when on-history-loaded
        (funcall on-history-loaded buffer)))
    (pop-to-buffer buffer)
    buffer))

(defun opencode-session--fetch-info (connection buffer)
  "Load the session info for BUFFER's session through CONNECTION.
Sessions opened by ID alone, such as subagent sessions, need their title,
agent, and model."
  (opencode-client-session-get
   connection
   (opencode-session-id (buffer-local-value 'opencode-session--session buffer))
   :success (lambda (&rest args)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (opencode-session--update-session (plist-get args :data)))))
   :error #'ignore))

;;; Connection management

(defun opencode-session--ensure-connection (callback)
  "Ensure the current session buffer has a live connection.

When the connection is alive, call CALLBACK immediately with the
connection.  When the connection is dead or missing, start a new
server for the session directory, update the buffer-local
connection, and then call CALLBACK with the new connection."
  (if (and opencode-session--connection
           (opencode-connection-alive-p opencode-session--connection))
      (funcall callback opencode-session--connection)
    (let ((directory (or (and opencode-session--session
                              (opencode-session-directory opencode-session--session))
                         (and opencode-session--connection
                              (opencode-connection-directory
                               opencode-session--connection))
                         default-directory))
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
             (message "OpenCode: reconnected")
             (funcall callback connection))))))))

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
                   (point-max))
    (goto-char (point-max))
    (insert input))
  (opencode-session--goto-input))

;;;###autoload
(defun opencode-session-toggle-reasoning ()
  "Toggle display of reasoning/thinking blocks in the session buffer.

The toggle is buffer-local: it flips
`opencode-session--show-reasoning' and re-renders the buffer so
existing reasoning blocks appear or disappear immediately.  Typed
input is preserved across the re-render."
  (interactive)
  (unless (derived-mode-p 'opencode-session-mode)
    (error "Not in an OpenCode session buffer"))
  (setq-local opencode-session--show-reasoning
              (not opencode-session--show-reasoning))
  (let ((input (opencode-session--current-input)))
    (opencode-session--render-buffer)
    (unless (string-empty-p input)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert input)))
    (opencode-session--goto-input))
  (message "OpenCode: thinking blocks %s"
           (if opencode-session--show-reasoning "shown" "hidden")))

;;;###autoload
(defun opencode-session-rename ()
  "Rename the current OpenCode session."
  (interactive)
  (unless (derived-mode-p 'opencode-session-mode)
    (error "Not in an OpenCode session buffer"))
  (unless opencode-session--session
    (error "OpenCode session is not connected"))
  (let ((title (read-string "Rename session: "
                            (opencode-session-title opencode-session--session)))
        (buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((session-id (opencode-session-id opencode-session--session)))
             (unless session-id
               (error "OpenCode session ID is missing"))
             (opencode-client-session-rename
              connection
              session-id
              title
              :success (lambda (&rest _args)
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (opencode-session--set-title title)))
                         (message "OpenCode session renamed"))
              :error (lambda (&rest args)
                       (let ((detail (opencode-client-format-error args)))
                         (message "OpenCode: failed to rename session%s"
                                  (if detail (format " (%s)" detail) ""))))))))))))

;;;###autoload
(defun opencode-session-compact ()
  "Compact the current OpenCode session."
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
           (let ((session-id (opencode-session-id opencode-session--session)))
             (unless session-id
               (error "OpenCode session ID is missing"))
             (opencode-client-session-compact
              connection
              session-id
              :success (lambda (&rest _args)
                         (message "OpenCode: compaction queued"))
              :error (lambda (&rest args)
                       (let ((detail (opencode-client-format-error args)))
                         (message "OpenCode: failed to compact session%s"
                                  (if detail (format " (%s)" detail) ""))))))))))))

;;;###autoload
(defun opencode-session-fork ()
  "Fork the current OpenCode session.

When point is in a rendered transcript message, fork before the user message
for that exchange.  When point is in the input area or outside any rendered
message, fork the whole session."
  (interactive)
  (unless (derived-mode-p 'opencode-session-mode)
    (error "Not in an OpenCode session buffer"))
  (unless opencode-session--session
    (error "OpenCode session is not connected"))
  (let ((buffer (current-buffer))
        (source-session opencode-session--session)
        (message-id (opencode-session--fork-message-id-at-point)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (opencode-client-session-fork
          connection
          (opencode-session-id source-session)
          :message-id message-id
          :success (lambda (&rest args)
                     (let* ((data (plist-get args :data))
                            (session (opencode-session--session-from-info data)))
                       (opencode-session-open session connection)
                       (message "OpenCode session forked")))
          :error (lambda (&rest args)
                   (let ((detail (opencode-client-format-error args)))
                     (error "Failed to fork OpenCode session%s"
                            (if detail (format " (%s)" detail) ""))))))))))

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
                  (opencode-session--send-input connection
                                                opencode-session--session
                                                input)
                  (opencode-session--clear-input)
                  (message "OpenCode message submitted")))))))))))


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
           (let ((session-id (opencode-session-id opencode-session--session)))
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
             (opencode-client-session-interrupt
              connection
              session-id
              :success (lambda (&rest _args)
                         (message "OpenCode: interrupt requested"))
              :error (lambda (&rest _args)
                       (message "OpenCode: failed to interrupt session"))))))))))

;;; Input area management

(defun opencode-session--ensure-markers ()
  "Ensure input markers exist."
  (unless opencode-session--input-start-marker
    (setq-local opencode-session--input-start-marker (copy-marker (point-max))))
  (unless opencode-session--input-marker
    ;; Compatibility marker for older call sites.  The authoritative input
    ;; region is `opencode-session--input-start-marker' through `point-max'.
    (setq-local opencode-session--input-marker (copy-marker (point-max) t))))

(defun opencode-session--ensure-input-region ()
  "Ensure the input prompt and compatibility marker are up to date."
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
  (goto-char (point-max)))

(defun opencode-session--maybe-goto-input ()
  "Move point to input when outside the input markers."
  (if (and opencode-session--input-start-marker
           opencode-session--input-marker)
      (let ((start (marker-position opencode-session--input-start-marker))
            (end (point-max)))
        (when (or (< (point) start)
                  (> (point) end))
          (opencode-session--goto-input)))
    (opencode-session--goto-input)))

(defun opencode-session--current-input ()
  "Return current input contents as a string."
  (if opencode-session--input-start-marker
      (buffer-substring-no-properties (marker-position opencode-session--input-start-marker)
                                      (point-max))
    ""))

(defun opencode-session--clear-input ()
  "Clear the input region."
  (let ((inhibit-read-only t))
    (delete-region (marker-position opencode-session--input-start-marker)
                   (point-max))
    (set-marker opencode-session--input-marker (point-max)))
  (opencode-session--goto-input))

(defun opencode-session--restore-input (input)
  "Restore INPUT into the input area."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert input))
  (opencode-session--goto-input))

;;; Sending messages

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

(defun opencode-session--send-input (connection session input)
  "Send INPUT to SESSION using CONNECTION.

Mentioned @-agents are sent alongside the text so the server can delegate
to them.  Restores INPUT when the request fails."
  (opencode-client-session-prompt
   connection
   (opencode-session-id session)
   input
   :agents (opencode-session--extract-agent-mentions input)
   :success (lambda (&rest _args)
              (message "OpenCode: message queued"))
   :error (lambda (&rest _args)
            (opencode-session--restore-input input)
            (message "OpenCode: failed to send message"))))

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
  (let ((session-id (opencode-session-id session)))
    (opencode-client-session-shell
     connection
     session-id
     command
     :success (lambda (&rest _args)
                (message "OpenCode: shell command queued"))
     :error (lambda (&rest _args)
              (opencode-session--restore-input (concat "!" command))
              (message "OpenCode: failed to send shell command")))))

(defun opencode-session--maybe-send-command (connection session input)
  "Send slash command INPUT to SESSION using CONNECTION when applicable.

Falls back to a normal prompt when INPUT does not match an available command."
  (let ((buffer (current-buffer)))
    (opencode-client-commands
     connection
     :success (lambda (&rest args)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (let* ((command-info (opencode-session--parse-command-input input))
                           (command (car command-info))
                           (arguments (cadr command-info))
                           (data (plist-get args :data))
                           (items (opencode-session--command-items data))
                           (names (opencode-session--command-names items))
                           (matched (and command (member command names))))
                      (if matched
                          (progn
                            (opencode-client-session-command
                             connection
                             (opencode-session-id session)
                             command
                             arguments
                             :success (lambda (&rest _args)
                                        (message "OpenCode command queued"))
                             :error (lambda (&rest _args)
                                      (opencode-session--restore-input input)
                                      (message "OpenCode: failed to send command")))
                            (opencode-session--clear-input))
                        (opencode-session--send-input connection session input)
                        (opencode-session--clear-input)
                        (message "OpenCode message submitted"))))))
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

(defun opencode-session--apply-session-info (session info)
  "Copy server session INFO into the SESSION struct."
  (let ((time (alist-get 'time info)))
    (setf (opencode-session-project-id session) (alist-get 'projectID info))
    (setf (opencode-session-directory session)
          (alist-get 'directory (alist-get 'location info)))
    (setf (opencode-session-title session) (alist-get 'title info))
    (setf (opencode-session-time-created session) (alist-get 'created time))
    (setf (opencode-session-time-updated session) (alist-get 'updated time))
    (setf (opencode-session-info session) info)
    session))

(defun opencode-session--session-from-info (info)
  "Create a session object from server session INFO."
  (opencode-session--apply-session-info
   (opencode-session-create :id (alist-get 'id info))
   info))

(defun opencode-session--message-at-point ()
  "Return the rendered message at point, if any."
  (let ((pos (point))
        result)
    (dolist (message opencode-session--messages result)
      (let ((start (opencode-message-start-marker message))
            (end (opencode-message-end-marker message)))
        (when (and (not result)
                   (markerp start)
                   (markerp end)
                   (marker-position start)
                   (marker-position end)
                   (<= (marker-position start) pos)
                   (< pos (marker-position end)))
          (setq result message))))))

(defun opencode-session--previous-user-message (message)
  "Return the nearest user message before MESSAGE."
  (let ((previous nil)
        (done nil))
    (dolist (candidate opencode-session--messages previous)
      (cond
       ((eq candidate message)
        (setq done t))
       ((not done)
        (when (string= (or (opencode-message-role candidate) "") "user")
          (setq previous candidate)))))))

(defun opencode-session--fork-message-id-at-point ()
  "Return the message ID to fork before at point, or nil for full-session fork.
A user prompt or shell command forks before itself; anything else forks
before the user prompt that led to it."
  (when-let* ((message (opencode-session--message-at-point)))
    (if (member (opencode-message-role message) '("user" "shell"))
        (opencode-message-id message)
      (when-let* ((user (opencode-session--previous-user-message message)))
        (opencode-message-id user)))))

(defun opencode-session--register-buffer (session buffer)
  "Register BUFFER for SESSION."
  (when-let* ((session-id (opencode-session-id session)))
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

(defun opencode-session--reset-render-markers ()
  "Reset markers invalidated by a full buffer re-render."
  (when (markerp opencode-session--retry-banner-start)
    (set-marker opencode-session--retry-banner-start nil))
  (when (markerp opencode-session--retry-banner-end)
    (set-marker opencode-session--retry-banner-end nil))
  (setq-local opencode-session--retry-banner-start nil)
  (setq-local opencode-session--retry-banner-end nil)
  (opencode-session--ensure-markers)
  (set-marker opencode-session--input-start-marker (point-min))
  (set-marker opencode-session--input-marker (point-min))
  (dolist (message opencode-session--messages)
    (when (markerp (opencode-message-start-marker message))
      (set-marker (opencode-message-start-marker message) nil))
    (when (markerp (opencode-message-end-marker message))
      (set-marker (opencode-message-end-marker message) nil))
    (setf (opencode-message-start-marker message) nil)
    (setf (opencode-message-end-marker message) nil)))

(defun opencode-session--render-buffer ()
  "Render the session buffer contents."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (opencode-session--reset-render-markers)
  (opencode-session--render-header)
  (opencode-session--render-messages)
  (opencode-session--ensure-input-region)
  (opencode-session--render-retry-banner)
  (font-lock-flush))

;;; Message state management

(defun opencode-session--find-message (message-id)
  "Return the message with MESSAGE-ID, if any."
  (cl-find message-id opencode-session--messages
           :key #'opencode-message-id
           :test #'equal))

(defun opencode-session--content-text (content)
  "Join the text items of tool CONTENT, or return nil when there are none."
  (let ((texts (delq nil (mapcar (lambda (item)
                                   (and (equal (alist-get 'type item) "text")
                                        (alist-get 'text item)))
                                 (opencode-session--normalize-items content)))))
    (when texts
      (apply #'concat texts))))

(defun opencode-session--error-message (error-info)
  "Return the message string of a structured ERROR-INFO."
  (cond
   ((stringp error-info) error-info)
   ((listp error-info) (alist-get 'message error-info))))

(defun opencode-session--tool-state (state)
  "Normalize server tool STATE into the alist the renderer reads.

The renderer expects `status' to be pending, running, completed, or error,
`output' to be text, and `error' to be a message string.  Streaming input
is raw JSON text, so it is dropped until the call is parsed."
  (let* ((status (alist-get 'status state))
         (input (alist-get 'input state))
         (metadata (alist-get 'metadata state))
         (output (opencode-session--content-text (alist-get 'content state)))
         (error-text (opencode-session--error-message (alist-get 'error state))))
    (append `((status . ,(if (equal status "streaming") "pending" status)))
            (when (and input (listp input)) `((input . ,input)))
            (when metadata `((metadata . ,metadata)))
            (when output `((output . ,output)))
            (when error-text `((error . ,error-text))))))

(defun opencode-session--content-part (session-id message-id index item)
  "Return the part for assistant content ITEM at INDEX of MESSAGE-ID.
SESSION-ID owns the message.  Text and reasoning parts are identified by
their position; tool parts by their call ID."
  (let ((id (format "%s:%d" message-id index)))
    (pcase (alist-get 'type item)
      ("text"
       (opencode-message-part-create
        :id id :session-id session-id :message-id message-id
        :type "text" :text (alist-get 'text item)))
      ("reasoning"
       (opencode-message-part-create
        :id id :session-id session-id :message-id message-id
        :type "reasoning" :text (alist-get 'text item)))
      ("tool"
       (opencode-message-part-create
        :id (alist-get 'id item) :session-id session-id :message-id message-id
        :type "tool" :tool (alist-get 'name item)
        :state (opencode-session--tool-state (alist-get 'state item)))))))

(defun opencode-session--shell-state (info)
  "Return renderer tool state for the shell message INFO.
Commands that timed out or were killed render as errors."
  (let ((status (alist-get 'status info))
        (output (alist-get 'output (alist-get 'output info))))
    (append `((status . ,(pcase status
                           ("running" "running")
                           ((or "timeout" "killed") "error")
                           (_ "completed")))
              (input . ((command . ,(alist-get 'command info)))))
            (pcase status
              ("timeout" '((error . "Command timed out")))
              ("killed" '((error . "Command was killed"))))
            (when (alist-get 'exit info)
              `((metadata . ((exit . ,(alist-get 'exit info))))))
            (when (stringp output) `((output . ,output))))))

(defun opencode-session--instruction-paths (info)
  "Return instruction file paths recorded on synthetic message INFO."
  (opencode-session--normalize-items
   (alist-get 'paths (alist-get 'instruction (alist-get 'metadata info)))))

(defun opencode-session--message-parts (info session-id)
  "Return the parts alist for message INFO in SESSION-ID."
  (let* ((id (alist-get 'id info))
         (part (lambda (suffix &rest props)
                 (let ((part-id (format "%s:%s" id suffix)))
                   (cons part-id
                         (apply #'opencode-message-part-create
                                :id part-id :session-id session-id :message-id id
                                props))))))
    (pcase (alist-get 'type info)
      ("user"
       (list (funcall part "text" :type "text" :text (alist-get 'text info))))
      ("assistant"
       (let ((index -1)
             parts)
         (dolist (item (opencode-session--normalize-items (alist-get 'content info)))
           (cl-incf index)
           (when-let* ((content (opencode-session--content-part session-id id index item)))
             (push (cons (opencode-message-part-id content) content) parts)))
         (nreverse parts)))
      ("shell"
       (list (funcall part "shell" :type "tool" :tool "shell"
                      :state (opencode-session--shell-state info))))
      ("compaction"
       (list (funcall part "compaction" :type "compaction"
                      :auto (equal (alist-get 'reason info) "auto")
                      :metadata `((status . ,(alist-get 'status info))
                                  (error . ,(opencode-session--error-message
                                             (alist-get 'error info)))))))
      ("synthetic"
       (list (funcall part "loaded" :type "loaded"
                      :metadata `((paths . ,(opencode-session--instruction-paths info)))))))))

(defun opencode-session--displayed-message-p (info)
  "Return non-nil when message INFO is shown in the transcript.
Bookkeeping messages such as agent switches and idle markers are hidden,
as are synthetic messages other than loaded instruction files."
  (pcase (alist-get 'type info)
    ((or "user" "assistant" "shell" "compaction") t)
    ("synthetic" (and (opencode-session--instruction-paths info) t))))

(defun opencode-session--message-role (type)
  "Return the transcript role for message TYPE."
  (if (equal type "synthetic") "system" type))

(defun opencode-session--message-from-info (info &optional session-id)
  "Create a message object from server message INFO in SESSION-ID.
Returns nil for messages that are not displayed."
  (when (and info (opencode-session--displayed-message-p info))
    (let* ((session-id (or session-id
                           (and opencode-session--session
                                (opencode-session-id opencode-session--session))))
           (message (opencode-message-create
                     :id (alist-get 'id info)
                     :session-id session-id
                     :role (opencode-session--message-role (alist-get 'type info)))))
      (opencode-session--update-message message info)
      (setf (opencode-message-parts message)
            (opencode-session--message-parts info session-id))
      (setf (opencode-message-text message)
            (opencode-session--message-text message))
      message)))

(defun opencode-session--update-message (message info)
  "Update the metadata of MESSAGE from server message INFO."
  (let ((time (alist-get 'time info))
        (model (alist-get 'model info)))
    (setf (opencode-message-agent message) (alist-get 'agent info))
    (setf (opencode-message-provider-id message) (alist-get 'providerID model))
    (setf (opencode-message-model-id message) (alist-get 'id model))
    (setf (opencode-message-time-created message) (alist-get 'created time))
    (setf (opencode-message-time-completed message) (alist-get 'completed time))
    (setf (opencode-message-finish message) (alist-get 'finish info))
    (setf (opencode-message-error message) (alist-get 'error info))
    (setf (opencode-message-info message) info)))

(defun opencode-session--add-message (message)
  "Append MESSAGE to the transcript and render it.
Returns MESSAGE, or the existing message with the same ID."
  (or (opencode-session--find-message (opencode-message-id message))
      (progn
        (setq opencode-session--messages
              (append opencode-session--messages (list message)))
        (opencode-session--adopt-model-from-message message)
        (opencode-session--render-message message)
        message)))

(defun opencode-session--remove-messages (predicate)
  "Remove messages matching PREDICATE and re-render when any were removed."
  (let ((kept (cl-remove-if predicate opencode-session--messages)))
    (unless (= (length kept) (length opencode-session--messages))
      (opencode-session--rerender-preserving-input
       (lambda () (setq opencode-session--messages kept))))))

(defun opencode-session--move-message-to-end (message-id)
  "Move the queued input MESSAGE-ID to the end of the transcript.
A queued prompt is delivered after messages that arrived while it waited.
Inputs that are no longer marked pending already sit in their final place."
  (when-let* ((message (opencode-session--find-message message-id))
              ((alist-get 'pending (opencode-message-info message))))
    (opencode-session--set-info message 'pending nil)
    (unless (eq message (car (last opencode-session--messages)))
      (opencode-session--rerender-preserving-input
       (lambda ()
         (setq opencode-session--messages
               (append (delq message (copy-sequence opencode-session--messages))
                       (list message))))))))

(defun opencode-session--rerender-preserving-input (mutate)
  "Call MUTATE, then re-render the buffer without losing typed input."
  (let ((input (opencode-session--current-input)))
    (funcall mutate)
    (opencode-session--render-buffer)
    (unless (string-empty-p input)
      (opencode-session--restore-input input))))

(defun opencode-session--adopt-model-from-message (message)
  "Adopt provider/model from MESSAGE when the buffer has no model yet."
  (when (and (opencode-message-p message)
             (null opencode-session--model-id)
             (stringp (opencode-message-provider-id message))
             (stringp (opencode-message-model-id message)))
    (setq-local opencode-session--provider-id (opencode-message-provider-id message))
    (setq-local opencode-session--model-id (opencode-message-model-id message))
    (opencode-session--render-header)))

(defun opencode-session--carry-tool-output (previous data)
  "Copy fetched tool output from PREVIOUS onto DATA when DATA lacks it.
Streamed tool events never carry output, so a later update to a completed
tool part must not drop output fetched for an open drawer."
  (let ((old-state (opencode-message-part-state previous))
        (new-state (opencode-message-part-state data)))
    (when (and (string= (or (opencode-message-part-type data) "") "tool")
               (stringp (alist-get 'output old-state))
               (not (alist-get 'output new-state)))
      (setf (opencode-message-part-state data)
            (cons (cons 'output (alist-get 'output old-state)) new-state)))))

;;; Session state management

(defun opencode-session--update-session (info)
  "Update the buffer session from server session INFO."
  (let ((previous-name (and opencode-session--session
                            (opencode-session--buffer-name opencode-session--session)))
        (model (alist-get 'model info)))
    (unless opencode-session--session
      (setq opencode-session--session (opencode-session-create :id (alist-get 'id info))))
    (opencode-session--apply-session-info opencode-session--session info)
    (when-let* ((agent (alist-get 'agent info)))
      (opencode-session--set-agent-from-server agent))
    (when model
      (opencode-session--set-model-from-server model))
    (opencode-session--rename-buffer previous-name)
    (opencode-session--render-header)))

(defun opencode-session--set-title (title)
  "Set the buffer session TITLE and rename the buffer to match."
  (when opencode-session--session
    (let ((previous-name (opencode-session--buffer-name opencode-session--session)))
      (setf (opencode-session-title opencode-session--session) title)
      (setf (alist-get 'title (opencode-session-info opencode-session--session)) title)
      (opencode-session--rename-buffer previous-name)
      (opencode-session--render-header))))

(defun opencode-session--update-status (session-id status)
  "Update STATUS for SESSION-ID.

STATUS is an `opencode-status' struct."
  (when-let* ((buffer (opencode-session--buffer-for-session session-id)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when opencode-session--session
          (setf (opencode-session-status opencode-session--session) status)
          (opencode-session--render-header)
          (opencode-session--render-retry-banner)
          (opencode-session--maybe-start-spinner)
          (opencode-session--maybe-stop-spinner))))))

(defun opencode-session--refresh-status (connection)
  "Sync the busy state of buffers using CONNECTION with the server."
  (opencode-client-session-active
   connection
   :success (lambda (&rest args)
              (let ((active (plist-get args :data)))
                (maphash
                 (lambda (session-id buffer)
                   (when (and (buffer-live-p buffer)
                              (eq (buffer-local-value 'opencode-session--connection buffer)
                                  connection))
                     (let ((busy (assq (intern session-id) active)))
                       (opencode-session--update-status
                        session-id
                        (opencode-status-create :type (if busy "busy" "idle"))))))
                 opencode-session--buffers)))
   :error #'ignore))

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
          (end (point-max))
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
          (end (point-max))
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
  (when-let* ((bounds (opencode-session--command-completion-bounds)))
    (let* ((start (car bounds))
           (end (cdr bounds))
           (connection opencode-session--connection))
      (when connection
        (let* ((cache (opencode-connection-context connection))
               (commands (opencode-connection-commands cache)))
          (pcase (opencode-connection-commands-state cache)
           ((pred consp) nil)
           ('nil
            (opencode-session--fetch-commands connection)
            (message "OpenCode: loading commands")
            nil)
           (_
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
          (input-end (point-max))
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
                  (setf (opencode-connection-agents (opencode-connection-context connection)) agents)
                  (setf (opencode-connection-agents-raw (opencode-connection-context connection)) raw)
                  (when (buffer-live-p session-buffer)
                    (with-current-buffer session-buffer
                      (opencode-session--apply-default-agent connection)
                      (completion-at-point)))))
     :error (lambda (&rest _args)
              (message "OpenCode: failed to load agents")))))

(defun opencode-session--complete-agent ()
  "Return completion data for @-agent mentions."
  (when-let* ((bounds (opencode-session--agent-completion-bounds)))
    (let* ((start (car bounds))
           (end (cdr bounds))
           (connection opencode-session--connection))
      (when connection
        (let ((raw (opencode-connection-agents-raw (opencode-connection-context connection))))
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

;;; History loading

(defvar-local opencode-session--history-loads 0
  "Number of history loads in flight for this buffer.
While positive, session events for the buffer are queued instead of
applied, because the loaded snapshot would discard their effects.")

(defvar-local opencode-session--queued-events nil
  "Events that arrived during a history load, newest first.
Each element is (HANDLER EVENT DATA META).")

(defvar opencode-session--replaying nil
  "Non-nil while queued events are replayed onto a loaded history.")

(defun opencode-session--queue-event (handler event data meta)
  "Queue HANDLER's EVENT with DATA and META when a history load is in flight.
Returns non-nil when queued."
  (when (> opencode-session--history-loads 0)
    (push (list handler event data meta) opencode-session--queued-events)
    t))

(defun opencode-session--replay-queued-events ()
  "Apply the events that arrived while history was loading.
Handlers skip changes the loaded snapshot already contains."
  (let ((events (nreverse opencode-session--queued-events))
        (opencode-session--replaying t))
    (setq opencode-session--queued-events nil)
    (dolist (entry events)
      (condition-case-unless-debug err
          (apply (car entry) (cdr entry))
        (error
         (message "OpenCode: error replaying %s: %s" (nth 1 entry)
                  (error-message-string err)))))))

(defun opencode-session--inbox-message (item session-id)
  "Return a message for queued inbox ITEM in SESSION-ID, or nil if hidden.
The message is marked pending until the session receives it."
  (when (member (alist-get 'type item) '("user" "synthetic"))
    (when-let* ((message (opencode-session--message-from-info
                          (append (list (cons 'id (alist-get 'id item))
                                        (cons 'type (alist-get 'type item))
                                        (cons 'pending t)
                                        (cons 'time (alist-get 'time item)))
                                  (alist-get 'payload item))
                          session-id)))
      message)))

(defun opencode-session--load-history (connection session buffer &optional on-history-loaded)
  "Load existing messages for SESSION using CONNECTION into BUFFER.

Queued inputs the session has not received yet are shown after the
messages.  Call ON-HISTORY-LOADED with BUFFER after the request completes."
  (let ((session-id (opencode-session-id session))
        inbox)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (cl-incf opencode-session--history-loads)))
    (cl-labels
        ((finish (messages)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (cl-decf opencode-session--history-loads)
               (when messages
                 (opencode-session--rerender-preserving-input
                  (lambda ()
                    (setq opencode-session--messages (car messages))))
                 (opencode-session--refetch-open-drawers))
               (when (zerop opencode-session--history-loads)
                 (opencode-session--replay-queued-events))))
           (when on-history-loaded
             (funcall on-history-loaded buffer)))
         (fetch-messages ()
           (opencode-client-session-messages
            connection session-id
            :success
            (lambda (&rest args)
              (let* ((messages (delq nil (mapcar #'opencode-session--hydrate-message
                                                 (plist-get args :data))))
                     (pending (cl-remove-if
                               (lambda (message)
                                 (cl-find (opencode-message-id message) messages
                                          :key #'opencode-message-id :test #'equal))
                               (delq nil (mapcar (lambda (item)
                                                   (opencode-session--inbox-message
                                                    item session-id))
                                                 inbox)))))
                (finish (list (append messages pending)))))
            :error (lambda (&rest _args)
                     (message "OpenCode: failed to load session history")
                     (finish nil)))))
      ;; Read the inbox first: an input delivered between the two reads then
      ;; appears in both, rather than in neither.
      (opencode-client-session-inbox
       connection session-id
       :success (lambda (&rest args)
                  (setq inbox (plist-get args :data))
                  (fetch-messages))
       :error (lambda (&rest _args) (fetch-messages))))))

(defun opencode-session--refetch-open-drawers ()
  "Fetch output for open tool drawers whose parts were reloaded without it."
  (dolist (message opencode-session--messages)
    (when (cl-some (lambda (entry)
                     (and (opencode-session--tool-drawer-open-p (car entry))
                          (opencode-session--tool-drawer-available-p (cdr entry))
                          (null (opencode-session--tool-output (cdr entry)))))
                   (opencode-message-parts message))
      (opencode-session--fetch-tool-output message nil))))

(defun opencode-session--hydrate-message (info)
  "Return a message object for server message INFO, or nil if hidden.
Registers subagent sessions started by its tool calls."
  (when-let* ((message (opencode-session--message-from-info info)))
    (dolist (entry (opencode-message-parts message))
      (opencode-session--maybe-register-subagent
       (cdr entry) (opencode-message-session-id message)))
    message))

(provide 'emacs-opencode-session-mode)

;;; emacs-opencode-session-mode.el ends here
