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
connection, and then call CALLBACK with the new connection."
  (if (and opencode-session--connection
           (opencode-connection-alive-p opencode-session--connection))
      (funcall callback opencode-session--connection)
    (let ((directory (or (and opencode-session--session
                              (opencode-session-directory opencode-session--session))
                         (and opencode-session--connection
                              (opencode-connection-directory
                               opencode-session--connection))))
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
           (let ((session-id (opencode-session-id opencode-session--session))
                 (model (opencode-session--active-model)))
             (unless session-id
               (error "OpenCode session ID is missing"))
             (unless model
               (error "OpenCode session has no selected model"))
             (opencode-client-session-compact
              connection
              session-id
              model
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
             (opencode-client-session-abort
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
  "Send INPUT to SESSION using CONNECTION.

Parses @-agent mentions from INPUT and includes them as agent parts
alongside the text part.  Restores INPUT when the request fails."
  (let ((session-id (opencode-session-id session))
        (parts (opencode-session--build-message-parts input))
        (agent opencode-session--agent)
        (model (opencode-session--selected-model))
        (variant opencode-session--variant))
    (opencode-client-session-prompt-async
     connection
     session-id
     parts
     :agent agent
     :variant variant
     :model model
     :success (lambda (&rest _args)
                (message "OpenCode: message queued"))
     :error (lambda (&rest _args)
              (opencode-session--restore-input input)
                (message "OpenCode: failed to send message")))))

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
                             :agent opencode-session--agent
                             :variant opencode-session--variant
                             :model (opencode-session--selected-model-string)
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

(defun opencode-session--session-from-info (info)
  "Create a session object from INFO."
  (let* ((time (alist-get 'time info))
         (created (alist-get 'created time))
         (updated (alist-get 'updated time)))
    (opencode-session-create
     :id (alist-get 'id info)
     :slug (alist-get 'slug info)
     :version (alist-get 'version info)
     :project-id (alist-get 'projectID info)
     :directory (alist-get 'directory info)
     :title (alist-get 'title info)
     :time-created created
     :time-updated updated
     :summary (alist-get 'summary info)
     :info info)))

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
  "Return the message ID to fork before at point, or nil for full-session fork."
  (when-let* ((message (opencode-session--message-at-point)))
    (pcase (opencode-message-role message)
      ("user" (opencode-message-id message))
      ("assistant" (or (opencode-message-parent-id message)
                       (when-let* ((user (opencode-session--previous-user-message message)))
                         (opencode-message-id user)))))))

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
        (when (and (opencode-message-part-p previous)
                   (member (opencode-message-part-type data)
                           '("text" "reasoning")))
          (setf (opencode-message-part-text data)
                (if delta
                    (concat (opencode-message-part-text previous) delta)
                  (or (opencode-message-part-text data)
                      (opencode-message-part-text previous)))))
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
     :state (alist-get 'state info)
     :auto (alist-get 'auto info)
     :overflow (alist-get 'overflow info)
     :tail-start-id (alist-get 'tail_start_id info))))

(defun opencode-session--upsert-compaction (session-id message-id reason &optional complete)
  "Upsert a compaction marker for SESSION-ID and MESSAGE-ID.

REASON is the server compaction reason.  When COMPLETE is non-nil, mark the
compaction part as completed."
  (let* ((message (or (opencode-session--find-message message-id)
                      (let ((created (opencode-message-create
                                      :id message-id
                                      :session-id session-id
                                      :role "user")))
                        (setq opencode-session--messages
                              (append opencode-session--messages (list created)))
                        created)))
         (part-id (format "%s-compaction" message-id))
         (existing (assoc part-id (opencode-message-parts message)))
         (part (or (cdr existing)
                   (opencode-message-part-create
                    :id part-id
                    :session-id session-id
                    :message-id message-id
                    :type "compaction"))))
    (setf (opencode-message-part-auto part) (string= reason "auto"))
    (when complete
      (setf (opencode-message-part-time-end part) complete))
    (if existing
        (setcdr existing part)
      (setf (opencode-message-parts message)
            (append (opencode-message-parts message) (list (cons part-id part)))))
    (setf (opencode-message-text message)
          (opencode-session--message-text message))
    (opencode-session--render-message message)))

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
  (when-let* ((bounds (opencode-session--agent-completion-bounds)))
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

(provide 'emacs-opencode-session-mode)

;;; emacs-opencode-session-mode.el ends here
