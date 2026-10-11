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
              :success (lambda (&rest args)
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (opencode-session--update-session
                              (or (plist-get args :data)
                                  (let ((current (opencode-session-info
                                                  opencode-session--session)))
                                    (cons `(title . ,title)
                                          (cl-remove 'title current
                                                     :key #'car)))))))
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

(defun opencode-session--agent-attachments (input)
  "Return agent attachment alists for @-mentions in INPUT."
  (mapcar (lambda (name) `((name . ,name)))
          (opencode-session--extract-agent-mentions input)))

(defun opencode-session--send-input (connection session input)
  "Send INPUT to SESSION using CONNECTION.

Parses @-agent mentions from INPUT and includes them as agent attachments
alongside the prompt text.  Restores INPUT when the request fails."
  (let ((session-id (opencode-session-id session))
        (agents (opencode-session--agent-attachments input)))
    (opencode-client-session-prompt-async
     connection
     session-id
     input
     :agents agents
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
  (dolist (form-state opencode-session--forms)
    (when (markerp (opencode-session--form-state-start-marker form-state))
      (set-marker (opencode-session--form-state-start-marker form-state) nil))
    (when (markerp (opencode-session--form-state-end-marker form-state))
      (set-marker (opencode-session--form-state-end-marker form-state) nil))
    (setf (opencode-session--form-state-start-marker form-state) nil)
    (setf (opencode-session--form-state-end-marker form-state) nil))
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
  (opencode-session--render-forms)
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
  "Update MESSAGE fields from INFO.

INFO is a V2 message alist (or a legacy V1 alist): the V2 message
type doubles as the role, the model nests under `model', and errors
carry `type'/`message'."
  (let* ((time (alist-get 'time info))
         (model (alist-get 'model info))
         (created (or (alist-get 'created time)
                      (alist-get 'started info)))
         (completed (or (alist-get 'completed time)
                        (alist-get 'ended info)))
         (provider-id (or (alist-get 'providerID info)
                          (alist-get 'providerID model)))
         (model-id (or (alist-get 'modelID info)
                       (alist-get 'id model))))
    (setf (opencode-message-session-id message) (alist-get 'sessionID info))
    (setf (opencode-message-role message)
          (or (alist-get 'role info) (alist-get 'type info)))
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
        (when (opencode-message-part-p previous)
          (opencode-session--carry-tool-output previous data))
        (if existing
            (setf (opencode-message-parts message)
                  (cl-subst entry existing (opencode-message-parts message)))
          (setf (opencode-message-parts message)
                (append (opencode-message-parts message) (list entry))))
        (setf (opencode-message-text message)
              (opencode-session--message-text message))
        (opencode-session--render-message message)))))

(defun opencode-session--carry-tool-output (previous data)
  "Copy fetched tool context from PREVIOUS onto DATA when DATA lacks it.
SSE part updates are partial: a later update to a completed tool
part must not drop the `output' fetched for an open drawer, nor the
`input' and `metadata' used to render the tool summary line."
  (let ((old-state (opencode-message-part-state previous))
        (new-state (opencode-message-part-state data)))
    (when (and (string= (or (opencode-message-part-type data) "") "tool")
               (listp old-state)
               (listp new-state))
      (dolist (key '(output input metadata))
        (when (and (alist-get key old-state)
                   (not (alist-get key new-state)))
          (setq new-state (cons (cons key (alist-get key old-state)) new-state))))
      (setf (opencode-message-part-state data) new-state))))

(defun opencode-session--content-text (content)
  "Concatenate the text of V2 CONTENT items into a single string.
CONTENT is a list of V2 content parts shaped as alists with a
`text' field, e.g. `((type . \"text\") (text . \"...\"))'."
  (let* ((items (opencode-session--normalize-items content))
         (texts (mapcar (lambda (item)
                          (if (stringp (alist-get 'text item))
                              (alist-get 'text item)
                            ""))
                        items)))
    (when texts
      (string-join texts ""))))

(defun opencode-session--normalize-part-state (state)
  "Normalize V2 tool STATE into the shape the render layer expects.

V2 completed and error states carry the tool output as `content',
a list of `(type . \"text\") (text . STRING)' alists, while the UI
reads `state.output'.  Returns STATE unchanged when there is no
output to add."
  (when (listp state)
    (let ((output (opencode-session--content-text (alist-get 'content state))))
      (if (and (stringp output) (not (string-empty-p output)))
          (cons (cons 'output output) state)
        state))))

(defun opencode-session--message-part-from-info (info)
  "Create a message part object from INFO.

INFO is a V2 message content item or a legacy V1 part alist.  V2
tool parts carry their name as `name' and their output inside
`state.content'; both are normalized here so streaming updates,
tool output fetches, and history hydration share one mapping."
  (let* ((time (alist-get 'time info))
         (start (or (alist-get 'start time)
                    (alist-get 'created time)
                    (alist-get 'ran time)))
         (end (or (alist-get 'end time)
                  (alist-get 'completed time)))
         (state (opencode-session--normalize-part-state
                 (alist-get 'state info))))
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
     :tool (or (alist-get 'tool info) (alist-get 'name info))
     :state state
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
  "Update the buffer session from INFO.

INFO is a V2 session alist or a legacy V1 alist.  V2 identifies the
session with `sessionID' and nests the directory under
`location.directory'."
  (let* ((time (alist-get 'time info))
         (created (alist-get 'created time))
         (updated (alist-get 'updated time))
         (session-id (or (alist-get 'id info) (alist-get 'sessionID info)))
         (directory (or (alist-get 'directory info)
                        (alist-get 'directory (alist-get 'location info))))
         (previous-name (and opencode-session--session
                             (opencode-session--buffer-name opencode-session--session))))
    (unless opencode-session--session
      (setq opencode-session--session (opencode-session-create :id session-id)))
    (setf (opencode-session-slug opencode-session--session) (alist-get 'slug info))
    (setf (opencode-session-version opencode-session--session) (alist-get 'version info))
    (setf (opencode-session-project-id opencode-session--session) (alist-get 'projectID info))
    (setf (opencode-session-directory opencode-session--session) directory)
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

Pages through the V2 message list oldest-first and hydrates each
message into BUFFER as the pages arrive.  Calls ON-HISTORY-LOADED
with BUFFER after the final page renders, or after an error."
  (cl-labels
      ((fetch-page (cursor first-page)
         (if (not (buffer-live-p buffer))
             (when on-history-loaded
               (funcall on-history-loaded buffer))
           (opencode-client-session-messages
            connection
            (opencode-session-id session)
            :order (unless cursor "asc")
            :cursor cursor
            :limit 100
            :success
            (lambda (&rest args)
              (let* ((body (plist-get args :data))
                     (items (opencode-session--normalize-items
                             (alist-get 'data body)))
                     (next (alist-get 'next (alist-get 'cursor body))))
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (when first-page
                      (setq opencode-session--messages nil))
                    (dolist (item items)
                      (opencode-session--hydrate-message item))
                    (unless next
                      (opencode-session--render-buffer))))
                (if (and next (buffer-live-p buffer))
                    (fetch-page next nil)
                  (opencode-session--sync-forms
                   connection (opencode-session-id session))
                  (when on-history-loaded
                    (funcall on-history-loaded buffer)))))
            :error
            (lambda (&rest _args)
              (message "OpenCode: failed to load session history")
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (opencode-session--render-buffer)))
              (opencode-session--sync-forms
               connection (opencode-session-id session))
              (when on-history-loaded
                (funcall on-history-loaded buffer)))))))
    (fetch-page nil t)))

(defun opencode-session--hydrate-message (item)
  "Add a message ITEM returned from the API to the current buffer.

ITEM is a V2 message alist (role in `type', assistant body in
`content', plain text in `text') or a legacy V1 item shaped as
`(info parts)'.  Returns the hydrated message, or nil for an empty
ITEM."
  (let* ((info (alist-get 'info item))
         (message (opencode-session--message-from-info (or info item))))
    (when message
      (if info
          (opencode-session--hydrate-legacy-message message item)
        (opencode-session--hydrate-v2-message message item))
      (setf (opencode-message-text message)
            (opencode-session--message-text message))
      (setq opencode-session--messages
            (append opencode-session--messages (list message)))
      message)))

(defun opencode-session--hydrate-legacy-message (message item)
  "Hydrate a legacy V1 message ITEM shaped as `(info parts)' into MESSAGE."
  (let* ((info (alist-get 'info item))
         (parts (alist-get 'parts item))
         (session-id (alist-get 'sessionID info)))
    (setf (opencode-message-parts message)
          (opencode-session--hydrate-parts parts))
    ;; Register subagent mappings for any task tool parts.
    (dolist (raw-part (opencode-session--normalize-items parts))
      (opencode-session--maybe-register-subagent raw-part session-id))))

(defun opencode-session--hydrate-v2-message (message item)
  "Populate MESSAGE from V2 message ITEM based on its `type'.

Assistant messages map their `content' array to parts; compaction
messages attach a boundary part; user, synthetic, system, skill and
shell messages carry their body as message text.  Other message
types (idle, agent-switched, ...) render no body."
  (let* ((type (opencode-message-role message))
         (session-id (or (opencode-message-session-id message)
                         (and opencode-session--session
                              (opencode-session-id opencode-session--session)))))
    (pcase type
      ("assistant"
       (let ((content (alist-get 'content item)))
         (setf (opencode-message-parts message)
               (opencode-session--hydrate-content content))
         (dolist (raw (opencode-session--normalize-items content))
           (opencode-session--maybe-register-subagent raw session-id))))
      ("compaction"
       (opencode-session--hydrate-compaction message item session-id))
      ((or "user" "synthetic" "system" "skill")
       (opencode-session--hydrate-text-message message item))
      ("shell"
       (opencode-session--hydrate-shell-message message item))
      (_ nil))))

(defun opencode-session--hydrate-content (content)
  "Hydrate a V2 assistant CONTENT array into an alist of message parts.

Text and reasoning items carry no id in the message list, so they
get synthetic ids mirroring the streaming convention (`text-N' /
`reasoning-N', N being the item's position in CONTENT).  Tool items
are keyed by their tool call id."
  (let ((result nil)
        (index 0))
    (dolist (part (opencode-session--normalize-items content))
      (let* ((part-id (or (alist-get 'id part)
                          (format "%s-%s" (or (alist-get 'type part) "text")
                                  index)))
             (data (opencode-session--message-part-from-info part))
             (existing (assoc part-id result)))
        (setf (opencode-message-part-id data) part-id)
        (if existing
            (setcdr existing data)
          (push (cons part-id data) result))
        (setq index (1+ index))))
    (nreverse result)))

(defun opencode-session--hydrate-compaction (message item session-id)
  "Attach a compaction boundary part to MESSAGE from compaction ITEM.

The part is tagged with SESSION-ID.  Running and completed compactions
render a boundary line; failed compactions are skipped."
  (let ((status (alist-get 'status item)))
    (when (member status '("running" "completed"))
      (let* ((message-id (opencode-message-id message))
             (part-id (format "%s-compaction" message-id))
             (created (alist-get 'created (alist-get 'time item)))
             (part (opencode-message-part-create
                    :id part-id
                    :session-id session-id
                    :message-id message-id
                    :type "compaction"
                    :auto (string= (alist-get 'reason item) "auto")
                    :time-end (unless (string= status "running") created))))
        (setf (opencode-message-parts message)
              (list (cons part-id part)))))))

(defun opencode-session--hydrate-text-message (message item)
  "Set MESSAGE text from a V2 text-bearing ITEM."
  (let ((text (opencode-session--nonempty-string (alist-get 'text item))))
    (when text
      (setf (opencode-message-text message) text))))

(defun opencode-session--hydrate-shell-message (message item)
  "Set MESSAGE text for a V2 shell ITEM as a command/output block."
  (let ((command (alist-get 'command item))
        (output (alist-get 'output (alist-get 'output item))))
    (when (stringp command)
      (setf (opencode-message-text message)
            (concat "$ " command
                    (when (and (stringp output)
                               (not (string-empty-p output)))
                      (concat "\n" output)))))))

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
