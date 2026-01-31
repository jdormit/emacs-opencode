;;; emacs-opencode.el --- OpenCode entrypoint  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-client)
(require 'emacs-opencode-session)
(require 'emacs-opencode-session-mode)

(defgroup emacs-opencode nil
  "Emacs client for the OpenCode server."
  :group 'applications)

(defcustom opencode-ready-timeout 5
  "Seconds to wait for OpenCode server readiness."
  :type 'number
  :group 'emacs-opencode)


(defvar opencode--connections (make-hash-table :test 'equal)
  "Registry mapping directories to OpenCode connections.")

(defvar opencode--prompt-history nil
  "History list for OpenCode prompts.")

(defun opencode--normalize-directory (directory)
  "Normalize DIRECTORY for registry lookups."
  (file-name-as-directory (expand-file-name directory)))

(defun opencode--get-connection (directory)
  "Return the OpenCode connection for DIRECTORY, if any."
  (gethash (opencode--normalize-directory directory) opencode--connections))

(defun opencode--register-connection (directory connection)
  "Register CONNECTION for DIRECTORY."
  (puthash (opencode--normalize-directory directory) connection opencode--connections))

(defun opencode--unregister-connection (directory)
  "Remove any registered connection for DIRECTORY."
  (remhash (opencode--normalize-directory directory) opencode--connections))

(defun opencode--registered-directories ()
  "Return a list of registered connection directories."
  (let (directories)
    (maphash (lambda (key _value)
               (push key directories))
             opencode--connections)
    (sort directories #'string<)))

(defun opencode--check-health (connection on-success on-error)
  "Check CONNECTION health once using callbacks.

ON-SUCCESS and ON-ERROR are called with request args." 
  (opencode-client-health
   connection
   :success on-success
   :error on-error))

(defun opencode--ready-timeout (directory connection)
  "Handle readiness timeout for DIRECTORY and CONNECTION."
  (opencode-connection-stop connection)
  (error "OpenCode server did not become ready within %ss" opencode-ready-timeout))

(defun opencode--session-from-data (data)
  "Create a session object from DATA."
  (let* ((time (alist-get 'time data))
         (created (alist-get 'created time))
         (updated (alist-get 'updated time)))
    (opencode-session-create
     :id (alist-get 'id data)
     :slug (alist-get 'slug data)
     :version (alist-get 'version data)
     :project-id (alist-get 'projectID data)
     :directory (alist-get 'directory data)
     :title (alist-get 'title data)
     :time-created created
     :time-updated updated
     :summary (alist-get 'summary data)
     :info data)))

(defun opencode--session-label (info &optional include-identifiers)
  "Return a display label for session INFO.

When INCLUDE-IDENTIFIERS is non-nil, include slug and ID." 
  (let ((title (or (alist-get 'title info) "Untitled session"))
        (slug (alist-get 'slug info))
        (session-id (alist-get 'id info)))
    (if include-identifiers
        (concat title
                (when slug (format " (%s)" slug))
                (when session-id (format " [%s]" session-id)))
      title)))

(defun opencode--session-items (data)
  "Normalize session list DATA into a list."
  (cond
   ((vectorp data) (append data nil))
   ((listp data) data)
   (t nil)))

(defun opencode--session-choices (items)
  "Return completion choices for session ITEMS."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (item items)
      (let* ((title (opencode--session-label item))
             (count (1+ (gethash title counts 0))))
        (puthash title count counts)))
    (mapcar (lambda (item)
              (let* ((title (opencode--session-label item))
                     (ambiguous (> (gethash title counts 0) 1))
                     (label (opencode--session-label item ambiguous)))
                (cons label item)))
            items)))

(defun opencode--session-buffer-order ()
  "Return session IDs ordered by most recently visited buffers."
  (let ((buffer-to-id (make-hash-table :test 'eq)))
    (maphash (lambda (session-id buffer)
               (when (buffer-live-p buffer)
                 (puthash buffer session-id buffer-to-id)))
             opencode-session--buffers)
    (delq nil
          (mapcar (lambda (buffer)
                    (gethash buffer buffer-to-id))
                  (buffer-list)))))

(defun opencode--order-sessions-by-buffer (items)
  "Order ITEMS by recent session buffers, keeping server order otherwise."
  (let ((ordered-ids (opencode--session-buffer-order))
        (items-by-id (make-hash-table :test 'equal))
        (ordered-items nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (item items)
      (let ((session-id (alist-get 'id item)))
        (when session-id
          (puthash session-id item items-by-id))))
    (dolist (session-id ordered-ids)
      (when-let ((item (gethash session-id items-by-id)))
        (puthash session-id t seen)
        (push item ordered-items)))
    (setq ordered-items (nreverse ordered-items))
    (dolist (item items)
      (let ((session-id (alist-get 'id item)))
        (unless (and session-id (gethash session-id seen))
          (setq ordered-items (append ordered-items (list item))))))
    ordered-items))

(defun opencode--select-session (connection prompt on-selected)
  "Prompt for a session via CONNECTION using PROMPT.

Call ON-SELECTED with the selected session and session info data."
  (opencode-client-sessions
   connection
   :success (lambda (&rest args)
              (let* ((data (plist-get args :data))
                     (items (opencode--session-items data))
                     (ordered-items (opencode--order-sessions-by-buffer items))
                     (choices (opencode--session-choices ordered-items))
                     (table (lambda (string pred action)
                              (if (eq action 'metadata)
                                  '(metadata (category . opencode-session)
                                             (display-sort-function . identity)
                                             (cycle-sort-function . identity))
                                (complete-with-action action choices string pred))))
                     (selected (completing-read prompt table nil t))
                     (info (cdr (assoc selected choices)))
                     (session (opencode--session-from-data info)))
                (funcall on-selected session info)))
   :error (lambda (&rest _args)
            (error "Failed to fetch OpenCode sessions"))))

(defun opencode--ensure-session-buffer (session connection &optional on-ready)
  "Open or reuse a buffer for SESSION using CONNECTION.

When ON-READY is non-nil, call it with the session buffer once ready."
  (let* ((session-id (opencode-session-id session))
         (existing-buffer (and session-id
                               (opencode-session--buffer-for-session session-id))))
    (if (and existing-buffer (buffer-live-p existing-buffer))
        (progn
          (with-current-buffer existing-buffer
            (setq-local opencode-session--connection connection)
            (when-let ((info (opencode-session-info session)))
              (opencode-session--update-session info))
            (opencode-session--ensure-agents connection))
          (pop-to-buffer existing-buffer)
          (when on-ready
            (funcall on-ready existing-buffer)))
      (opencode-session-open session connection on-ready))))

(defun opencode--project-directory ()
  "Return the current project root directory, if any."
  (when-let ((project (project-current)))
    (project-root project)))

(defun opencode--read-directory (prompt)
  "Read a directory using PROMPT, honoring the current project.

When the current buffer is in a project, use its root as the default
and skip prompting unless a prefix arg is supplied."
  (if current-prefix-arg
      (read-directory-name prompt default-directory nil t)
    (or (opencode--project-directory)
        (read-directory-name prompt default-directory nil t))))

;;;###autoload
(defun opencode-shutdown (directory)
  "Stop OpenCode server for DIRECTORY and remove it from the registry."
  (interactive
   (list
    (completing-read
     "Shutdown OpenCode for directory: "
     (opencode--registered-directories)
     nil
     t)))
  (let* ((normalized (opencode--normalize-directory directory))
         (connection (opencode--get-connection normalized)))
    (unless connection
      (error "No OpenCode connection registered for %s" normalized))
    (opencode-sse-close connection)
    (opencode-connection-stop connection)
    (opencode--unregister-connection normalized)
    (message "Stopped OpenCode server for %s" normalized)))

;;;###autoload
(defun opencode-run-server (directory &optional on-ready)
  "Start or reuse an OpenCode server for DIRECTORY.

When a connection already exists for DIRECTORY, reuse it without restarting
its server process. When ON-READY is non-nil, call it with the connection
once the server is ready."
  (interactive (list (opencode--read-directory "OpenCode directory: ")))
  (let* ((normalized (opencode--normalize-directory directory))
         (existing (opencode--get-connection normalized)))
    (if existing
        (progn
          (message "OpenCode already running for %s" normalized)
          (when on-ready
            (funcall on-ready existing))
          existing)
      (let* ((connection (opencode-connection-create-for-directory normalized))
             (timeout (run-at-time opencode-ready-timeout nil
                                   #'opencode--ready-timeout normalized connection)))
        (opencode-connection-start
         connection
         (lambda (_process)
           (when (timerp timeout)
             (cancel-timer timeout))
           (opencode--check-health
            connection
            (lambda (&rest _args)
              (opencode--register-connection normalized connection)
              (message "Started OpenCode server for %s" normalized)
              (when on-ready
                (funcall on-ready connection)))
            (lambda (&rest _args)
              (error "OpenCode server failed to become healthy for %s" normalized)))
           (opencode-sse-open connection)))
        (opencode--register-connection normalized connection)
        connection))))

;;;###autoload
(defun opencode (directory)
  "Create a new OpenCode session for DIRECTORY and open its buffer."
  (interactive (list (opencode--read-directory "OpenCode directory: ")))
  (let ((normalized (opencode--normalize-directory directory)))
    (opencode-run-server
     normalized
     (lambda (connection)
       (opencode-request
        connection
        'POST
        "/session"
        :data `(("directory" . ,normalized))
        :success (lambda (&rest args)
                   (let* ((data (plist-get args :data))
                          (session (opencode--session-from-data data)))
                     (opencode-session-open session connection)))
        :error (lambda (&rest _args)
                 (error "Failed to create OpenCode session")))))))

;;;###autoload
(defun opencode-ask (directory prompt)
  "Create a new session for DIRECTORY and send PROMPT."
  (interactive
   (list (opencode--read-directory "OpenCode directory: ")
         (read-from-minibuffer "OpenCode prompt: " nil nil nil
                               'opencode--prompt-history)))
  (let ((normalized (opencode--normalize-directory directory)))
    (opencode-run-server
     normalized
     (lambda (connection)
       (opencode-request
        connection
        'POST
        "/session"
        :data `(("directory" . ,normalized))
        :success (lambda (&rest args)
                   (let* ((data (plist-get args :data))
                          (session (opencode--session-from-data data)))
                     (opencode-session-open
                      session
                      connection
                      (lambda (buffer)
                        (with-current-buffer buffer
                          (opencode-session-insert-input prompt)
                          (opencode-session-send-input))))))
        :error (lambda (&rest _args)
                 (error "Failed to create OpenCode session")))))))

(defun opencode--contextual-snippet ()
  "Return contextual buffer text and metadata.

When the region is active, use its contents. Otherwise, return the 10 lines
surrounding point (five before and five after). Insert an inline marker at
point in the snippet. When the buffer visits a file, include file name and
line number metadata. Returns nil when no context can be collected."
  (let* ((has-region (use-region-p))
         (start (if has-region
                    (region-beginning)
                  (save-excursion
                    (forward-line -5)
                    (line-beginning-position))))
         (end (if has-region
                  (region-end)
                (save-excursion
                  (forward-line 5)
                  (line-end-position))))
         (context (buffer-substring-no-properties start end))
         (marker "<<< point >>>")
         (relative (max 0 (min (length context) (- (point) start))))
         (context-with-point (concat (substring context 0 relative)
                                     marker
                                     (substring context relative)))
         (start-line (line-number-at-pos start))
         (end-line (line-number-at-pos end))
         (file (buffer-file-name))
         (file-line (line-number-at-pos (point))))
    (when (string-empty-p (string-trim context))
      (setq context-with-point nil))
    (when context-with-point
      (if file
          (format "File: %s\nLines: %d-%d (point %d)\n\nNote: %s marks the point.\n\n%s"
                  (file-name-nondirectory file)
                  start-line
                  end-line
                  file-line
                  marker
                  context-with-point)
        (format "Note: %s marks the point.\n\n%s"
                marker
                context-with-point)))))

;;;###autoload
(defun opencode-ask-contextual (directory prompt)
  "Create a new session for DIRECTORY and send PROMPT with context.

If a region is active, use its contents as context. Otherwise include the 10
lines around point (five before and five after). When the buffer visits a
file, include the file name and relevant line numbers."
  (interactive
   (list (opencode--read-directory "OpenCode directory: ")
         (read-from-minibuffer "OpenCode prompt: " nil nil nil
                               'opencode--prompt-history)))
  (let* ((normalized (opencode--normalize-directory directory))
         (context (opencode--contextual-snippet))
         (final-prompt (if context
                           (format "```\n%s\n```\n\n%s"
                                   context
                                   prompt)
                         prompt)))
    (opencode-ask normalized final-prompt)))

;;;###autoload
(defun opencode-open-session (directory)
  "Prompt for a session in DIRECTORY and open its buffer."
  (interactive (list (opencode--read-directory "OpenCode directory: ")))
  (let ((normalized (opencode--normalize-directory directory)))
    (opencode-run-server
     normalized
     (lambda (connection)
       (opencode--select-session
        connection
        "OpenCode session: "
        (lambda (session _info)
          (opencode--ensure-session-buffer session connection)))))))

;;;###autoload
(defun opencode-send-to-session (directory input)
  "Send INPUT to a selected OpenCode session in DIRECTORY.

When called interactively, prompt for a session then INPUT."
  (interactive (list (opencode--read-directory "OpenCode directory: ") nil))
  (let ((normalized (opencode--normalize-directory directory)))
    (opencode-run-server
     normalized
     (lambda (connection)
       (opencode--select-session
        connection
        "OpenCode session: "
        (lambda (session _info)
          (let ((final-input input))
            (when (null final-input)
              (setq final-input
                    (read-from-minibuffer "OpenCode prompt: " nil nil nil
                                          'opencode--prompt-history)))
            (opencode--ensure-session-buffer
             session
             connection
             (lambda (buffer)
               (with-current-buffer buffer
                 (opencode-session-insert-input final-input)
                 (opencode-session-send-input)))))))))))

;;;###autoload
(defun opencode-send-context-to-session (directory prompt)
  "Send PROMPT with context to a selected OpenCode session in DIRECTORY.

When called interactively, prompt for a session then PROMPT." 
  (interactive (list (opencode--read-directory "OpenCode directory: ") nil))
  (let ((normalized (opencode--normalize-directory directory)))
    (opencode-run-server
     normalized
     (lambda (connection)
       (opencode--select-session
        connection
        "OpenCode session: "
        (lambda (session _info)
          (let* ((input prompt)
                 (context (opencode--contextual-snippet))
                 (final-input (if input
                                  input
                                (read-from-minibuffer "OpenCode prompt: " nil nil nil
                                                      'opencode--prompt-history)))
                 (final-prompt (if context
                                   (format "```\n%s\n```\n\n%s"
                                           context
                                           final-input)
                                 final-input)))
            (opencode--ensure-session-buffer
             session
             connection
             (lambda (buffer)
               (with-current-buffer buffer
                 (opencode-session-insert-input final-prompt)
                 (opencode-session-send-input)))))))))))

(defun opencode-mcp-status ()
  "Display the status OpenCode's current MCP connections."
  (interactive)
  ;; TODO make this a proper UX instead of shelling out
  (async-shell-command (format "%s mcp list" (executable-find opencode-server-command))))

(provide 'emacs-opencode)

;;; emacs-opencode.el ends here
