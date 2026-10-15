;;; emacs-opencode.el --- OpenCode entrypoint  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-client)
(require 'emacs-opencode-run)
(require 'emacs-opencode-session)
(require 'emacs-opencode-session-mode)

(defgroup emacs-opencode nil
  "Emacs client for the OpenCode server."
  :group 'applications)

(defcustom opencode-session-list-limit 1000
  "Maximum number of sessions to request when listing sessions.
The OpenCode server returns 50 sessions by default; this option raises
that limit for the session-selection completion table.  A value of nil
uses the server default."
  :type '(choice (const :tag "Server default" nil) integer)
  :group 'emacs-opencode)


(defvar opencode--service-connection nil
  "The shared OpenCode service connection.")

(defvar opencode--prompt-history nil
  "History list for OpenCode prompts.")

(defun opencode--normalize-directory (directory)
  "Normalize DIRECTORY for registry lookups."
  (file-name-as-directory (expand-file-name directory)))

(defun opencode--session-from-data (data)
  "Create a session object from server session DATA."
  (opencode-session--session-from-info data))

(defun opencode--session-label (info &optional include-identifiers)
  "Return a display label for session INFO.

When INCLUDE-IDENTIFIERS is non-nil, include the session ID."
  (let ((title (or (alist-get 'title info) "Untitled session"))
        (session-id (alist-get 'id info)))
    (if (and include-identifiers session-id)
        (format "%s [%s]" title session-id)
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
      (when-let* ((item (gethash session-id items-by-id)))
        (puthash session-id t seen)
        (push item ordered-items)))
    (setq ordered-items (nreverse ordered-items))
    (dolist (item items)
      (let ((session-id (alist-get 'id item)))
        (unless (and session-id (gethash session-id seen))
          (setq ordered-items (append ordered-items (list item))))))
    ordered-items))

(defun opencode--project-session-filter (connection on-filter)
  "Call ON-FILTER with keyword args that list CONNECTION's project sessions.
Sessions outside a version-controlled project share one global project, so
those are narrowed to the connection directory instead."
  (opencode-client-location
   connection
   :success (lambda (&rest args)
              (let* ((project (alist-get 'project (plist-get args :data)))
                     (project-id (alist-get 'id project)))
                (funcall on-filter
                         (if (and project-id (not (equal project-id "global")))
                             (list :project project-id)
                           (list :directory (opencode-client--location-directory
                                             connection))))))
   :error (lambda (&rest _args)
            (error "Failed to resolve the OpenCode project"))))

(defun opencode--select-session (connection prompt on-selected)
  "Prompt for a session via CONNECTION using PROMPT.

Call ON-SELECTED with the selected session and session info data."
  (opencode--project-session-filter
   connection
   (lambda (filter)
     (apply #'opencode--select-session-matching connection prompt on-selected filter))))

(defun opencode--select-session-matching (connection prompt on-selected &rest filter)
  "Prompt for a CONNECTION session matching FILTER using PROMPT.
FILTER holds keyword arguments for `opencode-client-sessions'.  Call
ON-SELECTED with the selected session and session info data."
  (apply
   #'opencode-client-sessions
   connection
   :limit opencode-session-list-limit
   :roots t
   :success (lambda (&rest args)
              (let* ((data (plist-get args :data))
                     (items (cl-remove-if
                             (lambda (item) (alist-get 'parentID item))
                             (opencode--session-items data)))
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
            (error "Failed to fetch OpenCode sessions"))
   filter))

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
            (when-let* ((info (opencode-session-info session)))
              (opencode-session--update-session info))
            (opencode-session--ensure-agents connection))
          (pop-to-buffer existing-buffer)
          (when on-ready
            (funcall on-ready existing-buffer)))
      (opencode-session-open session connection on-ready))))

(defun opencode--project-directory ()
  "Return the current project root directory, if any."
  (when-let* ((project (project-current)))
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
(defun opencode-disconnect ()
  "Disconnect Emacs from OpenCode, leaving the shared service running."
  (interactive)
  (when opencode--service-connection
    (opencode-sse-close opencode--service-connection)
    (opencode-connection-stop opencode--service-connection))
  (message "Disconnected from OpenCode"))

;;;###autoload
(defun opencode-shutdown (&optional _directory)
  "Disconnect Emacs from the shared OpenCode service.
The obsolete DIRECTORY argument is ignored."
  (interactive)
  (opencode-disconnect))

(defalias 'opencode-shutdown-all #'opencode-disconnect)

;;;###autoload
(defun opencode-run-server (directory &optional on-ready)
  "Connect to the shared OpenCode service for DIRECTORY.
Call ON-READY with the connection in DIRECTORY's context when healthy.
All directories share one connection and event stream."
  (interactive (list (opencode--read-directory "OpenCode directory: ")))
  (let ((directory (opencode--normalize-directory directory)))
    (unless opencode--service-connection
      (setq opencode--service-connection (opencode-connection-create :timeout 10 :shared t)))
    (let* ((connection opencode--service-connection)
           (ready (lambda (conn)
                    (let ((default-directory directory))
                      (let* ((contexts (opencode-connection-contexts conn))
                             (known (and contexts
                                         (gethash (file-name-as-directory (file-truename directory))
                                                  contexts))))
                        (opencode-connection-context conn)
                        (when (and (not known) (opencode-connection-sse-connected conn))
                          (opencode-session--prompt-pending-requests conn)))
                      (unless (opencode-connection-sse-process conn)
                        (opencode-sse-open conn))
                      (when on-ready (funcall on-ready conn))))))
      (if (opencode-connection-alive-p connection)
          (funcall ready connection)
        (opencode-connection-start connection ready))
      connection)))

(defun opencode--create-session (connection on-created)
  "Create a session on CONNECTION and call ON-CREATED with it.
The session starts with `opencode-session-default-agent' when the server
offers that agent."
  (opencode-session--ensure-agents
   connection
   (lambda (agents)
     (opencode-client-session-create
      connection
      :agent (car (member opencode-session-default-agent agents))
      :success (lambda (&rest args)
                 (funcall on-created (opencode--session-from-data (plist-get args :data))))
      :error (lambda (&rest args)
               (let ((detail (opencode-client-format-error args)))
                 (error "Failed to create OpenCode session%s"
                        (if detail (format " (%s)" detail) ""))))))))

;;;###autoload
(defun opencode (directory)
  "Create a new OpenCode session for DIRECTORY and open its buffer."
  (interactive (list (opencode--read-directory "OpenCode directory: ")))
  (let ((normalized (opencode--normalize-directory directory)))
    (opencode-run-server
     normalized
     (lambda (connection)
       (opencode--create-session
        connection
        (lambda (session)
          (opencode-session-open session connection)))))))

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
       (opencode--create-session
        connection
        (lambda (session)
          (opencode-session-open
           session
           connection
           (lambda (buffer)
             (with-current-buffer buffer
               (opencode-session-insert-input prompt)
               (opencode-session-send-input))))))))))

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
                  (file-truename file)
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
  (let ((normalized (opencode--normalize-directory directory))
        ;; Capture the context now; the callbacks below run in other buffers.
        (context (opencode--contextual-snippet)))
    (opencode-run-server
     normalized
     (lambda (connection)
       (opencode--select-session
        connection
        "OpenCode session: "
        (lambda (session _info)
          (let* ((input prompt)
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
