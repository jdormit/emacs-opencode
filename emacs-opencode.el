;;; emacs-opencode.el --- OpenCode entrypoint  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'project)
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
(defun opencode (directory)
  "Start or reuse an OpenCode server for DIRECTORY.

When a connection already exists for DIRECTORY, reuse it without restarting
its server process."
  (interactive (list (opencode--read-directory "OpenCode directory: ")))
  (let* ((normalized (opencode--normalize-directory directory))
         (existing (opencode--get-connection normalized)))
    (if existing
        (progn
          (message "OpenCode already running for %s" normalized)
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
              (message "Started OpenCode server for %s" normalized))
            (lambda (&rest _args)
              (error "OpenCode server failed to become healthy for %s" normalized)))
           (opencode-sse-open connection)))
        (opencode--register-connection normalized connection)
        connection))))

;;;###autoload
(defun opencode-new-session (directory)
  "Create a new session for DIRECTORY and open its buffer." 
  (interactive (list (opencode--read-directory "OpenCode directory: ")))
  (let* ((normalized (opencode--normalize-directory directory))
         (connection (opencode--get-connection normalized)))
    (unless connection
      (setq connection (opencode normalized)))
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
              (error "Failed to create OpenCode session")))))

;;;###autoload
(defun opencode-open-session (directory)
  "Prompt for a session in DIRECTORY and open its buffer." 
  (interactive (list (opencode--read-directory "OpenCode directory: ")))
  (let* ((normalized (opencode--normalize-directory directory))
         (connection (opencode--get-connection normalized)))
    (unless connection
      (setq connection (opencode normalized)))
    (opencode-client-sessions
     connection
     :success (lambda (&rest args)
                (let* ((data (plist-get args :data))
                       (items (if (listp data)
                                  (alist-get 'sessions data)
                                data))
                       (items-list (cond
                                    ((vectorp items) (append items nil))
                                    ((listp items) items)
                                    (t nil)))
                       (counts (make-hash-table :test 'equal))
                       (choices (mapcar (lambda (item)
                                          (let* ((title (opencode--session-label item))
                                                 (count (1+ (gethash title counts 0))))
                                            (puthash title count counts)))
                                        items-list))
                       (choices (mapcar (lambda (item)
                                          (let* ((title (opencode--session-label item))
                                                 (ambiguous (> (gethash title counts 0) 1))
                                                 (label (opencode--session-label item ambiguous)))
                                            (cons label item)))
                                        items-list))
                       (selected (completing-read "OpenCode session: " choices nil t))
                       (data (cdr (assoc selected choices)))
                       (session (opencode--session-from-data data)))
                  (opencode-session-open session connection)))
     :error (lambda (&rest _args)
              (error "Failed to fetch OpenCode sessions")))))

(provide 'emacs-opencode)

;;; emacs-opencode.el ends here
