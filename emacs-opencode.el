;;; emacs-opencode.el --- OpenCode entrypoint  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-client)

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
    (opencode-connection-stop connection)
    (opencode--unregister-connection normalized)
    (message "Stopped OpenCode server for %s" normalized)))

;;;###autoload
(defun opencode (directory)
  "Start or reuse an OpenCode server for DIRECTORY.

When a connection already exists for DIRECTORY, reuse it without restarting
its server process."
  (interactive (list (read-directory-name "OpenCode directory: " default-directory nil t)))
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
              (error "OpenCode server failed to become healthy for %s" normalized)))))
        (opencode--register-connection normalized connection)
        connection))))


(provide 'emacs-opencode)

;;; emacs-opencode.el ends here
