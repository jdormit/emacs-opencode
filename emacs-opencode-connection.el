;;; emacs-opencode-connection.el --- OpenCode connection management  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'json)

(declare-function opencode-client-commands "emacs-opencode-client" (conn &rest args))
(declare-function opencode-client-models "emacs-opencode-client" (conn &rest args))
(declare-function opencode-client-integrations "emacs-opencode-client" (conn &rest args))

(cl-defstruct (opencode-connection (:constructor opencode-connection-create))
  base-url
  hostname
  port
  directory
  username
  password
  timeout
  agents
  agents-raw
  commands
  commands-state
  models
  integrations
  models-state
  project-id
  registration
  connected
  waiters
  contexts
  service
  shared
  (generation 0)
  process
  sse-process
  sse-state
  sse-mode
  sse-stderr-process
  sse-connected
  sse-retry-timer
  sse-retry-count)


(defcustom opencode-server-command "opencode"
  "OpenCode executable or command."
  :type 'string
  :group 'emacs-opencode)

(defun opencode-connection-registration-file ()
  "Return the shared service registration file."
  (expand-file-name "opencode/service.json"
                    (or (getenv "XDG_STATE_HOME")
                        (expand-file-name "~/.local/state"))))

(defun opencode-connection--read-registration ()
  "Read the service registration, or return nil if unavailable or invalid."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents (opencode-connection-registration-file))
        (let ((info (json-parse-buffer :object-type 'alist)))
          (when (and (stringp (alist-get 'url info))
                     (integerp (alist-get 'pid info))
                     (> (alist-get 'pid info) 0)
                     (or (null (alist-get 'password info))
                         (stringp (alist-get 'password info))))
            info)))
    (error nil)))

(defun opencode-connection-context (connection)
  "Return the directory-local catalog cache for CONNECTION.
The shared transport has no directory; callers route through
`default-directory'.  Explicit directory connections are useful in tests."
  (if (not (opencode-connection-shared connection))
      connection
    (unless (opencode-connection-contexts connection)
      (setf (opencode-connection-contexts connection)
            (make-hash-table :test 'equal)))
    (let* ((directory (file-name-as-directory (file-truename default-directory)))
           (contexts (opencode-connection-contexts connection)))
      (or (gethash directory contexts)
          (puthash directory (opencode-connection-create :directory directory
                                                        :service connection)
                   contexts)))))

(defun opencode-connection-ensure-commands (connection &optional on-success on-error)
  "Ensure commands are fetched and cached for CONNECTION.

ON-SUCCESS is called with ITEMS when available. ON-ERROR is called on failure."
  (require 'emacs-opencode-client)
  (setq connection (opencode-connection-context connection))
  (pcase (opencode-connection-commands-state connection)
    (:loaded
     (when on-success (funcall on-success (opencode-connection-commands connection)))
     (opencode-connection-commands connection))
    ((pred consp) nil)
    (_
     ;; A fresh token marks this load; results of a load that was
     ;; invalidated while in flight are dropped.
     (let ((token (list :loading)))
       (setf (opencode-connection-commands-state connection) token)
       (opencode-client-commands
        connection
        :success (lambda (&rest args)
                   (when (eq (opencode-connection-commands-state connection) token)
                     (let ((data (plist-get args :data)))
                       (setf (opencode-connection-commands connection) data)
                       (setf (opencode-connection-commands-state connection) :loaded)
                       (when on-success
                         (funcall on-success data)))))
        :error (lambda (&rest _args)
                 (when (eq (opencode-connection-commands-state connection) token)
                   (setf (opencode-connection-commands-state connection) nil)
                   (if on-error
                       (funcall on-error)
                     (message "OpenCode: failed to load commands")))))))))

(defun opencode-connection-invalidate-commands (connection)
  "Drop the cached command list for CONNECTION."
  (setq connection (opencode-connection-context connection))
  (setf (opencode-connection-commands connection) nil)
  (setf (opencode-connection-commands-state connection) nil))

(defun opencode-connection-ensure-models (connection &optional on-success on-error)
  "Ensure the model and integration catalogs are cached for CONNECTION.

The model catalog lists models from connected providers; the integration
catalog lists every provider that can be connected.  ON-SUCCESS is called
with the model list once both are available.  ON-ERROR is called on
failure."
  (require 'emacs-opencode-client)
  (setq connection (opencode-connection-context connection))
  (pcase (opencode-connection-models-state connection)
    (:loaded
     (when on-success (funcall on-success (opencode-connection-models connection)))
     (opencode-connection-models connection))
    ((pred consp) nil)
    (_
     (let ((token (list :loading))
           (pending 2)
           (failed nil))
       (setf (opencode-connection-models-state connection) token)
       (cl-flet ((settle ()
                   (when (and (zerop (cl-decf pending))
                              (eq (opencode-connection-models-state connection) token))
                     (if failed
                         (progn
                           (opencode-connection-invalidate-models connection)
                           (if on-error
                               (funcall on-error)
                             (message "OpenCode: failed to load models")))
                       (setf (opencode-connection-models-state connection) :loaded)
                       (when on-success
                         (funcall on-success
                                  (opencode-connection-models connection)))))))
         (opencode-client-models
          connection
          :success (lambda (&rest args)
                     (when (eq (opencode-connection-models-state connection) token)
                       (setf (opencode-connection-models connection)
                             (plist-get args :data)))
                     (settle))
          :error (lambda (&rest _args)
                   (setq failed t)
                   (settle)))
         (opencode-client-integrations
          connection
          :success (lambda (&rest args)
                     (when (eq (opencode-connection-models-state connection) token)
                       (setf (opencode-connection-integrations connection)
                             (plist-get args :data)))
                     (settle))
          :error (lambda (&rest _args)
                   (setq failed t)
                   (settle))))))))

(defun opencode-connection-invalidate-models (connection)
  "Drop the cached model and integration catalogs for CONNECTION."
  (setq connection (opencode-connection-context connection))
  (setf (opencode-connection-models connection) nil)
  (setf (opencode-connection-integrations connection) nil)
  (setf (opencode-connection-models-state connection) nil))

(defun opencode-connection--discover (connection success failure)
  "Discover a healthy service for CONNECTION, calling SUCCESS or FAILURE."
  (require 'emacs-opencode-client)
  (if-let* ((info (opencode-connection--read-registration)))
      (let ((generation (opencode-connection-generation connection))
            (probe (opencode-connection-create
                    :base-url (alist-get 'url info) :timeout 2
                    :username "opencode" :password (alist-get 'password info))))
        (opencode-client-health
         probe
         :success
         (lambda (&rest args)
           (when (= generation (opencode-connection-generation connection))
             (let ((data (plist-get args :data)))
               (if (and (equal (alist-get 'pid data) (alist-get 'pid info))
                        (stringp (alist-get 'version data)))
                   (progn
                     (unless (equal info (opencode-connection-registration connection))
                       (when-let* ((contexts (opencode-connection-contexts connection)))
                         (maphash (lambda (_directory cache)
                                    (opencode-connection-invalidate-models cache)
                                    (opencode-connection-invalidate-commands cache)
                                    (setf (opencode-connection-agents cache) nil
                                          (opencode-connection-agents-raw cache) nil))
                                  contexts)))
                     (setf (opencode-connection-registration connection) info
                           (opencode-connection-base-url connection) (alist-get 'url info)
                           (opencode-connection-username connection) "opencode"
                           (opencode-connection-password connection) (alist-get 'password info))
                     (funcall success connection))
                 (funcall failure)))))
         :error (lambda (&rest _)
                  (when (= generation (opencode-connection-generation connection))
                    (funcall failure)))))
    (funcall failure)))

(defun opencode-connection-start (connection &optional ready-callback error-callback)
  "Discover or start the shared service for CONNECTION.
Call READY-CALLBACK with CONNECTION, or ERROR-CALLBACK with an error string.
Concurrent callers share one startup operation."
  (let ((waiting (opencode-connection-waiters connection)))
    (push (cons ready-callback error-callback)
          (opencode-connection-waiters connection))
    (unless waiting
      (cl-incf (opencode-connection-generation connection))
      (cl-labels
          ((finish (error-message)
             (let ((callbacks (nreverse (opencode-connection-waiters connection))))
               (setf (opencode-connection-waiters connection) nil
                     (opencode-connection-connected connection)
                     (or (opencode-connection-connected connection) (not error-message)))
               (dolist (pair callbacks)
                 (if error-message
                     (if (cdr pair) (funcall (cdr pair) error-message)
                       (message "OpenCode: %s" error-message))
                   (when (car pair) (funcall (car pair) connection))))))
           (ready (_connection) (finish nil))
           (start ()
             (let ((buffer (generate-new-buffer " *opencode-service-start*")))
               (condition-case err
                   (let ((process
                         (make-process
                          :name "opencode-service-start" :buffer buffer :noquery t
                          :command (list opencode-server-command "service" "start")
                          :sentinel
                          (lambda (process _event)
                            (when (memq (process-status process) '(exit signal))
                              (setf (opencode-connection-process connection) nil)
                              (unwind-protect
                                  (if (zerop (process-exit-status process))
                                      (opencode-connection--discover
                                       connection #'ready
                                       (lambda () (finish "Service registration is not healthy after startup")))
                                    (finish (format "Service startup failed: %s"
                                                    (with-current-buffer buffer
                                                      (string-trim (buffer-string))))))
                                (kill-buffer buffer)))))))
                     (setf (opencode-connection-process connection) process))
                 (error
                  (kill-buffer buffer)
                  (finish (error-message-string err)))))))
        (opencode-connection--discover connection #'ready #'start))))
  connection)

(defun opencode-connection-alive-p (connection)
  "Return non-nil when CONNECTION is attached to the shared service."
  (opencode-connection-connected connection))

(defun opencode-connection-stop (connection)
  "Detach CONNECTION without stopping the shared service."
  (cl-incf (opencode-connection-generation connection))
  (setf (opencode-connection-connected connection) nil
        (opencode-connection-waiters connection) nil)
  (when-let* ((process (opencode-connection-process connection)))
    (set-process-sentinel process #'ignore)
    (when (process-live-p process) (delete-process process))
    (when-let* ((buffer (process-buffer process))) (kill-buffer buffer))
    (setf (opencode-connection-process connection) nil)))


(provide 'emacs-opencode-connection)

;;; emacs-opencode-connection.el ends here
