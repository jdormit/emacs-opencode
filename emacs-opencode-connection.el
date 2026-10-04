;;; emacs-opencode-connection.el --- OpenCode connection management  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

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
  process
  sse-process
  sse-state
  sse-mode
  sse-stderr-process
  sse-connected
  sse-retry-timer
  sse-retry-count)


(defcustom opencode-server-host "127.0.0.1"
  "Default hostname for OpenCode servers."
  :type 'string
  :group 'emacs-opencode)

(defcustom opencode-server-port 4096
  "Default port for OpenCode servers."
  :type 'integer
  :group 'emacs-opencode)

(defcustom opencode-server-command "opencode"
  "OpenCode executable or command."
  :type 'string
  :group 'emacs-opencode)

(defcustom opencode-server-environment nil
  "Environment variables for OpenCode server processes.

Each entry is a (NAME . VALUE) pair. NAME is a string. VALUE is a string or
nil. When VALUE is nil, remove NAME from the server process environment.
Otherwise, set NAME to VALUE (converted with `format')."
  :type '(repeat (cons (string :tag "Name")
                       (choice (string :tag "Value")
                               (const :tag "Unset" nil))))
  :group 'emacs-opencode)

(defun opencode-connection--process-environment (environment)
  "Return `process-environment' updated with ENVIRONMENT.

ENVIRONMENT is an alist of (NAME . VALUE) pairs. NAME is a string. VALUE is a
string or nil. When VALUE is nil, remove NAME from the environment. When VALUE
is non-nil, set NAME to VALUE (converted with `format')."
  (let ((updated (copy-sequence process-environment)))
    (dolist (entry environment)
      (let* ((name (car entry))
             (value (cdr entry))
             (prefix (and (stringp name) (concat name "="))))
        (when prefix
          (setq updated (cl-remove-if (lambda (item)
                                        (string-prefix-p prefix item))
                                      updated))
          (when value
            (push (format "%s=%s" name value) updated)))))
    updated))

(defun opencode-connection--port-available-p (hostname port)
  "Return non-nil when PORT can be bound on HOSTNAME."
  (when (and port (> port 0))
    (condition-case nil
        (let ((process (make-network-process
                        :name "opencode-port-check"
                        :server t
                        :host hostname
                        :service port
                        :noquery t)))
          (delete-process process)
          t)
      (file-error nil))))

(defun opencode-connection--pick-random-port (hostname)
  "Return a free TCP port bound on HOSTNAME."
  (let ((process (make-network-process
                  :name "opencode-port-random"
                  :server t
                  :host hostname
                  :service 0
                  :noquery t)))
    (unwind-protect
        (process-contact process :service)
      (delete-process process))))

(defun opencode-connection--base-url (hostname port)
  "Build base URL for HOSTNAME and PORT."
  (format "http://%s:%d" hostname port))

(defun opencode-connection-create-for-directory (directory &optional hostname port)
  "Create a connection object for DIRECTORY.

HOSTNAME and PORT override the default server config."
  (let* ((resolved-host (or hostname opencode-server-host))
         (resolved-port (or port
                            (if (opencode-connection--port-available-p
                                 resolved-host
                                 opencode-server-port)
                                opencode-server-port
                              (opencode-connection--pick-random-port
                               resolved-host)))))
    (opencode-connection-create
     :base-url (opencode-connection--base-url resolved-host resolved-port)
     :hostname resolved-host
     :port resolved-port
     :directory (file-name-as-directory (expand-file-name directory))
     :timeout 10)))

(defun opencode-connection--environment-password (environment)
  "Return the server password set in ENVIRONMENT, if any.

ENVIRONMENT has the shape of `opencode-server-environment'."
  (or (cdr (assoc "OPENCODE_PASSWORD" environment))
      (cdr (assoc "OPENCODE_SERVER_PASSWORD" environment))))

(defun opencode-connection--generate-password ()
  "Return a random password for a new server."
  (let ((bytes (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (condition-case nil
                     (insert-file-contents-literally "/dev/urandom" nil 0 32)
                   (file-error
                    (dotimes (_ 32) (insert (random 256)))))
                 (buffer-string))))
    (base64url-encode-string bytes t)))

(defun opencode-connection--server-environment (password)
  "Return the server environment with PASSWORD set as its password.
An explicit value overrides any password inherited from Emacs."
  (opencode-connection--process-environment
   (append opencode-server-environment
           `(("OPENCODE_PASSWORD" . ,password)))))

(defun opencode-connection--maybe-ready (process output connection ready-callback)
  "Process OUTPUT and call READY-CALLBACK when server is ready."
  (when (and ready-callback
             (not (process-get process 'opencode-ready)))
    (let ((seen (concat (process-get process 'opencode-output) output)))
      (process-put process 'opencode-output seen)
      (when (string-match-p "server listening on" seen)
        (process-put process 'opencode-ready t)
        (process-put process 'opencode-output nil)
        (funcall ready-callback process)
        (opencode-connection-ensure-models connection)
        (opencode-connection-ensure-commands connection)))))

(defun opencode-connection-ensure-commands (connection &optional on-success on-error)
  "Ensure commands are fetched and cached for CONNECTION.

ON-SUCCESS is called with ITEMS when available. ON-ERROR is called on failure."
  (require 'emacs-opencode-client)
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
  (setf (opencode-connection-commands connection) nil)
  (setf (opencode-connection-commands-state connection) nil))

(defun opencode-connection-ensure-models (connection &optional on-success on-error)
  "Ensure the model and integration catalogs are cached for CONNECTION.

The model catalog lists models from connected providers; the integration
catalog lists every provider that can be connected.  ON-SUCCESS is called
with the model list once both are available.  ON-ERROR is called on
failure."
  (require 'emacs-opencode-client)
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
  (setf (opencode-connection-models connection) nil)
  (setf (opencode-connection-integrations connection) nil)
  (setf (opencode-connection-models-state connection) nil))

(defun opencode-connection-start (connection &optional ready-callback)
  "Start an OpenCode server for CONNECTION.

READY-CALLBACK is called when the server reports readiness. Returns the
updated CONNECTION."
  (let* ((default-directory (opencode-connection-directory connection))
          (hostname (opencode-connection-hostname connection))
          (port (opencode-connection-port connection))
          (password (or (opencode-connection--environment-password
                         opencode-server-environment)
                        (opencode-connection--generate-password)))
          (process-environment (opencode-connection--server-environment password))
          (command (list (executable-find opencode-server-command) "serve"
                         "--hostname" hostname
                         "--port" (number-to-string port)))
         (buffer (get-buffer-create (format " *opencode-server<%s>*" default-directory)))
         (process (apply #'start-process "opencode-server" buffer command)))
    (setf (opencode-connection-username connection) "opencode")
    (setf (opencode-connection-password connection) password)
    (set-process-filter
     process
     (lambda (proc output)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (goto-char (point-max))
           (insert output)))
       (opencode-connection--maybe-ready proc output connection ready-callback)))
    (setf (opencode-connection-process connection) process)
    connection))

(defun opencode-connection-alive-p (connection)
  "Return non-nil if CONNECTION's server process is alive."
  (when-let* ((process (opencode-connection-process connection)))
    (process-live-p process)))

(defun opencode-connection-stop (connection)
  "Stop the OpenCode server associated with CONNECTION."
  (when-let* ((process (opencode-connection-process connection)))
    (when (process-live-p process)
      (delete-process process))
    (when-let* ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (setf (opencode-connection-process connection) nil)))


(provide 'emacs-opencode-connection)

;;; emacs-opencode-connection.el ends here
