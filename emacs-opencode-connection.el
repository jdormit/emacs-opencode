;;; emacs-opencode-connection.el --- OpenCode connection management  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(declare-function opencode-client-commands "emacs-opencode-client" (conn &key success error))
(declare-function opencode-client-providers "emacs-opencode-client" (conn &key success error))
(declare-function opencode-acp-start "emacs-opencode-acp" (directory port &optional ready-callback))
(declare-function opencode-acp-stop "emacs-opencode-acp" (process))

(cl-defstruct (opencode-connection (:constructor opencode-connection-create))
  ;; HTTP server fields (for the embedded server in `opencode acp')
  base-url
  hostname
  port
  directory
  username
  password
  timeout
  ;; Cached metadata
  agents
  agents-raw
  commands
  providers
  provider-catalog
  ;; ACP process (JSON-RPC over stdio)
  process)


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

(defun opencode-connection-ensure-commands (connection &optional on-success on-error)
  "Ensure commands are fetched and cached for CONNECTION.

ON-SUCCESS is called with ITEMS when available. ON-ERROR is called on failure."
  (require 'emacs-opencode-client)
  (let ((commands (opencode-connection-commands connection)))
    (cond
     ((and commands (or (vectorp commands) (listp commands)))
      (when on-success (funcall on-success commands))
      commands)
     ((eq commands :loading) nil)
     (t
      (setf (opencode-connection-commands connection) :loading)
      (opencode-client-commands
       connection
       :success (lambda (&rest args)
                  (let ((data (plist-get args :data)))
                    (setf (opencode-connection-commands connection) data)
                    (when on-success
                      (funcall on-success data))))
       :error (lambda (&rest _args)
                (setf (opencode-connection-commands connection) nil)
                (if on-error
                    (funcall on-error)
                  (message "OpenCode: failed to load commands"))))))))

(defun opencode-connection-ensure-providers (connection &optional on-success on-error)
  "Ensure providers are fetched and cached for CONNECTION.

ON-SUCCESS is called with ITEMS when available. ON-ERROR is called on failure."
  (require 'emacs-opencode-client)
  (let ((providers (opencode-connection-providers connection)))
    (cond
     ((and providers (or (vectorp providers) (listp providers)))
      (when on-success (funcall on-success providers))
      providers)
     ((eq providers :loading) nil)
     (t
      (setf (opencode-connection-providers connection) :loading)
      (setf (opencode-connection-provider-catalog connection) :loading)
      (opencode-client-providers
       connection
       :success (lambda (&rest args)
                  (let* ((data (plist-get args :data))
                         (items (alist-get 'all data)))
                     (setf (opencode-connection-providers connection) items)
                     (setf (opencode-connection-provider-catalog connection) data)
                     (when on-success
                       (funcall on-success items))))
       :error (lambda (&rest _args)
                 (setf (opencode-connection-providers connection) nil)
                 (setf (opencode-connection-provider-catalog connection) nil)
                 (if on-error
                     (funcall on-error)
                   (message "OpenCode: failed to load providers"))))))))

(defun opencode-connection-start (connection &optional ready-callback)
  "Start an OpenCode ACP subprocess for CONNECTION.

Spawns `opencode acp' with JSON-RPC over stdio.  The embedded HTTP server
runs on the port stored in CONNECTION for lightweight polling.
READY-CALLBACK is called with the process when the ACP initialize
handshake succeeds.  Returns the updated CONNECTION."
  (require 'emacs-opencode-acp)
  (let* ((directory (opencode-connection-directory connection))
         (port (opencode-connection-port connection))
         (process (opencode-acp-start
                   directory port
                   (lambda (proc)
                     (setf (opencode-connection-process connection) proc)
                     (opencode-connection-ensure-providers connection)
                     (opencode-connection-ensure-commands connection)
                     (when ready-callback
                       (funcall ready-callback proc))))))
    (setf (opencode-connection-process connection) process)
    connection))

(defun opencode-connection-alive-p (connection)
  "Return non-nil if CONNECTION's server process is alive."
  (when-let ((process (opencode-connection-process connection)))
    (process-live-p process)))

(defun opencode-connection-stop (connection)
  "Stop the OpenCode ACP process associated with CONNECTION."
  (require 'emacs-opencode-acp)
  ;; Stop the ACP process.  Question/permission polling timers are
  ;; per-buffer and self-terminate when the connection dies.
  (when-let ((process (opencode-connection-process connection)))
    (opencode-acp-stop process)
    (setf (opencode-connection-process connection) nil)))


(provide 'emacs-opencode-connection)

;;; emacs-opencode-connection.el ends here
