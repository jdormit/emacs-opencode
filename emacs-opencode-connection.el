;;; emacs-opencode-connection.el --- OpenCode connection management  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(cl-defstruct (opencode-connection (:constructor opencode-connection-create))
  base-url
  hostname
  port
  directory
  username
  password
  timeout
  agents
  providers
  process
  sse-process
  sse-state)


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

(defun opencode-connection--maybe-ready (process output ready-callback)
  "Process OUTPUT and call READY-CALLBACK when server is ready."
  (when (and ready-callback
             (not (process-get process 'opencode-ready)))
    (when (string-match-p "opencode server listening on" output)
      (process-put process 'opencode-ready t)
      (funcall ready-callback process))))

(defun opencode-connection-start (connection &optional ready-callback)
  "Start an OpenCode server for CONNECTION.

READY-CALLBACK is called when the server reports readiness. Returns the
updated CONNECTION."
  (let* ((default-directory (opencode-connection-directory connection))
         (hostname (opencode-connection-hostname connection))
         (port (opencode-connection-port connection))
         (command (list (executable-find opencode-server-command) "serve"
                        "--hostname" hostname
                        "--port" (number-to-string port)))
         (buffer (get-buffer-create (format " *opencode-server<%s>*" default-directory)))
         (process (apply #'start-process "opencode-server" buffer command)))
    (set-process-filter
     process
     (lambda (proc output)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (goto-char (point-max))
           (insert output)))
       (opencode-connection--maybe-ready proc output ready-callback)))
    (setf (opencode-connection-process connection) process)
    connection))

(defun opencode-connection-stop (connection)
  "Stop the OpenCode server associated with CONNECTION."
  (when-let ((process (opencode-connection-process connection)))
    (when (process-live-p process)
      (delete-process process))
    (when-let ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (setf (opencode-connection-process connection) nil)))


(provide 'emacs-opencode-connection)

;;; emacs-opencode-connection.el ends here
