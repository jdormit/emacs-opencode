;;; emacs-opencode-sse.el --- OpenCode SSE handling  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-opencode-connection)

(defgroup emacs-opencode nil
  "Emacs client for the OpenCode server."
  :group 'applications)

(defcustom opencode-sse-curl-command "curl"
  "Curl command used for SSE streaming."
  :type 'string
  :group 'emacs-opencode)

(defvar opencode-sse--handlers nil
  "Alist mapping SSE event names to handler lists.")

(defun opencode-sse--add-handler (event handler)
  "Register HANDLER for SSE EVENT if not already present."
  (let ((current (alist-get event opencode-sse--handlers nil nil #'string=)))
    (unless (memq handler current)
      (setf (alist-get event opencode-sse--handlers nil nil #'string=)
            (cons handler current)))))

(defmacro opencode-sse-define-handler (event args &rest body)
  "Define and register an SSE handler for EVENT.

ARGS and BODY are passed to `defun`. EVENT is the SSE event name string.
Returns the created function symbol."
  (declare (indent defun))
  (let ((fn-name (intern (format "opencode-sse--on-%s" event))))
    `(progn
       (defun ,fn-name ,args
        ,(format "Handle SSE event %s." event)
        ,@body)
       (opencode-sse--add-handler ,event #',fn-name)
       #',fn-name)))

(defun opencode-sse-register-handler (event handler)
  "Register HANDLER for SSE EVENT.

EVENT is a string. HANDLER receives EVENT and DATA." 
  (opencode-sse--add-handler event handler))

(defun opencode-sse-unregister-handler (event)
  "Remove any handler registered for EVENT." 
  (setq opencode-sse--handlers
        (assoc-delete-all event opencode-sse--handlers)))

(defun opencode-sse--dispatch (event data)
  "Dispatch EVENT and DATA to registered handlers."
  (let ((handlers (alist-get event opencode-sse--handlers nil nil #'string=)))
    (dolist (handler handlers)
      (funcall handler event data))))

(defun opencode-sse--decode-data (data)
  "Decode DATA from JSON.

Signals an error when DATA is not valid JSON." 
  (condition-case err
      (with-temp-buffer
        (insert data)
        (goto-char (point-min))
        (json-read))
    (error "OpenCode SSE payload is not valid JSON: %s"
           (error-message-string err))))

(defun opencode-sse--ensure-curl ()
  "Ensure the configured curl executable exists." 
  (unless (executable-find opencode-sse-curl-command)
    (error "OpenCode SSE requires curl; ensure `%s` is on PATH" opencode-sse-curl-command)))

(defun opencode-sse--build-url (connection)
  "Build the SSE endpoint URL for CONNECTION." 
  (format "%s/event"
          (string-remove-suffix "/" (opencode-connection-base-url connection))))

(defun opencode-sse--auth-header (connection)
  "Return an Authorization header for CONNECTION when needed." 
  (when-let ((password (opencode-connection-password connection)))
    (let ((user (or (opencode-connection-username connection) "opencode")))
      (format "Authorization: Basic %s"
              (base64-encode-string (format "%s:%s" user password) t)))))

(defun opencode-sse--build-command (connection)
  "Build curl command for CONNECTION." 
  (let ((url (opencode-sse--build-url connection))
        (auth (opencode-sse--auth-header connection)))
    (append
     (list opencode-sse-curl-command "-N" "-s" "-S")
     (when auth (list "-H" auth))
     (list url))))

(defun opencode-sse--initialize-state (connection)
  "Initialize SSE parse state on CONNECTION." 
  (setf (opencode-connection-sse-state connection)
        (list :buffer "" :data nil)))

(defun opencode-sse--clear-event (state)
  "Reset event fields in STATE." 
  (plist-put state :data nil)
  state)

(defun opencode-sse--append-data (state value)
  "Append VALUE to STATE data field." 
  (let ((current (plist-get state :data)))
    (plist-put state :data (if current (concat current "\n" value) value))))

(defun opencode-sse--finalize-event (state)
  "Finalize and dispatch event from STATE." 
  (let ((data (plist-get state :data)))
    (when data
      (let* ((payload (opencode-sse--decode-data data))
             (event (alist-get 'type payload nil nil #'string=)))
        (unless event
          (error "OpenCode SSE payload missing type field"))
        (opencode-sse--dispatch event payload))))
  (opencode-sse--clear-event state))

(defun opencode-sse--process-line (state line)
  "Process LINE in SSE parser STATE." 
  (cond
   ((string-empty-p line)
    (opencode-sse--finalize-event state))
   ((string-prefix-p ":" line)
    state)
   (t
    (let* ((parts (split-string line ":" t " +"))
           (field (car parts))
           (value (string-join (cdr parts) ":")))
      (pcase field
        ("data" (opencode-sse--append-data state value))
        (_ state))))))

(defun opencode-sse--process-chunk (connection chunk)
  "Process SSE CHUNK for CONNECTION." 
  (let* ((state (or (opencode-connection-sse-state connection)
                    (opencode-sse--initialize-state connection)))
         (buffer (concat (plist-get state :buffer) chunk))
         (lines (split-string buffer "\n"))
         (incomplete (car (last lines)))
         (complete-lines (butlast lines)))
    (plist-put state :buffer incomplete)
    (dolist (line complete-lines)
      (opencode-sse--process-line state (string-trim-right line "\r")))))

(defun opencode-sse-open (connection)
  "Open an SSE stream for CONNECTION.

Returns the streaming process." 
  (opencode-sse--ensure-curl)
  (let* ((buffer (get-buffer-create (format " *opencode-sse<%s>*"
                                            (opencode-connection-directory connection))))
         (command (opencode-sse--build-command connection))
         (process (make-process
                   :name "opencode-sse"
                   :buffer buffer
                   :command command
                   :noquery t
                   :filter (lambda (proc output)
                             (when-let ((buffer (process-buffer proc)))
                               (when (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   (goto-char (point-max))
                                   (insert output))))
                             (opencode-sse--process-chunk connection output))
                   :sentinel (lambda (proc _event)
                               (when (memq (process-status proc) '(exit signal))
                                 (when (buffer-live-p buffer)
                                   (with-current-buffer buffer
                                     (goto-char (point-max))
                                     (insert "\n[opencode] SSE stream closed"))))))))
    (setf (opencode-connection-sse-process connection) process)
    process))

(defun opencode-sse-close (connection)
  "Stop the SSE stream for CONNECTION." 
  (when-let ((process (opencode-connection-sse-process connection)))
    (when (process-live-p process)
      (delete-process process))
    (when-let ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (setf (opencode-connection-sse-process connection) nil)
    (setf (opencode-connection-sse-state connection) nil)))

(provide 'emacs-opencode-sse)

;;; emacs-opencode-sse.el ends here
