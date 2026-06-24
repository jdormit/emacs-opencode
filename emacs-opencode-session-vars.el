;;; emacs-opencode-session-vars.el --- Shared session state  -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup emacs-opencode nil
  "Emacs client for the OpenCode server."
  :group 'applications)

(defvar opencode-session--buffers (make-hash-table :test 'equal)
  "Registry mapping session IDs to buffers.")

(defvar opencode-session--subagent-tools (make-hash-table :test 'equal)
  "Registry mapping subagent session IDs to tool call data.
Each value is a list of alists with keys `tool' and `state'.")

(defvar opencode-session--subagent-parents (make-hash-table :test 'equal)
  "Registry mapping subagent session IDs to parent info.
Each value is a cons (PARENT-SESSION-ID . TASK-PART-ID).")

(defvar-local opencode-session--session nil
  "Session object for the current buffer.")

(defvar-local opencode-session--messages nil
  "List of message objects for the current buffer.")

(defvar-local opencode-session--connection nil
  "Connection used for the current session buffer.")

(defvar-local opencode-session--input-start-marker nil
  "Marker indicating the start of the input region.")

(defvar-local opencode-session--input-marker nil
  "Compatibility marker tracking the end of the input region.

The authoritative input region is from
`opencode-session--input-start-marker' through `point-max'.")

(defvar-local opencode-session--agent nil
  "Selected agent name for the current session buffer.")

(defvar-local opencode-session--provider-id nil
  "Selected provider ID for the current session buffer.")

(defvar-local opencode-session--model-id nil
  "Selected model ID for the current session buffer.")

(defvar-local opencode-session--variant nil
  "Selected model variant for the current session buffer.")

(defvar-local opencode-session--show-reasoning nil
  "When non-nil, show reasoning/thinking blocks in this session buffer.
Initialized from `opencode-session-show-reasoning' when the buffer
enters `opencode-session-mode', and toggled per-buffer by
`opencode-session-toggle-reasoning'.")

(defvar-local opencode-session--expanded-collapse-syms nil
  "Hash table of invisibility symbols the user has manually expanded.
Keys are symbols of the form `opencode-collapse-<part-id>'.
When a symbol is present in this table, re-renders will not
re-collapse the corresponding block.")

(defun opencode-session--buffer-for-session (session-id)
  "Return the session buffer for SESSION-ID, if any."
  (gethash session-id opencode-session--buffers))

(defun opencode-session--any-live-session-buffer (&optional connection)
  "Return any live buffer from `opencode-session--buffers'.
This is used as a fallback when a session ID (e.g. from a subagent)
has no registered buffer.

When CONNECTION is non-nil, only return a buffer whose
`opencode-session--connection' matches CONNECTION.  This is
essential when multiple OpenCode servers are running: a response
must be routed back to the server that originated the request, not
to an arbitrary buffer that may belong to a different server."
  (let (result)
    (maphash (lambda (_id buf)
               (when (and (not result) (buffer-live-p buf))
                 (when (or (null connection)
                           (with-current-buffer buf
                             (eq opencode-session--connection connection)))
                   (setq result buf))))
             opencode-session--buffers)
    result))

(defun opencode-session--normalize-items (items)
  "Normalize ITEMS to a list when vector or list."
  (cond
   ((vectorp items) (append items nil))
   ((listp items) items)
   (t nil)))

(defun opencode-session--register-subagent (subagent-session-id parent-session-id task-part-id)
  "Register SUBAGENT-SESSION-ID as a child of PARENT-SESSION-ID.
TASK-PART-ID is the part ID of the task tool call in the parent."
  (puthash subagent-session-id
           (cons parent-session-id task-part-id)
           opencode-session--subagent-parents))

(defun opencode-session--subagent-parent (subagent-session-id)
  "Return the parent info for SUBAGENT-SESSION-ID.
Returns a cons (PARENT-SESSION-ID . TASK-PART-ID) or nil."
  (gethash subagent-session-id opencode-session--subagent-parents))

(defun opencode-session--subagent-tools-for (subagent-session-id)
  "Return the list of tool call data for SUBAGENT-SESSION-ID."
  (gethash subagent-session-id opencode-session--subagent-tools))

(defun opencode-session--update-subagent-tool (subagent-session-id part-id tool state)
  "Update tool tracking for SUBAGENT-SESSION-ID.
PART-ID identifies the tool part, TOOL is the tool name, and
STATE is the tool state alist."
  (let* ((tools (gethash subagent-session-id opencode-session--subagent-tools))
         (existing (cl-find part-id tools
                           :key (lambda (item) (alist-get 'part-id item))
                           :test #'equal))
         (entry `((part-id . ,part-id) (tool . ,tool) (state . ,state))))
    (if existing
        (puthash subagent-session-id
                 (cl-substitute entry existing tools :test #'equal)
                 opencode-session--subagent-tools)
      (puthash subagent-session-id
               (append tools (list entry))
               opencode-session--subagent-tools))))

(defun opencode-session--cleanup-subagent (parent-session-id)
  "Remove subagent tracking data for children of PARENT-SESSION-ID."
  (let (to-remove)
    (maphash (lambda (subagent-id parent-info)
               (when (equal (car parent-info) parent-session-id)
                 (push subagent-id to-remove)))
             opencode-session--subagent-parents)
    (dolist (id to-remove)
      (remhash id opencode-session--subagent-parents)
      (remhash id opencode-session--subagent-tools))))

(provide 'emacs-opencode-session-vars)

;;; emacs-opencode-session-vars.el ends here
