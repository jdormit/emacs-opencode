;;; emacs-opencode-run.el --- Async wrapper around `opencode run'  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-client)

(declare-function opencode-run-server "emacs-opencode" (directory &optional on-ready))

(defun opencode-run--build-args (prompt &rest args)
  "Build the argument list for `opencode run' with PROMPT.

ARGS is a plist supporting :MODEL, :AGENT, and :DANGEROUSLY-SKIP-PERMISSIONS,
each mapping to the corresponding CLI flag.  Flags with nil values are
omitted.  The run uses the shared service.
PROMPT is appended last as the message."
  (let ((model (plist-get args :model))
        (agent (plist-get args :agent))
        (skip (plist-get args :dangerously-skip-permissions))
        (result (list "run")))
    (when model
      (setq result (append result (list "--model" model))))
    (when agent
      (setq result (append result (list "--agent" agent))))
    (when skip
      (setq result (append result (list "--auto"))))
    (append result (list prompt))))

(defun opencode-run--process-environment (directory)
  "Return `process-environment' with PWD set to DIRECTORY.
The CLI resolves its project from PWD rather than its working directory."
  (cons (concat "PWD=" (directory-file-name (expand-file-name directory)))
        (cl-remove-if (lambda (entry) (string-prefix-p "PWD=" entry))
                      process-environment)))

(defun opencode-run--invoke-callback (callback output success-p)
  "Call CALLBACK with OUTPUT, passing SUCCESS-P when it accepts a second arg.

When CALLBACK is nil, do nothing.  Trailing whitespace and newlines are
stripped from OUTPUT before it is passed on.  CALLBACK's arity is inspected
with `func-arity': callbacks that accept two or more arguments receive
\(OUTPUT SUCCESS-P), while single-argument callbacks receive only OUTPUT."
  (when callback
    (let ((output (and output (string-trim-right output)))
          (max-arity (cdr (func-arity callback))))
      (if (or (eq max-arity 'many) (and (integerp max-arity) (>= max-arity 2)))
          (funcall callback output success-p)
        (funcall callback output)))))

(defun opencode-run--model-ref (model)
  "Parse MODEL, a \"provider/model#variant\" string, into a model reference."
  (when (and model (string-match "\\`\\([^/]+\\)/\\([^#]+\\)\\(?:#\\(.+\\)\\)?\\'" model))
    (append `((providerID . ,(match-string 1 model))
              (id . ,(match-string 2 model)))
            (when (match-string 3 model)
              `((variant . ,(match-string 3 model)))))))

(defun opencode-run--command-output (messages)
  "Return the assistant text that followed the last user prompt in MESSAGES."
  (let ((turn (cl-loop for message in (reverse messages)
                       until (equal (alist-get 'type message) "user")
                       collect message)))
    (string-join
     (cl-loop for message in (nreverse turn)
              when (equal (alist-get 'type message) "assistant")
              append (cl-loop for item in (alist-get 'content message)
                              when (equal (alist-get 'type item) "text")
                              collect (string-trim (alist-get 'text item))))
     "\n\n")))

(defun opencode-run--command (command prompt callback model agent skip)
  "Run COMMAND with PROMPT as its arguments in a new session.

The CLI cannot run commands, so this uses the server for
`default-directory'.  CALLBACK, MODEL, AGENT, and SKIP behave as in
`opencode-run'."
  (require 'emacs-opencode)
  (cl-flet ((fail (args)
              (opencode-run--invoke-callback
               callback
               (format "OpenCode command %s failed%s" command
                       (if-let* ((detail (opencode-client-format-error args)))
                           (format ": %s" detail)
                         ""))
               nil)))
    (opencode-run-server
     default-directory
     (lambda (connection)
       (opencode-client-session-create
        connection
        :agent agent
        :model (opencode-run--model-ref model)
        :permissions (when skip
                       [((action . "*") (resource . "*") (effect . "allow"))])
        :success
        (lambda (&rest args)
          (let ((session-id (alist-get 'id (plist-get args :data))))
            (opencode-client-session-command
             connection session-id command prompt
             :success
             (lambda (&rest _args)
               (opencode-client-session-wait
                connection session-id
                :success
                (lambda (&rest _args)
                  (opencode-client-session-messages
                   connection session-id
                   :success (lambda (&rest args)
                              (opencode-run--invoke-callback
                               callback
                               (opencode-run--command-output (plist-get args :data))
                               t))
                   :error (lambda (&rest args) (fail args))))
                :error (lambda (&rest args) (fail args))))
             :error (lambda (&rest args) (fail args)))))
        :error (lambda (&rest args) (fail args)))))))

(cl-defun opencode-run (prompt callback
                               &key model agent command
                               dangerously-skip-permissions)
  "Run `opencode run' asynchronously with PROMPT and invoke CALLBACK.

CALLBACK is called when the process finishes.  When CALLBACK accepts two or
more arguments it receives (OUTPUT SUCCESS-P); when it accepts a single
argument it receives just OUTPUT.  OUTPUT is the command's standard output
\(the model's plain-text answer); the formatted header and ANSI control
sequences OpenCode writes to standard error are discarded.  SUCCESS-P is
non-nil when the process exited successfully.

MODEL (\"provider/model\" or \"provider/model#variant\") and AGENT map to
the `--model' and `--agent' flags.  When DANGEROUSLY-SKIP-PERMISSIONS is
non-nil, permissions are approved automatically.

When COMMAND is non-nil, run that slash command with PROMPT as its
arguments instead.  The CLI cannot run commands, so this starts or reuses
the session server for the directory and runs the command in a new
session there.

The command runs in `default-directory'; bind it to control where OpenCode
runs."
  (if command
      (opencode-run--command command prompt callback model agent
                             dangerously-skip-permissions)
    (let ((executable (executable-find opencode-server-command)))
      (unless executable
        (error "OpenCode executable not found: %s" opencode-server-command))
      (let* ((args (opencode-run--build-args
                    prompt
                    :model model
                    :agent agent
                    :dangerously-skip-permissions dangerously-skip-permissions))
             (process-environment (opencode-run--process-environment default-directory))
             (buffer (generate-new-buffer
                      (format " *opencode-run<%s>*" default-directory)))
             ;; OpenCode writes a formatted header and ANSI control sequences to
             ;; stderr.  Route it to its own buffer so OUTPUT stays plain text.
             (stderr-buffer (generate-new-buffer
                             (format " *opencode-run-stderr<%s>*" default-directory)))
             (process
              (make-process
               :name "opencode-run"
               :buffer buffer
               :command (cons executable args)
               :connection-type 'pipe
               :noquery t
               :stderr stderr-buffer
               :sentinel
               (lambda (process _event)
                 (when (memq (process-status process) '(exit signal))
                   (let ((output (when (buffer-live-p buffer)
                                   (with-current-buffer buffer
                                     (buffer-string))))
                         (success-p (and (eq (process-status process) 'exit)
                                         (= (process-exit-status process) 0))))
                     (when (buffer-live-p buffer)
                       (kill-buffer buffer))
                     (when (buffer-live-p stderr-buffer)
                       (kill-buffer stderr-buffer))
                     (opencode-run--invoke-callback callback output success-p)))))))
        ;; `opencode run' reads stdin when it is not a terminal and waits
        ;; for EOF, so close the input stream immediately.
        (process-send-eof process)
        process))))

(provide 'emacs-opencode-run)

;;; emacs-opencode-run.el ends here
