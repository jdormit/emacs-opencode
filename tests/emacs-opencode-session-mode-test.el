;;; emacs-opencode-session-mode-test.el --- Tests for session mode  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-opencode-session-mode)

;;; parse-command-input

(ert-deftest test-opencode-session-mode/parse-command-basic ()
  "Parse a basic slash command."
  (let ((result (opencode-session--parse-command-input "/help")))
    (should (equal (car result) "help"))
    (should (equal (cadr result) ""))))

(ert-deftest test-opencode-session-mode/parse-command-with-args ()
  "Parse a slash command with arguments."
  (let ((result (opencode-session--parse-command-input "/search hello world")))
    (should (equal (car result) "search"))
    (should (equal (cadr result) "hello world"))))

(ert-deftest test-opencode-session-mode/parse-command-not-a-command ()
  "Return nil command for non-slash input."
  (let ((result (opencode-session--parse-command-input "regular text")))
    (should (null (car result)))
    (should (equal (cadr result) ""))))

(ert-deftest test-opencode-session-mode/parse-command-empty ()
  "Handle empty input."
  (let ((result (opencode-session--parse-command-input "")))
    (should (null (car result)))))

(ert-deftest test-opencode-session-mode/parse-command-slash-only ()
  "A bare slash is not a valid command."
  (let ((result (opencode-session--parse-command-input "/")))
    (should (null (car result)))))

;;; buffer-name

(ert-deftest test-opencode-session-mode/buffer-name-with-title ()
  "Buffer name includes session title."
  (let ((session (opencode-session-create :title "My Session")))
    (should (equal (opencode-session--buffer-name session)
                   "*OpenCode: My Session*"))))

(ert-deftest test-opencode-session-mode/buffer-name-fallback-slug ()
  "Fall back to slug when title is empty."
  (let ((session (opencode-session-create :slug "my-slug")))
    (should (equal (opencode-session--buffer-name session)
                   "*OpenCode: my-slug*"))))

(ert-deftest test-opencode-session-mode/buffer-name-fallback-id ()
  "Fall back to ID when title and slug are empty."
  (let ((session (opencode-session-create :id "abc123")))
    (should (equal (opencode-session--buffer-name session)
                   "*OpenCode: abc123*"))))

(ert-deftest test-opencode-session-mode/buffer-name-fallback-default ()
  "Fall back to 'session' when everything is nil."
  (let ((session (opencode-session-create)))
    (should (equal (opencode-session--buffer-name session)
                   "*OpenCode: session*"))))

(ert-deftest test-opencode-session-mode/buffer-name-trims-whitespace ()
  "Trim whitespace from title."
  (let ((session (opencode-session-create :title "  ")))
    ;; Empty after trim, should fall back
    (should (equal (opencode-session--buffer-name session)
                   "*OpenCode: session*"))))

;;; message-from-info

(ert-deftest test-opencode-session-mode/message-from-info ()
  "Create a message from an info alist."
  (let ((msg (opencode-session--message-from-info
              '((id . "m1")
                (sessionID . "s1")
                (role . "assistant")
                (providerID . "anthropic")
                (modelID . "claude-3")
                (time . ((created . "2024-01-01")))))))
    (should (opencode-message-p msg))
    (should (equal (opencode-message-id msg) "m1"))
    (should (equal (opencode-message-role msg) "assistant"))
    (should (equal (opencode-message-provider-id msg) "anthropic"))
    (should (equal (opencode-message-model-id msg) "claude-3"))))

(ert-deftest test-opencode-session-mode/message-from-info-nil ()
  "Return nil for nil info."
  (should (null (opencode-session--message-from-info nil))))

(ert-deftest test-opencode-session-mode/message-from-info-v2 ()
  "Map a V2 message alist: type as role, nested model, time.created."
  (let ((msg (opencode-session--message-from-info
              '((id . "msg_1")
                (sessionID . "s1")
                (type . "assistant")
                (agent . "build")
                (model . ((id . "claude")
                          (providerID . "anthropic")
                          (variant . "thinking")))
                (time . ((created . 1700000000000)
                         (completed . 1700000001000)))
                (finish . "stop")))))
    (should (equal (opencode-message-role msg) "assistant"))
    (should (equal (opencode-message-provider-id msg) "anthropic"))
    (should (equal (opencode-message-model-id msg) "claude"))
    (should (equal (opencode-message-agent msg) "build"))
    (should (equal (opencode-message-time-created msg) 1700000000000))
    (should (equal (opencode-message-time-completed msg) 1700000001000))
    (should (equal (opencode-message-finish msg) "stop"))))

;;; message-part-from-info

(ert-deftest test-opencode-session-mode/message-part-from-info ()
  "Create a message part from an info alist."
  (let ((part (opencode-session--message-part-from-info
               '((id . "p1")
                 (sessionID . "s1")
                 (messageID . "m1")
                 (type . "text")
                 (text . "hello")
                 (tool . "bash")
                 (time . ((start . "2024-01-01") (end . "2024-01-02")))))))
    (should (opencode-message-part-p part))
    (should (equal (opencode-message-part-id part) "p1"))
    (should (equal (opencode-message-part-type part) "text"))
    (should (equal (opencode-message-part-text part) "hello"))
    (should (equal (opencode-message-part-tool part) "bash"))
    (should (equal (opencode-message-part-time-start part) "2024-01-01"))))

(ert-deftest test-opencode-session-mode/message-part-from-info-v2-tool ()
  "Map a V2 tool part: name as tool, state.content as output."
  (let* ((info '((id . "call_1")
                 (sessionID . "s1")
                 (messageID . "m1")
                 (type . "tool")
                 (name . "bash")
                 (time . ((created . 1700000000000)
                          (completed . 1700000001000)))
                 (state . ((status . "completed")
                           (input . ((command . "echo hi")))
                           (content . [((type . "text") (text . "hi\n"))
                                       ((type . "text") (text . "bye"))])
                           (metadata . ((status . "completed")))))))
         (part (opencode-session--message-part-from-info info))
         (state (opencode-message-part-state part)))
    (should (equal (opencode-message-part-tool part) "bash"))
    (should (equal (opencode-message-part-time-start part) 1700000000000))
    (should (equal (opencode-message-part-time-end part) 1700000001000))
    (should (equal (alist-get 'status state) "completed"))
    (should (equal (alist-get 'output state) "hi\nbye"))
    (should (equal (alist-get 'input state) '((command . "echo hi"))))))

(ert-deftest test-opencode-session-mode/message-part-from-info-v2-no-content ()
  "A V2 tool part without content keeps its state unchanged."
  (let* ((info '((id . "call_1")
                 (type . "tool")
                 (name . "read")
                 (state . ((status . "running")
                           (input . ((path . "foo.el")))))))
         (part (opencode-session--message-part-from-info info)))
    (should (null (alist-get 'output (opencode-message-part-state part))))))

(ert-deftest test-opencode-session-mode/message-part-from-compaction-info ()
  "Create a compaction message part from an info alist."
  (let ((part (opencode-session--message-part-from-info
               '((id . "p1")
                 (sessionID . "s1")
                 (messageID . "m1")
                 (type . "compaction")
                 (auto . t)
                 (overflow . t)
                 (tail_start_id . "m-tail")))))
    (should (equal (opencode-message-part-type part) "compaction"))
    (should (eq (opencode-message-part-auto part) t))
    (should (eq (opencode-message-part-overflow part) t))
    (should (equal (opencode-message-part-tail-start-id part) "m-tail"))))

;;; command-items

(ert-deftest test-opencode-session-mode/command-items-vector ()
  "Normalize command vector to list."
  (should (equal (opencode-session--command-items [1 2]) '(1 2))))

(ert-deftest test-opencode-session-mode/command-items-list ()
  "Pass list through."
  (should (equal (opencode-session--command-items '(1 2)) '(1 2))))

(ert-deftest test-opencode-session-mode/command-items-nil ()
  "Return nil for nil."
  (should (null (opencode-session--command-items nil))))

;;; command-names

(ert-deftest test-opencode-session-mode/command-names ()
  "Extract command names from items."
  (should (equal (opencode-session--command-names
                  '(((name . "help") (description . "Show help"))
                    ((name . "clear") (description . "Clear"))))
                 '("help" "clear"))))

(ert-deftest test-opencode-session-mode/command-names-filters-non-lists ()
  "Filter out non-list items (delq removes nils)."
  (should (equal (opencode-session--command-names '("not-an-alist" ((name . "ok"))))
                 '("ok"))))

;;; hydrate-parts

(ert-deftest test-opencode-session-mode/hydrate-parts ()
  "Hydrate raw parts into an alist of part structs."
  (let ((result (opencode-session--hydrate-parts
                 '(((id . "p1") (type . "text") (text . "hello"))
                   ((id . "p2") (type . "tool") (tool . "bash"))))))
    (should (= (length result) 2))
    (should (equal (car (car result)) "p1"))
    (should (opencode-message-part-p (cdr (car result))))
    (should (equal (opencode-message-part-type (cdr (car result))) "text"))))

;;; hydrate-content (V2)

(ert-deftest test-opencode-session-mode/hydrate-content ()
  "Text and reasoning parts get positional ids; tool parts keep theirs."
  (let ((result (opencode-session--hydrate-content
                 '(((type . "reasoning") (text . "hmm"))
                   ((type . "tool") (id . "call_1") (name . "read")
                    (state . ((status . "completed")
                              (input . ((filePath . "a.el")))
                              (content . (((type . "text") (text . "out")))))))
                   ((type . "text") (text . "hello"))))))
    (should (equal (mapcar #'car result) '("reasoning-0" "call_1" "text-2")))
    (should (equal (opencode-message-part-type (cdr (nth 0 result))) "reasoning"))
    (should (equal (opencode-message-part-id (cdr (nth 1 result))) "call_1"))
    (should (equal (opencode-message-part-type (cdr (nth 2 result))) "text"))))

(ert-deftest test-opencode-session-mode/hydrate-content-list-input ()
  "Content given as a list hydrates the same way."
  (let ((result (opencode-session--hydrate-content
                 '(((type . "text") (text . "a"))
                   ((type . "text") (text . "b"))))))
    (should (equal (mapcar #'car result) '("text-0" "text-1")))))

;;; hydrate-message (V2 history hydration)

(ert-deftest test-opencode-session-mode/hydrate-message-v2-assistant ()
  "A V2 assistant message hydrates its content into parts."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-hydrate-assistant*")))
    (unwind-protect
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq-local opencode-session--session (opencode-session-create :id "s1"))
          (opencode-session--hydrate-message
           '((id . "msg_1")
             (type . "assistant")
             (time . ((created . 1000)))
             (content . (((type . "reasoning") (text . "hmm"))
                         ((type . "text") (text . "hi"))
                         ((type . "tool") (id . "call_1") (name . "read")
                          (state . ((status . "completed")
                                    (input . ((filePath . "a.el"))))))))))
          (let* ((message (car opencode-session--messages))
                 (parts (opencode-message-parts message)))
            (should (= (length opencode-session--messages) 1))
            (should (equal (opencode-message-role message) "assistant"))
            (should (equal (mapcar #'car parts)
                           '("reasoning-0" "text-1" "call_1")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-opencode-session-mode/hydrate-message-v2-user ()
  "A V2 user message carries its text on the message."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-hydrate-user*")))
    (unwind-protect
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq-local opencode-session--session (opencode-session-create :id "s1"))
          (opencode-session--hydrate-message
           '((id . "msg_2") (type . "user") (time . ((created . 2000))) (text . "hello")))
          (let ((message (car opencode-session--messages)))
            (should (equal (opencode-message-role message) "user"))
            (should (equal (opencode-message-text message) "hello"))
            (should (null (opencode-message-parts message)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-opencode-session-mode/hydrate-message-v2-compaction ()
  "A completed V2 compaction message attaches a boundary part."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-hydrate-compaction*")))
    (unwind-protect
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq-local opencode-session--session (opencode-session-create :id "s1"))
          (opencode-session--hydrate-message
           '((id . "msg_3") (type . "compaction") (status . "completed")
             (reason . "auto") (time . ((created . 3000)))))
          (let* ((message (car opencode-session--messages))
                 (entry (car (opencode-message-parts message)))
                 (part (cdr entry)))
            (should (equal (opencode-message-role message) "compaction"))
            (should (equal (car entry) "msg_3-compaction"))
            (should (equal (opencode-message-part-type part) "compaction"))
            (should (opencode-message-part-auto part))
            (should (opencode-message-part-time-end part))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-opencode-session-mode/hydrate-message-v2-shell ()
  "A V2 shell message renders as a command/output block."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-hydrate-shell*")))
    (unwind-protect
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq-local opencode-session--session (opencode-session-create :id "s1"))
          (opencode-session--hydrate-message
           '((id . "msg_4") (type . "shell") (command . "ls -la")
             (output . ((output . "a.el\nb.el\n")))
             (time . ((created . 4000)))))
          (should (equal (opencode-message-text (car opencode-session--messages))
                         "$ ls -la\na.el\nb.el\n")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-opencode-session-mode/hydrate-message-v1-shape ()
  "Legacy `(info parts)' items still hydrate through the fallback path."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (buffer (generate-new-buffer " *oc-hydrate-v1*")))
    (unwind-protect
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq-local opencode-session--session (opencode-session-create :id "s1"))
          (opencode-session--hydrate-message
           '((info . ((id . "msg_9") (sessionID . "s1") (type . "assistant")))
             (parts . [((id . "p1") (type . "text") (text . "legacy"))])))
          (let* ((message (car opencode-session--messages))
                 (part (cdr (car (opencode-message-parts message)))))
            (should (equal (opencode-message-role message) "assistant"))
            (should (equal (opencode-message-part-text part) "legacy"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; load-history (paged history hydration)

(ert-deftest test-opencode-session-mode/load-history-pages-till-exhausted ()
  "load-history follows cursor pages oldest-first and renders them."
  (let ((opencode-session--buffers (make-hash-table :test 'equal))
        (calls nil)
        (done nil)
        (buffer (generate-new-buffer " *oc-load-history*")))
    (unwind-protect
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq-local opencode-session--session (opencode-session-create :id "s1"))
          (let ((connection (opencode-connection-create
                             :base-url "http://127.0.0.1:9999")))
            (cl-letf (((symbol-function 'request)
                       (lambda (&rest request-args)
                         (let* ((url (car request-args))
                                (kwargs (cdr request-args))
                                (parser (plist-get kwargs :parser))
                                (success (plist-get kwargs :success))
                                (params (plist-get kwargs :params))
                                (cursor (cdr (assoc "cursor" params)))
                                (order (cdr (assoc "order" params))))
                           ;; Only the message list requests come through
                           ;; here; the form sync that follows finalizes
                           ;; the buffer through a separate endpoint and
                           ;; must not be counted against the page calls.
                           (if (not (string-match-p "/message" url))
                               (funcall success :data nil)
                             (push (list order cursor) calls)
                             (with-temp-buffer
                               (insert (json-encode
                                        (if (null cursor)
                                            '((data . [((id . "m1") (type . "user") (text . "one"))
                                                       ((id . "m2") (type . "user") (text . "two"))])
                                              (cursor . ((next . "cur2") (previous . nil))))
                                          '((data . [((id . "m3") (type . "user") (text . "three"))])
                                            (cursor . ((next . nil) (previous . "cur2")))))))
                               (goto-char (point-min))
                               (funcall success
                                        :data (when parser (funcall parser)))))))))
              (opencode-session--load-history
               connection opencode-session--session buffer
               (lambda (_b) (setq done t)))
              (should done)
              (should (= (length opencode-session--messages) 3))
              (should (equal (mapcar (lambda (m) (opencode-message-text m))
                                     opencode-session--messages)
                             '("one" "two" "three")))
              ;; The order parameter is sent only on the first page; the
              ;; cursor alone drives the following pages.
              (should (equal (nreverse calls)
                             (list (list "asc" nil)
                                   (list nil "cur2"))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))))

;;; classify-input

(ert-deftest test-opencode-session-mode/classify-input-message ()
  "Regular text is classified as a message."
  (let ((result (opencode-session--classify-input "hello world")))
    (should (eq (car result) 'message))
    (should (equal (cdr result) "hello world"))))

(ert-deftest test-opencode-session-mode/classify-input-command ()
  "Slash-prefixed text is classified as a command."
  (let ((result (opencode-session--classify-input "/help")))
    (should (eq (car result) 'command))
    (should (equal (cdr result) "/help"))))

(ert-deftest test-opencode-session-mode/classify-input-shell ()
  "Bang-prefixed text is classified as shell with prefix stripped."
  (let ((result (opencode-session--classify-input "!ls -la")))
    (should (eq (car result) 'shell))
    (should (equal (cdr result) "ls -la"))))

(ert-deftest test-opencode-session-mode/classify-input-shell-strips-only-bang ()
  "Only the leading ! is stripped from shell input."
  (let ((result (opencode-session--classify-input "!echo '!hello'")))
    (should (eq (car result) 'shell))
    (should (equal (cdr result) "echo '!hello'"))))

(ert-deftest test-opencode-session-mode/classify-input-shell-bare-bang ()
  "A bare ! is classified as shell with empty payload."
  (let ((result (opencode-session--classify-input "!")))
    (should (eq (car result) 'shell))
    (should (equal (cdr result) ""))))

(ert-deftest test-opencode-session-mode/classify-input-slash-priority ()
  "Slash takes priority when input starts with /."
  (let ((result (opencode-session--classify-input "/!mixed")))
    (should (eq (car result) 'command))))

(ert-deftest test-opencode-session-mode/classify-input-bang-not-at-start ()
  "A ! not at the start is treated as a regular message."
  (let ((result (opencode-session--classify-input "hello !world")))
    (should (eq (car result) 'message))
    (should (equal (cdr result) "hello !world"))))

;;; compact command

(ert-deftest test-opencode-session-mode/compact-sends-session ()
  "The compact command sends the current session."
  (with-temp-buffer
    (opencode-session-mode)
    (setq-local opencode-session--session (opencode-session-create :id "s1"))
    (setq-local opencode-session--provider-id "anthropic")
    (setq-local opencode-session--model-id "claude")
    (let (sent-connection sent-session)
      (cl-letf (((symbol-function 'opencode-session--ensure-connection)
                 (lambda (callback) (funcall callback 'conn)))
                ((symbol-function 'opencode-client-session-compact)
                 (lambda (connection session-id &rest _args)
                   (setq sent-connection connection
                         sent-session session-id))))
        (opencode-session-compact))
      (should (eq sent-connection 'conn))
      (should (equal sent-session "s1")))))

;;; rename command

(ert-deftest test-opencode-session-mode/rename-updates-session-and-buffer-name ()
  "The rename command sends and applies the new session title."
  (let ((buffer (generate-new-buffer "*OpenCode: Old title*")))
    (unwind-protect
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq-local opencode-session--session
                      (opencode-session-create :id "s1" :title "Old title"))
          (let (sent-connection sent-session sent-title)
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _args) "New title"))
                      ((symbol-function 'opencode-session--ensure-connection)
                       (lambda (callback) (funcall callback 'conn)))
                      ((symbol-function 'opencode-client-session-rename)
                       (lambda (connection session-id title &rest args)
                         (setq sent-connection connection
                               sent-session session-id
                               sent-title title)
                         (funcall (plist-get args :success)
                                  :data '((id . "s1")
                                          (title . "New title"))))))
              (opencode-session-rename))
            (should (eq sent-connection 'conn))
            (should (equal sent-session "s1"))
            (should (equal sent-title "New title"))
            (should (equal (opencode-session-title opencode-session--session)
                           "New title"))
            (should (equal (buffer-name) "*OpenCode: New title*"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-opencode-session-mode/rename-applies-title-without-response ()
  "Rename applies the title locally when the server returns no body."
  (let ((buffer (generate-new-buffer "*OpenCode: Old title*")))
    (unwind-protect
        (with-current-buffer buffer
          (opencode-session-mode)
          (setq-local opencode-session--session
                      (opencode-session-create :id "s1" :title "Old title"))
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _args) "New title"))
                    ((symbol-function 'opencode-session--ensure-connection)
                     (lambda (callback) (funcall callback 'conn)))
                    ((symbol-function 'opencode-client-session-rename)
                     (lambda (_connection _session-id _title &rest args)
                       ;; V2 PATCH returns an empty body.
                       (funcall (plist-get args :success)))))
            (opencode-session-rename))
          (should (equal (opencode-session-title opencode-session--session)
                         "New title"))
          (should (equal (buffer-name) "*OpenCode: New title*")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; extract-agent-mentions

(ert-deftest test-opencode-session-mode/extract-mentions-single ()
  "Extract a single @-mention."
  (let ((opencode-session--connection
         (opencode-connection-create :agents-raw
          '(((id . "explore") (mode . "subagent") (hidden . nil))
            ((id . "general") (mode . "subagent") (hidden . nil))))))
    (should (equal (opencode-session--extract-agent-mentions "hello @explore")
                   '("explore")))))

(ert-deftest test-opencode-session-mode/extract-mentions-multiple ()
  "Extract multiple @-mentions."
  (let ((opencode-session--connection
         (opencode-connection-create :agents-raw
          '(((id . "explore") (mode . "subagent") (hidden . nil))
            ((id . "general") (mode . "subagent") (hidden . nil))))))
    (should (equal (opencode-session--extract-agent-mentions
                    "@explore do this @general do that")
                   '("explore" "general")))))

(ert-deftest test-opencode-session-mode/extract-mentions-dedup ()
  "Duplicate mentions are deduplicated."
  (let ((opencode-session--connection
         (opencode-connection-create :agents-raw
          '(((id . "explore") (mode . "subagent") (hidden . nil))))))
    (should (equal (opencode-session--extract-agent-mentions
                    "@explore and @explore again")
                   '("explore")))))

(ert-deftest test-opencode-session-mode/extract-mentions-unknown-agent ()
  "Unknown agent names are not extracted."
  (let ((opencode-session--connection
         (opencode-connection-create :agents-raw
          '(((id . "explore") (mode . "subagent") (hidden . nil))))))
    (should (null (opencode-session--extract-agent-mentions "hello @unknown")))))

(ert-deftest test-opencode-session-mode/extract-mentions-no-at ()
  "Input without @ returns empty list."
  (let ((opencode-session--connection
         (opencode-connection-create :agents-raw
          '(((id . "explore") (mode . "subagent") (hidden . nil))))))
    (should (null (opencode-session--extract-agent-mentions "hello world")))))

(ert-deftest test-opencode-session-mode/extract-mentions-at-start ()
  "Mention at the start of input is extracted."
  (let ((opencode-session--connection
         (opencode-connection-create :agents-raw
          '(((id . "explore") (mode . "subagent") (hidden . nil))))))
    (should (equal (opencode-session--extract-agent-mentions "@explore find files")
                   '("explore")))))

(ert-deftest test-opencode-session-mode/extract-mentions-mid-word ()
  "@ embedded in a word (no preceding whitespace) is not extracted."
  (let ((opencode-session--connection
         (opencode-connection-create :agents-raw
          '(((id . "explore") (mode . "subagent") (hidden . nil))))))
    (should (null (opencode-session--extract-agent-mentions "email@explore")))))

;;; agent-attachments

(ert-deftest test-opencode-session-mode/agent-attachments-none ()
  "Return no attachments without mentions."
  (let ((opencode-session--connection
         (opencode-connection-create :agents-raw
          '(((id . "explore") (mode . "subagent") (hidden . nil))))))
    (should (null (opencode-session--agent-attachments "hello world")))))

(ert-deftest test-opencode-session-mode/agent-attachments-with-mention ()
  "Return an agent attachment for an @-mention."
  (let ((opencode-session--connection
         (opencode-connection-create :agents-raw
          '(((id . "explore") (mode . "subagent") (hidden . nil))))))
    (should (equal (opencode-session--agent-attachments "hello @explore")
                   '(((name . "explore")))))))

;;; send-input

(ert-deftest test-opencode-session-mode/send-input-passes-text-and-attachments ()
  "Send input passes the raw text plus @-mention agent attachments."
  (with-temp-buffer
    (setq-local opencode-session--connection
                (opencode-connection-create :agents-raw
                 '(((id . "explore") (mode . "subagent") (hidden . nil)))))
    (let (captured)
      (cl-letf (((symbol-function 'opencode-client-session-prompt-async)
                 (lambda (_conn session-id text &rest args)
                   (setq captured (list session-id text
                                        (plist-get args :agents))))))
        (opencode-session--send-input
         'connection (opencode-session-create :id "ses_1") "hello @explore"))
      (should (equal captured
                     '("ses_1" "hello @explore" (((name . "explore")))))))))

;;; agent-completion-bounds

(ert-deftest test-opencode-session-mode/agent-bounds-at-trigger ()
  "Detect @ at the start of input."
  (with-temp-buffer
    (opencode-session-mode)
    (opencode-session--ensure-markers)
    (opencode-session--ensure-input-region)
    (goto-char (marker-position opencode-session--input-marker))
    (insert "@expl")
    (let ((bounds (opencode-session--agent-completion-bounds)))
      (should bounds)
      (should (= (car bounds)
                 (1+ (marker-position opencode-session--input-start-marker))))
      (should (= (cdr bounds) (point))))))

(ert-deftest test-opencode-session-mode/agent-bounds-after-space ()
  "Detect @ after a space in input."
  (with-temp-buffer
    (opencode-session-mode)
    (opencode-session--ensure-markers)
    (opencode-session--ensure-input-region)
    (goto-char (marker-position opencode-session--input-marker))
    (insert "hello @expl")
    (let ((bounds (opencode-session--agent-completion-bounds)))
      (should bounds))))

(ert-deftest test-opencode-session-mode/agent-bounds-no-trigger ()
  "Return nil when no @ is present."
  (with-temp-buffer
    (opencode-session-mode)
    (opencode-session--ensure-markers)
    (opencode-session--ensure-input-region)
    (goto-char (marker-position opencode-session--input-marker))
    (insert "hello world")
    (should (null (opencode-session--agent-completion-bounds)))))

(ert-deftest test-opencode-session-mode/agent-bounds-mid-word ()
  "Return nil when @ is not preceded by whitespace."
  (with-temp-buffer
    (opencode-session-mode)
    (opencode-session--ensure-markers)
    (opencode-session--ensure-input-region)
    (goto-char (marker-position opencode-session--input-marker))
    (insert "email@expl")
    (should (null (opencode-session--agent-completion-bounds)))))

;;; fork-message-id-at-point

(ert-deftest test-opencode-session-mode/fork-id-on-user-message ()
  "Forking from a user message uses that message ID."
  (with-temp-buffer
    (opencode-session-mode)
    (setq-local opencode-session--session
                (opencode-session-create :id "test-session"))
    (let ((user (opencode-message-create :id "u1" :role "user" :text "prompt"))
          (assistant (opencode-message-create :id "a1" :role "assistant"
                                             :parent-id "u1" :text "answer")))
      (setq-local opencode-session--messages (list user assistant))
      (opencode-session--render-buffer)
      (goto-char (marker-position (opencode-message-start-marker user)))
      (should (equal (opencode-session--fork-message-id-at-point) "u1")))))

(ert-deftest test-opencode-session-mode/fork-id-on-assistant-message ()
  "Forking from an assistant message uses its parent user message ID."
  (with-temp-buffer
    (opencode-session-mode)
    (setq-local opencode-session--session
                (opencode-session-create :id "test-session"))
    (let ((user (opencode-message-create :id "u1" :role "user" :text "prompt"))
          (assistant (opencode-message-create :id "a1" :role "assistant"
                                             :parent-id "u1" :text "answer")))
      (setq-local opencode-session--messages (list user assistant))
      (opencode-session--render-buffer)
      (goto-char (marker-position (opencode-message-start-marker assistant)))
      (should (equal (opencode-session--fork-message-id-at-point) "u1")))))

(ert-deftest test-opencode-session-mode/fork-id-in-input-area-is-nil ()
  "Forking from the input area omits MESSAGE-ID for a whole-session fork."
  (with-temp-buffer
    (opencode-session-mode)
    (setq-local opencode-session--session
                (opencode-session-create :id "test-session"))
    (let ((user (opencode-message-create :id "u1" :role "user" :text "prompt")))
      (setq-local opencode-session--messages (list user))
      (opencode-session--render-buffer)
      (goto-char (point-max))
      (should (null (opencode-session--fork-message-id-at-point))))))

(ert-deftest test-opencode-session-mode/retry-banner-renders-above-prompt ()
  "Retry banner inserts above the prompt and updates markers correctly.

Regression test: previously a second render of the banner would
collapse `opencode-session--input-start-marker' to the start of the
banner, causing the prompt overlay to render before the banner."
  (with-temp-buffer
    (opencode-session-mode)
    (setq-local opencode-session--session
                (opencode-session-create :id "test-session"))
    (opencode-session--ensure-markers)
    (opencode-session--ensure-input-region)
    ;; First render
    (setf (opencode-session-status opencode-session--session)
          (opencode-status-create :type "retry"
                                  :message "first message"
                                  :attempt 1))
    (opencode-session--render-retry-banner)
    (let ((banner-start-1 (marker-position opencode-session--retry-banner-start))
          (banner-end-1 (marker-position opencode-session--retry-banner-end))
          (input-start-1 (marker-position opencode-session--input-start-marker))
          (overlay-start-1 (overlay-start opencode-session--input-prompt-overlay)))
      (should (= banner-end-1 input-start-1))
      (should (= overlay-start-1 input-start-1))
      (should (< banner-start-1 banner-end-1))
      ;; Second render with a different message
      (setf (opencode-session-status opencode-session--session)
            (opencode-status-create :type "retry"
                                    :message "second message"
                                    :attempt 2))
      (opencode-session--render-retry-banner)
      (let ((banner-start-2 (marker-position opencode-session--retry-banner-start))
            (banner-end-2 (marker-position opencode-session--retry-banner-end))
            (input-start-2 (marker-position opencode-session--input-start-marker))
            (overlay-start-2 (overlay-start opencode-session--input-prompt-overlay)))
        ;; The prompt must remain anchored after the banner.
        (should (= banner-end-2 input-start-2))
        (should (= overlay-start-2 input-start-2))
        (should (< banner-start-2 banner-end-2))
        ;; Buffer text reflects the latest message and contains no prefix.
        (let ((banner-text (buffer-substring-no-properties
                            banner-start-2 banner-end-2)))
          (should (string-match-p "second message" banner-text))
          (should-not (string-match-p "OpenCode:" banner-text)))))))

(ert-deftest test-opencode-session-mode/retry-banner-clears-when-status-clears ()
  "Banner is removed and markers reset when status leaves retry."
  (with-temp-buffer
    (opencode-session-mode)
    (setq-local opencode-session--session
                (opencode-session-create :id "test-session"))
    (opencode-session--ensure-markers)
    (opencode-session--ensure-input-region)
    (setf (opencode-session-status opencode-session--session)
          (opencode-status-create :type "retry" :message "boom" :attempt 1))
    (opencode-session--render-retry-banner)
    (should (markerp opencode-session--retry-banner-start))
    (should (marker-position opencode-session--retry-banner-start))
    (setf (opencode-session-status opencode-session--session)
          (opencode-status-create :type "idle"))
    (opencode-session--render-retry-banner)
    (should (null opencode-session--retry-banner-start))
    (should (null opencode-session--retry-banner-end))))

(ert-deftest test-opencode-session-mode/current-input-survives-message-insert ()
  "Input remains readable when a message is inserted before it."
  (with-temp-buffer
    (opencode-session-mode)
    (opencode-session--ensure-markers)
    (opencode-session--ensure-input-region)
    (insert "typed input")
    (let ((message (opencode-message-create :id "m1" :role "assistant"
                                            :text "assistant text")))
      (opencode-session--render-message message)
      (should (equal (opencode-session--current-input) "typed input"))
      (should (string-match-p "assistant text"
                              (buffer-substring-no-properties
                               (point-min) (marker-position opencode-session--input-start-marker)))))))

(ert-deftest test-opencode-session-mode/clear-input-keeps-log ()
  "Clearing input deletes only text after the input boundary."
  (with-temp-buffer
    (opencode-session-mode)
    (opencode-session--ensure-markers)
    (opencode-session--ensure-input-region)
    (let ((message (opencode-message-create :id "m1" :role "assistant"
                                            :text "assistant text")))
      (opencode-session--render-message message))
    (goto-char (point-max))
    (insert "typed input")
    (opencode-session--clear-input)
    (should (equal (opencode-session--current-input) ""))
    (should (string-match-p "assistant text"
                            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-opencode-session-mode/streaming-rerender-preserves-message-point ()
  "Re-rendering a message does not force point back to input."
  (save-window-excursion
    (let ((buffer (get-buffer-create " *opencode-session-point-test*")))
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (erase-buffer)
            (opencode-session-mode)
            (opencode-session--ensure-markers)
            (opencode-session--ensure-input-region)
            (let* ((prefix (mapconcat (lambda (n) (format "line %03d" n))
                                      (number-sequence 1 80)
                                      "\n"))
                   (suffix (mapconcat (lambda (n) (format "line %03d" n))
                                      (number-sequence 81 160)
                                      "\n"))
                   (message (opencode-message-create
                             :id "m1"
                             :role "assistant"
                             :text (concat prefix "\nneedle old\n" suffix))))
              (opencode-session--render-message message)
              (goto-char (point-min))
              (search-forward "needle")
              (goto-char (match-beginning 0))
              (set-window-start (selected-window) (point-min) t)
              (setf (opencode-message-text message)
                    (concat prefix "\nneedle new\n" suffix))
              (opencode-session--render-message message)
              (should (looking-at-p "needle"))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest test-opencode-session-mode/rerender-with-bottom-visible-preserves-message-point ()
  "A bottom-visible window only follows input when point is in input."
  (save-window-excursion
    (let ((buffer (get-buffer-create " *opencode-session-bottom-visible-test*")))
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (erase-buffer)
            (opencode-session-mode)
            (opencode-session--ensure-markers)
            (opencode-session--ensure-input-region)
            (let ((message (opencode-message-create
                            :id "m1"
                            :role "assistant"
                            :text "before\nneedle old\nafter")))
              (opencode-session--render-message message)
              (goto-char (point-min))
              (search-forward "needle")
              (goto-char (match-beginning 0))
              (should (= (window-end (selected-window) t) (point-max)))
              (setf (opencode-message-text message) "before\nneedle new\nafter")
              (opencode-session--render-message message)
              (should (looking-at-p "needle"))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest test-opencode-session-mode/new-message-preserves-message-point ()
  "Inserting a new message does not force point back to input."
  (save-window-excursion
    (let ((buffer (get-buffer-create " *opencode-session-insert-point-test*")))
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (erase-buffer)
            (opencode-session-mode)
            (opencode-session--ensure-markers)
            (opencode-session--ensure-input-region)
            (let* ((text (concat (mapconcat (lambda (n) (format "line %03d" n))
                                            (number-sequence 1 80)
                                            "\n")
                                 "\nneedle\n"
                                 (mapconcat (lambda (n) (format "line %03d" n))
                                            (number-sequence 81 160)
                                            "\n")))
                   (first (opencode-message-create :id "m1"
                                                   :role "assistant"
                                                   :text text))
                   (second (opencode-message-create :id "m2"
                                                    :role "assistant"
                                                    :text "new response")))
              (opencode-session--render-message first)
              (goto-char (point-min))
              (search-forward "needle")
              (goto-char (match-beginning 0))
              (set-window-start (selected-window) (point-min) t)
              (opencode-session--render-message second)
              (should (looking-at-p "needle"))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest test-opencode-session-mode/toggle-reasoning-flips-state ()
  "Toggling reasoning flips the buffer-local visibility flag."
  (with-temp-buffer
    (opencode-session-mode)
    (setq-local opencode-session--session
                (opencode-session-create :id "test-session"))
    (opencode-session--ensure-markers)
    (opencode-session--ensure-input-region)
    (setq-local opencode-session--show-reasoning nil)
    (opencode-session-toggle-reasoning)
    (should (eq opencode-session--show-reasoning t))
    (opencode-session-toggle-reasoning)
    (should (eq opencode-session--show-reasoning nil))))

(ert-deftest test-opencode-session-mode/toggle-reasoning-preserves-input ()
  "Toggling reasoning after render does not pull transcript into input."
  (with-temp-buffer
    (opencode-session-mode)
    (setq-local opencode-session--session
                (opencode-session-create :id "test-session"))
    (opencode-session--ensure-markers)
    (opencode-session--ensure-input-region)
    (let ((message (opencode-message-create
                    :id "m1"
                    :role "assistant"
                    :parts (list
                            (cons "p1"
                                  (opencode-message-part-create
                                   :id "p1"
                                   :type "text"
                                   :text "before tool"))
                            (cons "p2"
                                  (opencode-message-part-create
                                   :id "p2"
                                   :type "tool"
                                   :tool "grep"
                                   :state '((status . "completed")
                                            (input . ((pattern . "needle")))
                                            (metadata . ((matches . 1))))))
                            (cons "p3"
                                  (opencode-message-part-create
                                   :id "p3"
                                   :type "reasoning"
                                   :text "deep thoughts"))))))
      (setq-local opencode-session--messages (list message))
      (opencode-session--render-buffer))
    (goto-char (point-max))
    (insert "typed input")
    (setq-local opencode-session--show-reasoning nil)
    (opencode-session-toggle-reasoning)
    (should (eq opencode-session--show-reasoning t))
    (should (equal (opencode-session--current-input) "typed input"))
    (should (string-match-p
             "deep thoughts"
             (buffer-substring-no-properties
              (point-min)
              (marker-position opencode-session--input-start-marker))))))

(ert-deftest test-opencode-session-mode/toggle-reasoning-keeps-message-markers-before-input ()
  "Repeated reasoning toggles keep message regions before the input marker."
  (with-temp-buffer
    (opencode-session-mode)
    (setq-local opencode-session--session
                (opencode-session-create :id "test-session"))
    (opencode-session--ensure-markers)
    (opencode-session--ensure-input-region)
    (let ((first (opencode-message-create
                  :id "m1"
                  :role "assistant"
                  :parts (list (cons "p1"
                                     (opencode-message-part-create
                                      :id "p1"
                                      :type "text"
                                      :text "first response")))))
          (second (opencode-message-create
                   :id "m2"
                   :role "assistant"
                   :parts (list
                           (cons "p2"
                                 (opencode-message-part-create
                                  :id "p2"
                                  :type "tool"
                                  :tool "read"
                                  :state '((status . "completed")
                                           (input . ((filePath . "/tmp/file"))))))
                           (cons "p3"
                                 (opencode-message-part-create
                                  :id "p3"
                                  :type "reasoning"
                                  :text "thoughts"))))))
      (setq-local opencode-session--messages (list first second))
      (opencode-session--render-buffer)
      (goto-char (point-max))
      (insert "typed input")
      (setq-local opencode-session--show-reasoning nil)
      (opencode-session-toggle-reasoning)
      (opencode-session-toggle-reasoning)
      (let ((input-start (marker-position opencode-session--input-start-marker)))
        (should (equal (opencode-session--current-input) "typed input"))
        (dolist (message opencode-session--messages)
          (should (< (marker-position (opencode-message-start-marker message))
                     (marker-position (opencode-message-end-marker message))))
          (should (<= (marker-position (opencode-message-end-marker message))
                      input-start)))
        (should (string-match-p
                 "first response"
                 (buffer-substring-no-properties (point-min) input-start)))
        (should (string-match-p
                 "Read"
                 (buffer-substring-no-properties (point-min) input-start)))))))

(ert-deftest test-opencode-session-mode/toggle-reasoning-hides-on-second-toggle ()
  "Toggling reasoning off and back on restores reasoning text."
  (with-temp-buffer
    (opencode-session-mode)
    (setq-local opencode-session--session
                (opencode-session-create :id "test-session"))
    (opencode-session--ensure-markers)
    (opencode-session--ensure-input-region)
    (let ((message (opencode-message-create
                    :id "m1"
                    :role "assistant"
                    :parts (list (cons "p1"
                                       (opencode-message-part-create
                                        :id "p1"
                                        :type "reasoning"
                                        :text "deep thoughts"))))))
      (setq-local opencode-session--messages (list message)))
    (setq-local opencode-session--show-reasoning nil)
    (opencode-session-toggle-reasoning)
    (opencode-session-toggle-reasoning)
    (should (eq opencode-session--show-reasoning nil))
    (should-not (string-match-p
                 "deep thoughts"
                 (buffer-substring-no-properties (point-min) (point-max))))
    (opencode-session-toggle-reasoning)
    (should (eq opencode-session--show-reasoning t))
    (should (string-match-p
             "deep thoughts"
             (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-opencode-session-mode/reasoning-update-while-hidden-is-retained ()
  "Reasoning text updated while hidden reappears when toggled back on."
  (with-temp-buffer
    (opencode-session-mode)
    (setq-local opencode-session--session
                (opencode-session-create :id "test-session"))
    (opencode-session--ensure-markers)
    (opencode-session--ensure-input-region)
    (let ((message (opencode-message-create
                    :id "m1"
                    :role "assistant"
                    :parts (list (cons "p1"
                                       (opencode-message-part-create
                                        :id "p1"
                                        :session-id "test-session"
                                        :message-id "m1"
                                        :type "reasoning"
                                        :text "first"))))))
      (setq-local opencode-session--messages (list message))
      (setq-local opencode-session--show-reasoning t)
      (opencode-session--render-buffer)
      (opencode-session-toggle-reasoning)
      (opencode-session--update-message-part
       '((id . "p1")
         (sessionID . "test-session")
         (messageID . "m1")
         (type . "reasoning"))
       " second")
      (opencode-session-toggle-reasoning)
      (should (string-match-p
               "first second"
               (buffer-substring-no-properties (point-min) (point-max)))))))

;;; tool output drawer

(defun opencode-session-mode-test--tool-buffer-setup (state)
  "Prepare the current buffer with one completed grep tool part in STATE.
Leaves point on the tool summary line."
  (opencode-session-mode)
  (setq-local opencode-session--session
              (opencode-session-create :id "test-session"))
  (setq-local opencode-session--connection
              (opencode-connection-create :base-url "http://127.0.0.1:1"))
  (opencode-session--ensure-markers)
  (opencode-session--ensure-input-region)
  (setq-local opencode-session--messages
              (list (opencode-message-create
                     :id "m1"
                     :session-id "test-session"
                     :role "assistant"
                     :parts (list (cons "p1"
                                        (opencode-message-part-create
                                         :id "p1"
                                         :session-id "test-session"
                                         :message-id "m1"
                                         :type "tool"
                                         :tool "grep"
                                         :state state))))))
  (opencode-session--render-buffer)
  (goto-char (point-min))
  (search-forward "Grep"))

(defun opencode-session-mode-test--transcript ()
  "Return the rendered transcript above the input area."
  (buffer-substring-no-properties
   (point-min) (marker-position opencode-session--input-start-marker)))

(ert-deftest test-opencode-session-mode/toggle-tool-output-opens-and-closes ()
  "TAB on a tool summary shows its output, and again hides it."
  (with-temp-buffer
    (opencode-session-mode-test--tool-buffer-setup
     '((status . "completed")
       (input . ((pattern . "needle")))
       (output . "a.el:1:needle")))
    (should-not (string-match-p "a\\.el:1:needle"
                                (opencode-session-mode-test--transcript)))
    (opencode-session-toggle-tool-output)
    (should (string-match-p "a\\.el:1:needle"
                            (opencode-session-mode-test--transcript)))
    (should (string-match-p "Grep" (thing-at-point 'line t)))
    (opencode-session-toggle-tool-output)
    (should-not (string-match-p "a\\.el:1:needle"
                                (opencode-session-mode-test--transcript)))))

(ert-deftest test-opencode-session-mode/toggle-tool-output-survives-rerender ()
  "An open drawer stays open when the message re-renders."
  (with-temp-buffer
    (opencode-session-mode-test--tool-buffer-setup
     '((status . "completed") (output . "a.el:1:needle")))
    (opencode-session-toggle-tool-output)
    (opencode-session--render-message (car opencode-session--messages))
    (should (string-match-p "a\\.el:1:needle"
                            (opencode-session-mode-test--transcript)))))

(ert-deftest test-opencode-session-mode/toggle-tool-output-fetches-missing-output ()
  "Opening a drawer without output fetches the message and fills all parts."
  (with-temp-buffer
    (let (captured)
      (cl-letf (((symbol-function 'opencode-client-session-message)
                 (lambda (_conn session-id message-id &rest args)
                   (setq captured (list session-id message-id args)))))
        (opencode-session-mode-test--tool-buffer-setup
         '((status . "completed")))
        (opencode-session-toggle-tool-output)
        (should (equal (car captured) "test-session"))
        (should (equal (cadr captured) "m1"))
        (should (string-match-p "Loading"
                                (opencode-session-mode-test--transcript)))
        (funcall (plist-get (nth 2 captured) :success)
                 :data '((info . ((id . "m1")))
                         (parts . [((id . "p1")
                                    (type . "tool")
                                    (state . ((status . "completed")
                                              (output . "fetched"))))])))
        (should (string-match-p "fetched"
                                (opencode-session-mode-test--transcript)))
        (should-not (string-match-p "Loading"
                                    (opencode-session-mode-test--transcript)))))))

(ert-deftest test-opencode-session-mode/toggle-tool-output-fetch-v2-content ()
  "A V2 fetched message maps state.content into the drawer output."
  (with-temp-buffer
    (let (captured)
      (cl-letf (((symbol-function 'opencode-client-session-message)
                 (lambda (_conn _session-id _message-id &rest args)
                   (setq captured args)))
                ((symbol-function 'message) #'ignore))
        (opencode-session-mode-test--tool-buffer-setup
         '((status . "completed")))
        (opencode-session-toggle-tool-output)
        (should (string-match-p "Loading"
                                (opencode-session-mode-test--transcript)))
        (funcall (plist-get captured :success)
                 :data '((info . ((id . "m1")))
                         (content . [((id . "p1")
                                      (type . "tool")
                                      (name . "bash")
                                      (state . ((status . "completed")
                                                (content . [((type . "text")
                                                             (text . "fetched-v2"))]))))])))
        (should (string-match-p "fetched-v2"
                                (opencode-session-mode-test--transcript)))
        (should-not (string-match-p "Loading"
                                    (opencode-session-mode-test--transcript)))))))

(ert-deftest test-opencode-session-mode/toggle-tool-output-fetch-error-closes ()
  "A failed output fetch closes the drawer instead of leaving it loading."
  (with-temp-buffer
    (let (captured)
      (cl-letf (((symbol-function 'opencode-client-session-message)
                 (lambda (_conn _session-id _message-id &rest args)
                   (setq captured args)))
                ((symbol-function 'message) #'ignore))
        (opencode-session-mode-test--tool-buffer-setup
         '((status . "completed")))
        (opencode-session-toggle-tool-output)
        (funcall (plist-get captured :error))
        (should-not (string-match-p "Loading"
                                    (opencode-session-mode-test--transcript)))
        (should-not (opencode-session--tool-drawer-open-p "p1"))))))

(ert-deftest test-opencode-session-mode/tool-output-survives-part-update ()
  "Fetched output is kept when a later part update lacks it."
  (with-temp-buffer
    (opencode-session-mode-test--tool-buffer-setup
     '((status . "completed") (output . "kept")))
    (opencode-session-toggle-tool-output)
    (opencode-session--update-message-part
     '((id . "p1")
       (sessionID . "test-session")
       (messageID . "m1")
       (type . "tool")
       (tool . "grep")
       (state . ((status . "completed") (time . ((compacted . 3))))))
     nil)
    (should (string-match-p "kept" (opencode-session-mode-test--transcript)))))

(ert-deftest test-opencode-session-mode/tool-output-update-with-output-wins ()
  "A part update that carries its own output replaces the stored one."
  (with-temp-buffer
    (opencode-session-mode-test--tool-buffer-setup
     '((status . "completed") (output . "old")))
    (opencode-session-toggle-tool-output)
    (opencode-session--update-message-part
     '((id . "p1")
       (sessionID . "test-session")
       (messageID . "m1")
       (type . "tool")
       (tool . "grep")
       (state . ((status . "completed") (output . "new"))))
     nil)
    (let ((transcript (opencode-session-mode-test--transcript)))
      (should (string-match-p "new" transcript))
      (should-not (string-match-p "old" transcript)))))

(ert-deftest test-opencode-session-mode/toggle-tool-output-fetch-without-output ()
  "A fetched part with no output shows the empty placeholder, not loading."
  (with-temp-buffer
    (let (captured)
      (cl-letf (((symbol-function 'opencode-client-session-message)
                 (lambda (_conn _session-id _message-id &rest args)
                   (setq captured args))))
        (opencode-session-mode-test--tool-buffer-setup
         '((status . "completed")))
        (opencode-session-toggle-tool-output)
        (funcall (plist-get captured :success)
                 :data '((parts . [((id . "p1")
                                    (type . "tool")
                                    (state . ((status . "completed"))))])))
        (should (string-match-p "(no output)"
                                (opencode-session-mode-test--transcript)))))))

(ert-deftest test-opencode-session-mode/toggle-tool-output-without-connection ()
  "Opening a drawer that needs a fetch with no connection leaves it closed."
  (with-temp-buffer
    (cl-letf (((symbol-function 'message) #'ignore))
      (opencode-session-mode-test--tool-buffer-setup '((status . "completed")))
      (setq-local opencode-session--connection nil)
      (opencode-session-toggle-tool-output)
      (should-not (opencode-session--tool-drawer-open-p "p1"))
      (should-not (string-match-p "Loading"
                                  (opencode-session-mode-test--transcript))))))

(ert-deftest test-opencode-session-mode/toggle-tool-output-outside-tool ()
  "Toggling away from a tool part is a no-op."
  (with-temp-buffer
    (opencode-session-mode-test--tool-buffer-setup
     '((status . "completed") (output . "x")))
    (goto-char (point-max))
    (opencode-session-toggle-tool-output)
    (should-not (opencode-session--tool-drawer-open-p "p1"))))

(provide 'emacs-opencode-session-mode-test)

;;; emacs-opencode-session-mode-test.el ends here
