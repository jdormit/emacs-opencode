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

(ert-deftest test-opencode-session-mode/message-from-info-assistant ()
  "Assistant messages map their content to text, reasoning, and tool parts."
  (let* ((msg (opencode-session--message-from-info
               '((id . "m1")
                 (type . "assistant")
                 (agent . "build")
                 (model . ((id . "claude") (providerID . "anthropic") (variant . "high")))
                 (time . ((created . 10) (completed . 20)))
                 (cost . 0.5)
                 (content . (((type . "reasoning") (text . "hmm"))
                             ((type . "text") (text . "hello"))
                             ((type . "tool") (id . "call_1") (name . "grep")
                              (state . ((status . "completed")
                                        (input . ((pattern . "x")))
                                        (content . (((type . "text") (text . "a\n"))
                                                    ((type . "text") (text . "b"))))
                                        (metadata . ((matches . 2)))))))))
               "s1"))
         (parts (opencode-message-parts msg)))
    (should (equal (opencode-message-role msg) "assistant"))
    (should (equal (opencode-message-provider-id msg) "anthropic"))
    (should (equal (opencode-message-model-id msg) "claude"))
    (should (equal (opencode-message-agent msg) "build"))
    (should (equal (opencode-message-time-completed msg) 20))
    (should (equal (mapcar #'car parts) '("m1:0" "m1:1" "call_1")))
    (should (equal (opencode-message-part-type (cdr (nth 0 parts))) "reasoning"))
    (should (equal (opencode-message-part-text (cdr (nth 1 parts))) "hello"))
    (let ((tool (cdr (nth 2 parts))))
      (should (equal (opencode-message-part-tool tool) "grep"))
      (should (equal (opencode-message-part-session-id tool) "s1"))
      (should (equal (alist-get 'output (opencode-message-part-state tool)) "a\nb"))
      (should (equal (alist-get 'matches (alist-get 'metadata (opencode-message-part-state tool))) 2)))))

(ert-deftest test-opencode-session-mode/message-from-info-user ()
  "User messages become a single text part."
  (let ((msg (opencode-session--message-from-info
              '((id . "u1") (type . "user") (text . "hi")) "s1")))
    (should (equal (opencode-message-role msg) "user"))
    (should (equal (opencode-message-part-text (cdar (opencode-message-parts msg))) "hi"))))

(ert-deftest test-opencode-session-mode/message-from-info-shell ()
  "User shell commands render as a shell tool part."
  (let* ((msg (opencode-session--message-from-info
               '((id . "sh1") (type . "shell") (command . "ls") (status . "exited")
                 (exit . 0) (output . ((output . "a.txt\n"))))
               "s1"))
         (state (opencode-message-part-state (cdar (opencode-message-parts msg)))))
    (should (equal (opencode-message-role msg) "shell"))
    (should (equal (alist-get 'status state) "completed"))
    (should (equal (alist-get 'command (alist-get 'input state)) "ls"))
    (should (equal (alist-get 'output state) "a.txt\n"))))

(ert-deftest test-opencode-session-mode/message-from-info-compaction ()
  "Compaction messages carry their status, reason, and error."
  (let* ((msg (opencode-session--message-from-info
               '((id . "c1") (type . "compaction") (status . "failed") (reason . "auto")
                 (error . ((type . "x") (message . "too big"))))
               "s1"))
         (part (cdar (opencode-message-parts msg))))
    (should (equal (opencode-message-part-type part) "compaction"))
    (should (opencode-message-part-auto part))
    (should (equal (opencode-message-part-metadata part)
                   '((status . "failed") (error . "too big"))))))

(ert-deftest test-opencode-session-mode/message-from-info-loaded-instructions ()
  "Synthetic messages that load instruction files are shown."
  (let ((msg (opencode-session--message-from-info
              '((id . "y1") (type . "synthetic")
                (metadata . ((instruction . ((paths . ("/p/AGENTS.md")))))))
              "s1")))
    (should (equal (opencode-message-role msg) "system"))
    (should (equal (alist-get 'paths (opencode-message-part-metadata
                                      (cdar (opencode-message-parts msg))))
                   '("/p/AGENTS.md")))))

(ert-deftest test-opencode-session-mode/message-from-info-hidden-types ()
  "Bookkeeping messages are not displayed."
  (dolist (info '(((id . "a") (type . "idle"))
                  ((id . "b") (type . "agent-switched"))
                  ((id . "c") (type . "model-switched"))
                  ((id . "d") (type . "system"))
                  ((id . "e") (type . "synthetic") (metadata . ((source . "shell"))))))
    (should-not (opencode-session--message-from-info info "s1"))))

(ert-deftest test-opencode-session-mode/message-from-info-nil ()
  "Return nil for nil info."
  (should (null (opencode-session--message-from-info nil))))

;;; tool-state

(ert-deftest test-opencode-session-mode/tool-state-streaming-is-pending ()
  "Streaming tool calls render as pending and drop their raw JSON input."
  (should (equal (opencode-session--tool-state
                  '((status . "streaming") (input . "{\"a\"")))
                 '((status . "pending")))))

(ert-deftest test-opencode-session-mode/tool-state-error-message ()
  "Structured tool errors become their message."
  (should (equal (alist-get 'error (opencode-session--tool-state
                                    '((status . "error") (input . ((a . 1)))
                                      (error . ((type . "t") (message . "boom"))))))
                 "boom")))

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
  "The compact command queues compaction for the current session."
  (with-temp-buffer
    (opencode-session-mode)
    (setq-local opencode-session--session (opencode-session-create :id "s1"))
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

;;; background command

(ert-deftest test-opencode-session-mode/background-sends-session ()
  "The background command moves blocking work for the current session."
  (with-temp-buffer
    (opencode-session-mode)
    (setq-local opencode-session--session
                (opencode-session-create :id "s1" :info '((id . "s1"))))
    (let (sent-session)
      (cl-letf (((symbol-function 'opencode-session--ensure-connection)
                 (lambda (callback) (funcall callback 'conn)))
                ((symbol-function 'opencode-client-session-background)
                 (lambda (_connection session-id &rest _args)
                   (setq sent-session session-id))))
        (opencode-session-background))
      (should (equal sent-session "s1")))))

(ert-deftest test-opencode-session-mode/background-noop-in-subagent ()
  "The background command warns and does nothing in a subagent session."
  (dolist (session (list (opencode-session-create
                          :id "child" :info '((id . "child") (parentID . "parent")))
                         (opencode-session-create :id "registered-child")))
    (with-temp-buffer
      (opencode-session-mode)
      (setq-local opencode-session--session session)
      (let ((opencode-session--subagent-parents (make-hash-table :test 'equal))
            called warned)
        (opencode-session--register-subagent "registered-child" "parent" "part")
        (cl-letf (((symbol-function 'opencode-session--ensure-connection)
                   (lambda (callback) (funcall callback 'conn)))
                  ((symbol-function 'opencode-client-session-background)
                   (lambda (&rest _args) (setq called t)))
                  ((symbol-function 'message)
                   (lambda (format &rest args)
                     (setq warned (apply #'format-message format args)))))
          (opencode-session-background))
        (should-not called)
        (should (string-match-p "parent session" warned))))))

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

;;; send-input

(ert-deftest test-opencode-session-mode/send-input-sends-mentioned-agents ()
  "Prompts include the @-mentioned subagents."
  (let ((opencode-session--connection
         (opencode-connection-create :agents-raw
          '(((id . "explore") (mode . "subagent") (hidden . nil)))))
        sent)
    (cl-letf (((symbol-function 'opencode-client-session-prompt)
               (lambda (_conn session-id text &rest args)
                 (setq sent (list session-id text (plist-get args :agents))))))
      (opencode-session--send-input opencode-session--connection
                                    (opencode-session-create :id "s1")
                                    "hello @explore"))
    (should (equal sent '("s1" "hello @explore" ("explore"))))))

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
      (puthash "test-session" (current-buffer) opencode-session--buffers)
      (unwind-protect
          (opencode-session--handle-content-text
           "session.reasoning.delta"
           '((data . ((sessionID . "test-session")
                      (assistantMessageID . "m1")
                      (delta . " second")))))
        (remhash "test-session" opencode-session--buffers))
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
                 :data '((id . "m1")
                         (type . "assistant")
                         (content . (((type . "tool") (id . "p1") (name . "grep")
                                      (state . ((status . "completed")
                                                (content . (((type . "text")
                                                             (text . "fetched")))))))))))
        (should (string-match-p "fetched"
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

(ert-deftest test-opencode-session-mode/tool-output-survives-terminal-event ()
  "Fetched output is kept when the terminal tool event arrives without it."
  (with-temp-buffer
    (opencode-session-mode-test--tool-buffer-setup
     '((status . "running") (output . "kept")))
    (opencode-session--apply-tool-event
     (car opencode-session--messages) "session.tool.success"
     '((id . "p1") (sessionID . "test-session") (metadata . ((matches . 1)))))
    (let ((state (opencode-message-part-state
                  (cdar (opencode-message-parts (car opencode-session--messages))))))
      (should (equal (alist-get 'status state) "completed"))
      (should (equal (alist-get 'output state) "kept")))))

(ert-deftest test-opencode-session-mode/tool-output-from-event-wins ()
  "A terminal event that carries output replaces the stored one."
  (with-temp-buffer
    (opencode-session-mode-test--tool-buffer-setup
     '((status . "running") (output . "old")))
    (opencode-session--apply-tool-event
     (car opencode-session--messages) "session.tool.success"
     '((id . "p1") (content . (((type . "text") (text . "new"))))))
    (should (equal (alist-get 'output (opencode-message-part-state
                                       (cdar (opencode-message-parts
                                              (car opencode-session--messages)))))
                   "new"))))

(ert-deftest test-opencode-session-mode/reload-refetches-open-drawer-output ()
  "Reloading history fetches output for drawers that stay open."
  (with-temp-buffer
    (let (fetched)
      (cl-letf (((symbol-function 'opencode-client-session-message)
                 (lambda (_conn _session-id message-id &rest _args)
                   (setq fetched message-id)))
                ((symbol-function 'opencode-client-session-inbox)
                 (lambda (_conn _session-id &rest args)
                   (funcall (plist-get args :success) :data nil)))
                ((symbol-function 'opencode-client-session-messages)
                 (lambda (_conn _session-id &rest args)
                   (funcall (plist-get args :success)
                            :data '(((id . "m1") (type . "assistant")
                                     (content . (((type . "tool") (id . "p1") (name . "grep")
                                                  (state . ((status . "completed")
                                                            (input . ((pattern . "x"))))))))))))))
        (opencode-session-mode-test--tool-buffer-setup
         '((status . "completed") (output . "x")))
        (opencode-session-toggle-tool-output)
        (opencode-session--load-history opencode-session--connection
                                        opencode-session--session
                                        (current-buffer))
        (should (equal fetched "m1"))))))

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
                 :data '((id . "m1")
                         (type . "assistant")
                         (content . (((type . "tool") (id . "p1") (name . "grep")
                                      (state . ((status . "completed"))))))))
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
