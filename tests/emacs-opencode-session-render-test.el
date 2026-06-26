;;; emacs-opencode-session-render-test.el --- Tests for session rendering  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-opencode-session-render)

;;; strip-ansi

(ert-deftest test-opencode-render/strip-ansi-removes-escapes ()
  "Remove ANSI escape codes from a string."
  (should (equal (opencode-session--strip-ansi "\e[31mred\e[0m") "red")))

(ert-deftest test-opencode-render/strip-ansi-plain ()
  "Plain strings pass through unchanged."
  (should (equal (opencode-session--strip-ansi "hello") "hello")))

(ert-deftest test-opencode-render/strip-ansi-nil ()
  "Return nil for nil input."
  (should (null (opencode-session--strip-ansi nil))))

;;; nonempty-string

(ert-deftest test-opencode-render/nonempty-string-returns-value ()
  "Return the string when non-empty."
  (should (equal (opencode-session--nonempty-string "hello") "hello")))

(ert-deftest test-opencode-render/nonempty-string-empty ()
  "Return nil for empty string."
  (should (null (opencode-session--nonempty-string ""))))

(ert-deftest test-opencode-render/nonempty-string-nil ()
  "Return nil for nil."
  (should (null (opencode-session--nonempty-string nil))))

(ert-deftest test-opencode-render/nonempty-string-number ()
  "Return nil for non-string input."
  (should (null (opencode-session--nonempty-string 42))))

;;; format-input-params

(ert-deftest test-opencode-render/format-input-params-basic ()
  "Format an alist of primitives."
  (should (equal (opencode-session--format-input-params
                  '((name . "foo") (count . 3)))
                 "[name=foo, count=3]")))

(ert-deftest test-opencode-render/format-input-params-booleans ()
  "Format boolean values."
  (should (equal (opencode-session--format-input-params
                  '((flag . t) (off . :json-false)))
                 "[flag=true, off=false]")))

(ert-deftest test-opencode-render/format-input-params-skips-complex ()
  "Skip non-primitive values."
  (should (equal (opencode-session--format-input-params
                  '((name . "foo") (nested . ((a . 1)))))
                 "[name=foo]")))

(ert-deftest test-opencode-render/format-input-params-empty ()
  "Return nil for empty alist."
  (should (null (opencode-session--format-input-params nil))))

;;; format-count

(ert-deftest test-opencode-render/format-count-basic ()
  "Format a match count."
  (should (equal (opencode-session--format-count 5 nil) "(5 matches)")))

(ert-deftest test-opencode-render/format-count-truncated ()
  "Format a truncated match count."
  (should (equal (opencode-session--format-count 100 t) "(100+ matches)")))

(ert-deftest test-opencode-render/format-count-nil ()
  "Return nil when count is nil."
  (should (null (opencode-session--format-count nil nil))))

;;; format-args

(ert-deftest test-opencode-render/format-args-basic ()
  "Format a list of args."
  (should (equal (opencode-session--format-args '("a=1" "b=2"))
                 "[a=1, b=2]")))

(ert-deftest test-opencode-render/format-args-empty ()
  "Return nil for empty args."
  (should (null (opencode-session--format-args nil))))

(ert-deftest test-opencode-render/format-args-with-nils ()
  "Filter nil entries."
  (should (equal (opencode-session--format-args '(nil "a=1" nil))
                 "[a=1]")))

;;; format-quoted

(ert-deftest test-opencode-render/format-quoted ()
  "Wrap a string in quotes."
  (should (equal (opencode-session--format-quoted "foo") "\"foo\"")))

(ert-deftest test-opencode-render/format-quoted-nil ()
  "Return nil for nil."
  (should (null (opencode-session--format-quoted nil))))

;;; todo-marker

(ert-deftest test-opencode-render/todo-marker-completed ()
  "Completed status returns checkmark."
  (should (equal (opencode-session--todo-marker "completed") "✓")))

(ert-deftest test-opencode-render/todo-marker-in-progress ()
  "In-progress status returns bullet."
  (should (equal (opencode-session--todo-marker "in_progress") "•")))

(ert-deftest test-opencode-render/todo-marker-pending ()
  "Other statuses return space."
  (should (equal (opencode-session--todo-marker "pending") " ")))

;;; tool-attach-status

(ert-deftest test-opencode-render/attach-status-pending ()
  "Append [pending] suffix."
  (should (equal (opencode-session--tool-attach-status "✱ Shell ls" "pending")
                 "✱ Shell ls [pending]")))

(ert-deftest test-opencode-render/attach-status-completed ()
  "Completed status is not appended."
  (should (equal (opencode-session--tool-attach-status "✱ Shell ls" "completed")
                 "✱ Shell ls")))

(ert-deftest test-opencode-render/attach-status-already-present ()
  "Don't duplicate an existing status suffix."
  (should (equal (opencode-session--tool-attach-status "✱ Shell ls [running]" "running")
                 "✱ Shell ls [running]")))

;;; tool-error-line

(ert-deftest test-opencode-render/tool-error-line-with-error ()
  "Return error string when status is error."
  (should (equal (opencode-session--tool-error-line "error"
                   '((error . "something broke")))
                 "something broke")))

(ert-deftest test-opencode-render/tool-error-line-no-error ()
  "Return nil when status is not error."
  (should (null (opencode-session--tool-error-line "completed"
                  '((error . "something broke"))))))

;;; tool summary functions

(ert-deftest test-opencode-render/tool-glob ()
  "Render glob tool summary."
  (let ((result (opencode-session--tool-glob
                 '((pattern . "*.el") (path . "/src"))
                 '((count . 5) (truncated . nil)))))
    (should (string-match-p "Glob" result))
    (should (string-match-p "\\*\\.el" result))
    (should (string-match-p "(5 matches)" result))))

(ert-deftest test-opencode-render/tool-grep ()
  "Render grep tool summary."
  (let ((result (opencode-session--tool-grep
                 '((pattern . "TODO") (path . "/src"))
                 '((matches . 12) (truncated . t)))))
    (should (string-match-p "Grep" result))
    (should (string-match-p "TODO" result))
    (should (string-match-p "(12\\+ matches)" result))))

(ert-deftest test-opencode-render/tool-read ()
  "Render read tool summary."
  (let ((opencode-session--connection nil))
    (let ((result (opencode-session--tool-read '((filePath . "foo.el")))))
      (should (string-match-p "Read" result))
      (should (string-match-p "foo\\.el" result)))))

(ert-deftest test-opencode-render/tool-read-with-offset ()
  "Render read tool with offset/limit args."
  (let ((opencode-session--connection nil))
    (let ((result (opencode-session--tool-read
                   '((filePath . "foo.el") (offset . 10) (limit . 20)))))
      (should (string-match-p "offset=10" result))
      (should (string-match-p "limit=20" result)))))

(ert-deftest test-opencode-render/tool-bash-description ()
  "Render bash tool with description."
  (should (equal (opencode-session--tool-bash
                  '((description . "list files") (command . "ls -la"))
                  nil)
                 "✱ Shell list files")))

(ert-deftest test-opencode-render/tool-bash-command ()
  "Render bash tool with only command."
  (should (equal (opencode-session--tool-bash
                  '((command . "ls -la"))
                  nil)
                 "✱ Shell ls -la")))

(ert-deftest test-opencode-render/tool-bash-empty ()
  "Render bash tool with no input."
  (should (equal (opencode-session--tool-bash nil nil)
                 "✱ Shell")))

(ert-deftest test-opencode-render/tool-webfetch ()
  "Render webfetch tool summary."
  (let ((result (opencode-session--tool-webfetch
                 '((url . "https://example.com") (format . "markdown")))))
    (should (string-match-p "Webfetch" result))
    (should (string-match-p "example\\.com" result))
    (should (string-match-p "format=markdown" result))))

(ert-deftest test-opencode-render/tool-generic ()
  "Render generic tool summary."
  (let ((result (opencode-session--tool-generic
                 "my_tool"
                 '((query . "hello"))
                 "completed"
                 nil)))
    (should (string-match-p "my_tool" result))
    (should (string-match-p "query=hello" result))))

(ert-deftest test-opencode-render/tool-edit-write ()
  "Render edit/write tool summary."
  (let ((opencode-session--connection nil))
    (should (equal (opencode-session--tool-edit-write
                    "Edit" '((filePath . "src/main.el")) nil)
                   "→ Edit src/main.el"))))

(ert-deftest test-opencode-render/tool-todos ()
  "Render todo list."
  (let ((result (opencode-session--tool-todos
                 "# Todos"
                 '((todos . [((status . "completed") (content . "first"))
                             ((status . "pending") (content . "second"))]))
                 nil)))
    (should (string-match-p "# Todos" result))
    (should (string-match-p "\\[✓\\] first" result))
    (should (string-match-p "\\[ \\] second" result))))

(ert-deftest test-opencode-render/compaction-part ()
  "Render compaction parts as boundary markers."
  (let* ((part (opencode-message-part-create
                :id "p1"
                :type "compaction"
                :auto nil
                :time-end t))
         (result (opencode-session--render-message-part
                  (opencode-message-create :id "m1" :role "user")
                  part)))
    (should (string-match-p "Session compacted" result))
    (should (string-match-p "no longer sent verbatim" result))))

;;; tool-extract-todos

(ert-deftest test-opencode-render/extract-todos-from-input ()
  "Extract todos from input."
  (let ((result (opencode-session--tool-extract-todos
                 '((todos . [((status . "pending") (content . "do it"))]))
                 nil)))
    (should (= (length result) 1))
    (should (equal (alist-get 'content (car result)) "do it"))))

(ert-deftest test-opencode-render/extract-todos-from-metadata ()
  "Fall back to metadata when input has no todos."
  (let ((result (opencode-session--tool-extract-todos
                 nil
                 '((todos . [((status . "completed") (content . "done"))])))))
    (should (= (length result) 1))))

;;; collapse-symbol

(ert-deftest test-opencode-render/collapse-symbol ()
  "Generate a unique collapse symbol."
  (should (eq (opencode-session--collapse-symbol "abc123")
              'opencode-collapse-abc123)))

;;; task-current-tool

(ert-deftest test-opencode-render/task-current-tool ()
  "Find the latest non-pending tool."
  (let ((tools '(((tool . "read") (state . ((status . "completed"))))
                 ((tool . "edit") (state . ((status . "running"))))
                 ((tool . "bash") (state . ((status . "pending")))))))
    (let ((result (opencode-session--task-current-tool tools)))
      (should (equal (alist-get 'tool result) "edit")))))

(ert-deftest test-opencode-render/task-current-tool-all-pending ()
  "Return nil when all tools are pending."
  (let ((tools '(((tool . "read") (state . ((status . "pending")))))))
    (should (null (opencode-session--task-current-tool tools)))))

;;; task-tool-line

(ert-deftest test-opencode-render/task-tool-line ()
  "Format a tool line."
  (let ((item '((tool . "read") (state . ((status . "completed") (title . "foo.el"))))))
    (should (equal (opencode-session--task-tool-line item) "Read foo.el"))))

(ert-deftest test-opencode-render/task-tool-line-running ()
  "Format a running tool line without title."
  (let ((item '((tool . "bash") (state . ((status . "running"))))))
    (should (equal (opencode-session--task-tool-line item) "Bash"))))

;;; role-face

(ert-deftest test-opencode-render/role-face-user ()
  "User messages get the user face."
  (let ((msg (opencode-message-create :role "user")))
    (should (eq (opencode-session--role-face msg) 'opencode-session-user-face))))

(ert-deftest test-opencode-render/role-face-assistant ()
  "Assistant messages get the assistant face."
  (let ((msg (opencode-message-create :role "assistant")))
    (should (eq (opencode-session--role-face msg) 'opencode-session-assistant-face))))

;;; message-error-text

(ert-deftest test-opencode-render/message-error-text-with-detail ()
  "Extract error detail message."
  (let ((msg (opencode-message-create
              :role "assistant"
              :error '((name . "SomeError")
                       (data . ((message . "something failed")))))))
    (let ((result (opencode-session--message-error-text msg)))
      (should (stringp result))
      (should (string-match-p "something failed" result)))))

(ert-deftest test-opencode-render/message-error-text-aborted ()
  "MessageAbortedError returns nil."
  (let ((msg (opencode-message-create
              :role "assistant"
              :error '((name . "MessageAbortedError")
                       (data . ((message . "aborted")))))))
    (should (null (opencode-session--message-error-text msg)))))

(ert-deftest test-opencode-render/message-error-text-no-error ()
  "Non-assistant or no-error messages return nil."
  (let ((msg (opencode-message-create :role "user")))
    (should (null (opencode-session--message-error-text msg)))))

;;; table alignment

(ert-deftest test-opencode-render/table-line-p ()
  "Detect markdown table lines."
  (should (opencode-session--table-line-p "| a | b |"))
  (should (opencode-session--table-line-p "|a|b|"))
  (should (opencode-session--table-line-p "  | a | b |  "))
  (should-not (opencode-session--table-line-p "no pipes here"))
  (should-not (opencode-session--table-line-p "| only opening"))
  (should-not (opencode-session--table-line-p "only closing |")))

(ert-deftest test-opencode-render/table-separator-p ()
  "Detect markdown table separator rows."
  (should (opencode-session--table-separator-p "|---|---|"))
  (should (opencode-session--table-separator-p "| --- | --- |"))
  (should (opencode-session--table-separator-p "|:---|---:|"))
  (should (opencode-session--table-separator-p "| :---: | --- |"))
  (should-not (opencode-session--table-separator-p "| a | b |"))
  (should-not (opencode-session--table-separator-p "| --- | text |")))

(ert-deftest test-opencode-render/split-table-cells ()
  "Split a table line into trimmed cells."
  (should (equal (opencode-session--split-table-cells "| a | b | c |")
                 '("a" "b" "c")))
  (should (equal (opencode-session--split-table-cells "|  foo  |bar|")
                 '("foo" "bar")))
  (should (equal (opencode-session--split-table-cells "| a |  |")
                 '("a" ""))))

(ert-deftest test-opencode-render/build-table-separator ()
  "Build a separator row from column widths."
  (should (equal (opencode-session--build-table-separator '(5 3 4))
                 "| ----- | --- | ---- |")))

(ert-deftest test-opencode-render/build-table-row ()
  "Build a padded table row."
  (should (equal (opencode-session--build-table-row '("a" "bb") '(5 5))
                 "| a     | bb    |")))

(ert-deftest test-opencode-render/align-simple-table ()
  "Align a simple two-column table."
  (let ((input "| Name | Age |\n|---|---|\n| Alice | 30 |\n| Bob | 25 |")
        (expected "| Name  | Age |\n| ----- | --- |\n| Alice | 30  |\n| Bob   | 25  |"))
    (should (equal (opencode-session--align-markdown-tables input)
                   expected))))

(ert-deftest test-opencode-render/align-uneven-columns ()
  "Align a table with varying cell widths."
  (let ((input "|x|long column value|\n|---|---|\n|ab|c|"))
    (let ((result (opencode-session--align-markdown-tables input)))
      ;; All rows should have aligned pipes
      (let ((lines (split-string result "\n")))
        ;; Each line should start and end with |
        (dolist (line lines)
          (should (string-prefix-p "|" (string-trim line)))
          (should (string-suffix-p "|" (string-trim line))))))))

(ert-deftest test-opencode-render/align-no-table ()
  "Non-table text passes through unchanged."
  (let ((input "Just some text\nwith multiple lines\nno tables here"))
    (should (equal (opencode-session--align-markdown-tables input) input))))

(ert-deftest test-opencode-render/align-table-between-text ()
  "Table embedded in surrounding text is aligned; other text is preserved."
  (let ((input "Before\n\n| a | bb |\n|---|---|\n| ccc | d |\n\nAfter"))
    (let ((result (opencode-session--align-markdown-tables input)))
      (should (string-prefix-p "Before\n\n" result))
      (should (string-suffix-p "\n\nAfter" result))
      ;; The table portion should be aligned
      (let ((lines (split-string result "\n")))
        (should (equal (nth 0 lines) "Before"))
        (should (equal (nth 1 lines) ""))
        ;; Table lines (indices 2-4) should be properly formatted
        (should (string-prefix-p "| " (nth 2 lines)))
        (should (equal (nth 5 lines) ""))
        (should (equal (nth 6 lines) "After"))))))

(ert-deftest test-opencode-render/align-empty-cells ()
  "Table with empty cells is handled."
  (let ((input "| a | | c |\n|---|---|---|\n| | b | |"))
    (let ((result (opencode-session--align-markdown-tables input)))
      ;; Should not error
      (should (stringp result))
      ;; Should have aligned columns
      (let ((lines (split-string result "\n")))
        (should (= (length lines) 3))))))

(ert-deftest test-opencode-render/align-preserves-empty-text ()
  "Empty string passes through."
  (should (equal (opencode-session--align-markdown-tables "") "")))

;;; retry-banner-text

(ert-deftest test-opencode-render/retry-banner-text-non-retry ()
  "Non-retry status returns nil."
  (should (null (opencode-session--retry-banner-text
                 (opencode-status-create :type "idle"))))
  (should (null (opencode-session--retry-banner-text
                 (opencode-status-create :type "busy")))))

(ert-deftest test-opencode-render/retry-banner-text-message-and-attempt ()
  "Retry banner includes the message and attempt without an OpenCode prefix."
  (let* ((status (opencode-status-create
                  :type "retry"
                  :attempt 2
                  :message "Free usage exceeded, subscribe to Go"))
         (text (opencode-session--retry-banner-text status)))
    (should (stringp text))
    (should-not (string-prefix-p "OpenCode: " text))
    (should (string-prefix-p "Free usage exceeded" text))
    (should (string-match-p "attempt #2" text))))

(ert-deftest test-opencode-render/retry-banner-text-includes-countdown ()
  "Retry banner includes a countdown when next is supplied."
  (let* ((next (+ (* 1000.0 (float-time)) 5000))
         (status (opencode-status-create
                  :type "retry"
                  :attempt 1
                  :message "Rate Limited"
                  :next next))
         (text (opencode-session--retry-banner-text status)))
    (should (string-match-p "retrying in [0-9]+s" text))))

(ert-deftest test-opencode-render/retry-banner-text-no-message ()
  "Retry banner with no message falls back to a placeholder."
  (let* ((status (opencode-status-create :type "retry"))
         (text (opencode-session--retry-banner-text status)))
    (should (stringp text))
    (should (string-match-p "retrying" text))))

;;; reasoning visibility

(ert-deftest test-opencode-render/reasoning-hidden-when-off ()
  "Reasoning parts render to nil when reasoning display is off."
  (let ((opencode-session-show-reasoning nil)
        (opencode-session--show-reasoning nil)
        (msg (opencode-message-create :role "assistant"))
        (part (opencode-message-part-create
               :type "reasoning"
               :text "let me think")))
    (should (null (opencode-session--render-message-part msg part)))))

(ert-deftest test-opencode-render/reasoning-shown-when-on ()
  "Reasoning parts render text when reasoning display is on."
  (let ((opencode-session-show-reasoning nil)
        (opencode-session--show-reasoning t)
        (msg (opencode-message-create :role "assistant"))
        (part (opencode-message-part-create
               :type "reasoning"
               :text "let me think")))
    (let ((result (opencode-session--render-message-part msg part)))
      (should (stringp result))
      (should (string-match-p "Thinking:" result))
      (should (string-match-p "let me think" result)))))

(ert-deftest test-opencode-render/reasoning-respects-buffer-local-override ()
  "Buffer-local override wins over the global default."
  (let ((opencode-session-show-reasoning t)
        (opencode-session--show-reasoning nil)
        (msg (opencode-message-create :role "assistant"))
        (part (opencode-message-part-create
               :type "reasoning"
               :text "let me think")))
    (should (null (opencode-session--render-message-part msg part)))))

(provide 'emacs-opencode-session-render-test)

;;; emacs-opencode-session-render-test.el ends here
