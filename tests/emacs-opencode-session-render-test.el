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

(ert-deftest test-opencode-render/read-shows-loaded-instructions ()
  "Render instruction files loaded by a completed read tool."
  (let* ((opencode-session--connection
          (opencode-connection-create :directory "/project/"))
         (part (opencode-message-part-create
                :id "p1"
                :type "tool"
                :tool "read"
                :state '((status . "completed")
                         (input . ((filePath . "/project/src/main.el")))
                         (metadata . ((loaded . ["/project/AGENTS.md"
                                                "/project/src/CLAUDE.md"])))
                         (time . ((start . 1) (end . 2)))))))
    (should (equal (substring-no-properties
                    (opencode-session--render-message-part
                     (opencode-message-create :role "assistant") part))
                   "→ Read src/main.el ▸\n↳ Loaded AGENTS.md\n↳ Loaded src/CLAUDE.md"))))

(ert-deftest test-opencode-render/read-hides-loaded-instructions-when-compacted ()
  "Do not render loaded instruction files from compacted read parts."
  (let ((opencode-session--connection nil)
        (part (opencode-message-part-create
               :id "p1"
               :type "tool"
               :tool "read"
               :state '((status . "completed")
                        (input . ((filePath . "/project/src/main.el")))
                        (metadata . ((loaded . ["/project/AGENTS.md"])))
                        (time . ((compacted . 3)))))))
    (should-not (string-match-p
                 "Loaded"
                 (opencode-session--render-message-part
                  (opencode-message-create :role "assistant") part)))))

(ert-deftest test-opencode-render/read-hides-loaded-instructions-while-running ()
  "Do not render loaded instruction files before a read completes."
  (let ((opencode-session--connection nil)
        (part (opencode-message-part-create
               :id "p1"
               :type "tool"
               :tool "read"
               :state '((status . "running")
                        (input . ((filePath . "/project/src/main.el")))
                        (metadata . ((loaded . ["/project/AGENTS.md"])))))))
    (should-not (string-match-p
                 "Loaded"
                 (opencode-session--render-message-part
                  (opencode-message-create :role "assistant") part)))))

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

;;; tool output drawer

(defun opencode-render-test--tool-part (id tool status &rest state)
  "Build a tool part with ID, TOOL, STATUS, and extra STATE pairs."
  (opencode-message-part-create
   :id id
   :message-id "m1"
   :type "tool"
   :tool tool
   :state (append `((status . ,status)) state)))

(ert-deftest test-opencode-render/tool-drawer-closed-indicator ()
  "A completed tool renders a closed drawer indicator on its first line."
  (with-temp-buffer
    (let* ((part (opencode-render-test--tool-part
                  "p1" "grep" "completed"
                  '(input . ((pattern . "needle")))
                  '(output . "a.el:1:needle")))
           (text (opencode-session--tool-part-line part)))
      (should (string-match-p "Grep.*▸$" (car (split-string text "\n"))))
      (should-not (string-match-p "a\\.el:1:needle" text))
      (should (equal (get-text-property 0 'opencode-tool-part-id text) "p1"))
      (should (equal (get-text-property 0 'opencode-tool-message-id text) "m1"))
      (should (eq (get-text-property 0 'keymap text)
                  opencode-session--tool-drawer-keymap)))))

(ert-deftest test-opencode-render/tool-drawer-hidden-while-running ()
  "Tools that have not completed show no drawer indicator or keymap."
  (with-temp-buffer
    (let ((text (opencode-session--tool-part-line
                 (opencode-render-test--tool-part "p1" "grep" "running"))))
      (should-not (string-match-p "[▸▾]" text))
      (should-not (get-text-property 0 'keymap text)))))

(ert-deftest test-opencode-render/tool-drawer-open-shows-output ()
  "An open drawer renders the tool output tagged as tool-output."
  (with-temp-buffer
    (opencode-session--set-tool-drawer-open "p1" t)
    (let* ((part (opencode-render-test--tool-part
                  "p1" "grep" "completed"
                  '(output . "\e[31ma.el:1:needle\e[0m\n")))
           (text (opencode-session--tool-part-line part))
           (pos (string-match "a\\.el:1:needle" text)))
      (should (string-match-p "▾$" (car (split-string text "\n"))))
      (should pos)
      (should (equal (get-text-property pos 'opencode-part-type text)
                     "tool-output"))
      (should-not (string-match-p "\e\\[" text)))))

(ert-deftest test-opencode-render/tool-drawer-open-empty-output ()
  "An open drawer with empty output shows a placeholder."
  (with-temp-buffer
    (opencode-session--set-tool-drawer-open "p1" t)
    (should (string-match-p
             "(no output)"
             (opencode-session--tool-part-line
              (opencode-render-test--tool-part "p1" "write" "completed"
                                               '(output . "")))))))

(ert-deftest test-opencode-render/tool-drawer-open-missing-output-loading ()
  "An open drawer whose output has not been fetched shows a loading line."
  (with-temp-buffer
    (opencode-session--set-tool-drawer-open "p1" t)
    (should (string-match-p
             "Loading"
             (opencode-session--tool-part-line
              (opencode-render-test--tool-part "p1" "read" "completed"))))))

(ert-deftest test-opencode-render/tool-drawer-bash-output-from-metadata ()
  "Bash keeps its command visible and puts metadata output in the drawer."
  (with-temp-buffer
    (let* ((part (opencode-render-test--tool-part
                  "p1" "bash" "completed"
                  '(input . ((command . "ls") (description . "list files")))
                  '(metadata . ((output . "a\nb\nc")))))
           (closed (opencode-session--tool-part-line part)))
      (should (string-match-p "^\\$ ls$" closed))
      (should-not (string-match-p "^a\nb\nc" closed))
      (opencode-session--set-tool-drawer-open "p1" t)
      (should (string-match-p "^\\$ ls\na\nb\nc"
                              (opencode-session--tool-part-line part))))))

(ert-deftest test-opencode-render/tool-drawer-task-keymap ()
  "Task blocks open the subagent on RET and toggle the drawer on TAB."
  (should (eq (lookup-key opencode-session--task-keymap (kbd "RET"))
              #'opencode-session-open-subagent))
  (should (eq (lookup-key opencode-session--task-keymap (kbd "TAB"))
              #'opencode-session-toggle-tool-output))
  (should (eq (lookup-key opencode-session--tool-drawer-keymap (kbd "TAB"))
              #'opencode-session-toggle-tool-output))
  (should (eq (lookup-key opencode-session--tool-drawer-keymap (kbd "<tab>"))
              #'opencode-session-toggle-tool-output)))

(ert-deftest test-opencode-render/tool-drawer-task-keeps-subagent-props ()
  "A completed task with a subagent keeps its task keymap over the drawer's."
  (with-temp-buffer
    (let ((text (opencode-session--tool-part-line
                 (opencode-render-test--tool-part
                  "p1" "task" "completed"
                  '(input . ((description . "explore")))
                  '(metadata . ((sessionId . "ses_child")))
                  '(output . "result")))))
      (should (eq (get-text-property 0 'keymap text)
                  opencode-session--task-keymap))
      (should (eq (get-text-property (string-match "▸" text) 'keymap text)
                  opencode-session--task-keymap))
      (should (string-match-p "RET" (get-text-property 0 'help-echo text)))
      (should (equal (get-text-property 0 'opencode-tool-part-id text) "p1")))))

(ert-deftest test-opencode-render/tool-drawer-open-spacing ()
  "An open drawer keeps its line in place and adds a blank line after its body."
  (with-temp-buffer
    (opencode-session--set-tool-drawer-open "p2" t)
    (let* ((message (opencode-message-create :id "m1" :role "assistant"))
           (parts (list (cons "p1" (opencode-render-test--tool-part
                                    "p1" "glob" "completed"))
                        (cons "p2" (opencode-render-test--tool-part
                                    "p2" "grep" "completed"
                                    '(output . "line1\nline2")))
                        (cons "p3" (opencode-render-test--tool-part
                                    "p3" "glob" "completed"))
                        (cons "p4" (opencode-message-part-create
                                    :id "p4" :type "text" :text "done"))))
           (text (substring-no-properties
                  (opencode-session--render-message-parts message parts))))
      (should (equal text
                     "✱ Glob ▸\n✱ Grep ▾\nline1\nline2\n\n✱ Glob ▸\ndone\n")))))

(ert-deftest test-opencode-render/tool-drawer-open-last-part-no-trailing-blank ()
  "An open drawer at the end of a message adds no trailing blank line."
  (with-temp-buffer
    (opencode-session--set-tool-drawer-open "p1" t)
    (let* ((message (opencode-message-create :id "m1" :role "assistant"))
           (parts (list (cons "p1" (opencode-render-test--tool-part
                                    "p1" "grep" "completed"
                                    '(output . "line1")))))
           (text (substring-no-properties
                  (opencode-session--render-message-parts message parts))))
      (should (equal text "✱ Grep ▾\nline1")))))

(ert-deftest test-opencode-render/inline-tools-ending-in-n-stay-on-own-lines ()
  "A tool line ending in the letter n is still followed by a newline."
  (with-temp-buffer
    (let* ((message (opencode-message-create :id "m1" :role "assistant"))
           (parts (list (cons "p1" (opencode-render-test--tool-part
                                    "p1" "read" "running"
                                    '(input . ((filePath . "foo.json")))))
                        (cons "p2" (opencode-render-test--tool-part
                                    "p2" "read" "running"
                                    '(input . ((filePath . "bar.el")))))))
           (text (substring-no-properties
                  (opencode-session--render-message-parts message parts))))
      (should (equal text "→ Read foo.json [running]\n→ Read bar.el [running]")))))

(ert-deftest test-opencode-render/inline-tool-then-block-tool-spacing ()
  "Closed inline tool lines keep their single-newline spacing before blocks."
  (with-temp-buffer
    (let* ((message (opencode-message-create :id "m1" :role "assistant"))
           (parts (list (cons "p1" (opencode-render-test--tool-part
                                    "p1" "glob" "completed"))
                        (cons "p2" (opencode-render-test--tool-part
                                    "p2" "bash" "completed"
                                    '(input . ((command . "ls")))))))
           (text (substring-no-properties
                  (opencode-session--render-message-parts message parts))))
      (should (equal text "✱ Glob ▸\n✱ Shell ls ▸\n$ ls\n")))))

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

;;; form blocks

(defun opencode-render-test--form-state (&optional status answers)
  "Return a form-state fixture with STATUS and ANSWERS."
  (opencode-session--form-state-create
   :form-id "frm_1"
   :session-id "ses_1"
   :form `((title . "Deploy")
           (fields .
            [((key . "env") (type . "string") (title . "Environment"))
             ((key . "token") (type . "external") (title . "Token")
              (url . "https://example.com/oauth"))]))
   :status status
   :answers answers))

(ert-deftest test-opencode-render/form-block-text-pending ()
  "Pending forms show the title, fields and an awaiting line."
  (let ((text (opencode-session--form-block-text
               (opencode-render-test--form-state 'pending))))
    (should (string-match-p "Deploy" text))
    (should (string-match-p "\[awaiting\]" text))
    (should (string-match-p "Environment" text))
    (should (string-match-p "Awaiting your answer" text))
    (should (string-match-p "https://example.com/oauth" text))))

(ert-deftest test-opencode-render/form-block-text-answered ()
  "Answered forms show the submitted values."
  (let ((text (opencode-session--form-block-text
               (opencode-render-test--form-state
                'answered '((env . "prod")
                            (count . 3)
                            (dry-run . :json-false)
                            (tags . ["a" "b"]))))))
    (should (string-match-p "\[answered\]" text))
    (should (string-match-p "Environment: prod" text))
    (should (string-match-p "count: 3" text))
    (should (string-match-p "dry-run: false" text))
    (should (string-match-p "tags: a, b" text))
    (should-not (string-match-p "Awaiting" text))))

(ert-deftest test-opencode-render/form-block-text-cancelled ()
  "Cancelled forms show a cancelled header only."
  (let ((text (opencode-session--form-block-text
               (opencode-render-test--form-state 'cancelled))))
    (should (string-match-p "\[cancelled\]" text))
    (should (string-match-p "Deploy" text))
    (should-not (string-match-p "Awaiting" text))))

(ert-deftest test-opencode-render/form-value-text ()
  "Answer values render as readable strings."
  (should (equal (opencode-session--form-value-text t) "true"))
  (should (equal (opencode-session--form-value-text :json-false) "false"))
  (should (equal (opencode-session--form-value-text '("a" "b")) "a, b"))
  (should (equal (opencode-session--form-value-text ["a" "b"]) "a, b"))
  (should (equal (opencode-session--form-value-text "x") "x"))
  (should (equal (opencode-session--form-value-text 3) "3")))

(ert-deftest test-opencode-render/form-render-and-clear-region ()
  "render-forms inserts blocks above the input and clear-forms removes them."
  (let ((buffer (generate-new-buffer " *oc-form-render*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local opencode-session--forms nil)
          (setq-local opencode-session--input-start-marker
                      (copy-marker (point-min)))
          (setq-local opencode-session--input-marker
                      (copy-marker (point-max)))
          (push (opencode-render-test--form-state 'pending)
                opencode-session--forms)
          (opencode-session--render-forms)
          (should (string-match-p "Awaiting your answer" (buffer-string)))
          (should (string-match-p "Deploy" (buffer-string)))
          (let ((form-state (car opencode-session--forms)))
            (should (markerp (opencode-session--form-state-start-marker
                              form-state)))
            (should (markerp (opencode-session--form-state-end-marker
                              form-state))))
          (opencode-session--clear-forms)
          (should-not (string-match-p "Deploy" (buffer-string)))
          (should-not (opencode-session--form-state-start-marker
                       (car opencode-session--forms)))
          (should-not (opencode-session--form-state-end-marker
                       (car opencode-session--forms))))
      (kill-buffer buffer))))

(provide 'emacs-opencode-session-render-test)

;;; emacs-opencode-session-render-test.el ends here
