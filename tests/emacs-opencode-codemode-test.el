;;; emacs-opencode-codemode-test.el --- Code Mode rendering tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-opencode)

(ert-deftest test-opencode-codemode/live-and-history ()
  "Nested calls update live and survive history projection with their output."
  (with-temp-buffer
    (let* ((message (opencode-message-create :id "m1" :session-id "s1" :role "assistant"))
           (base '((id . "call") (sessionID . "s1") (assistantMessageID . "m1")))
           (code '((code . "return await tools.tasks.list({filter: {status: 'pending'}})")))
           (calls '(((tool . "tasks.list") (status . "running")
                     (input . ((filter . ((status . "pending"))))))
                    ((tool . "tasks.list") (status . "completed"))
                    ((tool . "tasks.remove") (status . "error") (input . ((id . "missing")))))))
      (opencode-session--apply-tool-event message "session.tool.input.started"
                                         (append base '((name . "execute"))))
      (opencode-session--apply-tool-event message "session.tool.called"
                                         (append base `((input . ,code))))
      (opencode-session--apply-tool-event message "session.tool.progress"
                                         (append base `((metadata . ((toolCalls . ,calls))))))
      (let* ((part (cdar (opencode-message-parts message)))
             (live (substring-no-properties (opencode-session--tool-part-line part))))
        (should (string-match-p "tasks.list \\[running\\]" live))
        (should (string-match-p (regexp-quote "{\"filter\":{\"status\":\"pending\"}}") live))
        (should (string-match-p "tasks.list \\[completed\\]" live))
        (should (string-match-p "tasks.remove \\[error\\]" live))
        (setf (alist-get 'status (car calls)) "completed")
        (opencode-session--apply-tool-event
         message "session.tool.success"
         (append base `((metadata . ((toolCalls . ,calls)))
                        (content . (((type . "text") (text . "result: 2 tasks")))))))
        (let* ((history (opencode-session--message-from-info
                         `((id . "m1") (type . "assistant")
                           (content . (((type . "tool") (id . "call") (name . "execute")
                                        (state . ((status . "completed") (input . ,code)
                                                  (metadata . ((toolCalls . ,(vconcat calls))))
                                                  (content . (((type . "text") (text . "result: 2 tasks"))))))))))
                         "s1"))
               (reloaded (cdar (opencode-message-parts history))))
          (opencode-session--set-tool-drawer-open "call" t)
          (should (equal (substring-no-properties (opencode-session--tool-part-line part))
                         (substring-no-properties (opencode-session--tool-part-line reloaded))))
          (should (string-match-p "result: 2 tasks" (opencode-session--tool-part-line reloaded)))
          (should-not (string-match-p "tasks.list \\[running\\]"
                                      (opencode-session--tool-part-line reloaded))))))))

(ert-deftest test-opencode-codemode/script-error-keeps-result-drawer ()
  "An execute script error is visible even when its tool execution completed."
  (with-temp-buffer
    (let* ((part (opencode-message-part-create
                  :id "call" :tool "execute"
                  :state '((status . "completed") (metadata . ((error . t)))
                           (output . "Unknown tool: tasks.missing"))))
           (text (opencode-session--tool-part-line part)))
      (should (string-match-p "\\[error\\]" text))
      (should (opencode-session--tool-drawer-available-p part))
      (opencode-session--set-tool-drawer-open "call" t)
      (should (string-match-p "Unknown tool: tasks.missing"
                              (opencode-session--tool-part-line part))))))

(ert-deftest test-opencode-codemode/json-arguments-survive-both-parsers ()
  "Tool arguments keep false, null, arrays, and objects distinct."
  (let* ((json "{\"type\":\"session.tool.progress\",\"data\":{\"metadata\":{\"toolCalls\":[{\"tool\":\"tasks.list\",\"status\":\"running\",\"input\":{\"enabled\":false,\"empty\":null,\"filters\":[],\"options\":{},\"nested\":[{}]}},{\"tool\":\"tasks.list\",\"status\":\"completed\",\"input\":{}}]}}}")
         (http (with-temp-buffer (insert json) (goto-char (point-min)) (opencode--json-read)))
         (bridge (opencode-sse--bridge-parse-line json)))
    (dolist (event (list http bridge))
      (let ((text (opencode-session--execute-calls (alist-get 'metadata (alist-get 'data event)))))
        (should (string-match-p (regexp-quote "\"enabled\":false") text))
        (should (string-match-p (regexp-quote "\"empty\":null") text))
        (should (string-match-p (regexp-quote "\"filters\":[]") text))
        (should (string-match-p (regexp-quote "\"options\":{}") text))
        (should (string-match-p (regexp-quote "\"nested\":[{}]") text))
        (should (string-match-p (regexp-quote "tasks.list [completed] {}") text))))))

(provide 'emacs-opencode-codemode-test)
;;; emacs-opencode-codemode-test.el ends here
