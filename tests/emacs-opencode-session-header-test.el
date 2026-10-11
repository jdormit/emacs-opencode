;;; emacs-opencode-session-header-test.el --- Tests for session header  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-opencode-session-header)

;;; format-number

(ert-deftest test-opencode-header/format-number-small ()
  "Small numbers have no separator."
  (should (equal (opencode-session--format-number 42) "42")))

(ert-deftest test-opencode-header/format-number-zero ()
  "Zero formats correctly."
  (should (equal (opencode-session--format-number 0) "0")))

(ert-deftest test-opencode-header/format-number-thousands ()
  "Thousands get a comma."
  (should (equal (opencode-session--format-number 1234) "1,234")))

(ert-deftest test-opencode-header/format-number-millions ()
  "Millions get two commas."
  (should (equal (opencode-session--format-number 1234567) "1,234,567")))

(ert-deftest test-opencode-header/format-number-exact-thousand ()
  "Exact thousand."
  (should (equal (opencode-session--format-number 1000) "1,000")))

(ert-deftest test-opencode-header/format-number-negative ()
  "Negative numbers are clamped to zero."
  (should (equal (opencode-session--format-number -5) "0")))

(ert-deftest test-opencode-header/format-number-float ()
  "Floats are truncated."
  (should (equal (opencode-session--format-number 1234.56) "1,234")))

;;; safe-number

(ert-deftest test-opencode-header/safe-number-value ()
  "Return the number when it is a number."
  (should (= (opencode-session--safe-number 42) 42)))

(ert-deftest test-opencode-header/safe-number-nil ()
  "Return 0 for nil."
  (should (= (opencode-session--safe-number nil) 0)))

(ert-deftest test-opencode-header/safe-number-string ()
  "Return 0 for non-number."
  (should (= (opencode-session--safe-number "hello") 0)))

;;; tokens-total

(ert-deftest test-opencode-header/tokens-total ()
  "Sum all token counts."
  (let ((tokens '((input . 100) (output . 200) (reasoning . 50)
                  (cache . ((read . 30) (write . 20))))))
    (should (= (opencode-session--tokens-total tokens) 400))))

(ert-deftest test-opencode-header/tokens-total-partial ()
  "Handle missing token fields."
  (let ((tokens '((input . 100) (output . 200))))
    (should (= (opencode-session--tokens-total tokens) 300))))

(ert-deftest test-opencode-header/tokens-total-nil ()
  "Return 0 for nil tokens (nil is a list in Elisp)."
  (should (= (opencode-session--tokens-total nil) 0)))

;;; assistant-message helpers

(ert-deftest test-opencode-header/has-assistant-messages-v2 ()
  "A V2 assistant message (role derived from `type') is detected."
  (let ((opencode-session--messages
         (list (opencode-message-create :role "assistant"))))
    (should (opencode-session--has-assistant-messages-p))))

(ert-deftest test-opencode-header/has-assistant-messages-none ()
  "Only user messages means no assistant messages."
  (let ((opencode-session--messages
         (list (opencode-message-create :role "user"))))
    (should-not (opencode-session--has-assistant-messages-p))))

(ert-deftest test-opencode-header/assistant-cost-total ()
  "Sum the cost carried on assistant message info."
  (let* ((assistant (opencode-message-create :role "assistant"))
         (opencode-session--messages
          (list (progn (setf (opencode-message-info assistant)
                             '((cost . 0.25)))
                       assistant)
                (opencode-message-create :role "user"))))
    (should (= (opencode-session--assistant-cost-total) 0.25))))

(ert-deftest test-opencode-header/last-assistant-info ()
  "Return the most recent assistant info carrying output tokens."
  (let* ((first (opencode-message-create :role "assistant"))
         (last (opencode-message-create :role "assistant"))
         (opencode-session--messages (list first last)))
    (setf (opencode-message-info first) '((tokens . ((output . 5)))))
    (setf (opencode-message-info last) '((tokens . ((output . 42)))))
    (should (equal (alist-get 'output
                              (alist-get 'tokens
                                         (opencode-session--last-assistant-info)))
                   42))))

(ert-deftest test-opencode-header/last-assistant-info-skips-user ()
  "User messages never provide assistant token info."
  (let* ((user (opencode-message-create :role "user"))
         (opencode-session--messages (list user)))
    (setf (opencode-message-info user) '((tokens . ((output . 7)))))
    (should-not (opencode-session--last-assistant-info))))

;;; status-busy-p

(ert-deftest test-opencode-header/status-busy-p-idle ()
  "Idle is not busy."
  (should (null (opencode-session--status-busy-p "idle"))))

(ert-deftest test-opencode-header/status-busy-p-running ()
  "Running is busy."
  (should (opencode-session--status-busy-p "running")))

(ert-deftest test-opencode-header/status-busy-p-waiting ()
  "Waiting is busy."
  (should (opencode-session--status-busy-p "waiting")))

(ert-deftest test-opencode-header/status-busy-p-struct-retry ()
  "Retry struct is busy."
  (should (opencode-session--status-busy-p
           (opencode-status-create :type "retry"))))

(ert-deftest test-opencode-header/status-busy-p-struct-idle ()
  "Idle struct is not busy."
  (should (null (opencode-session--status-busy-p
                 (opencode-status-create :type "idle")))))

(ert-deftest test-opencode-header/status-busy-p-nil ()
  "Nil status is not busy."
  (should (null (opencode-session--status-busy-p nil))))

;;; truncate-retry-message

(ert-deftest test-opencode-header/truncate-short-message ()
  "Short messages pass through untouched."
  (let ((opencode-session-header-retry-message-max 60))
    (should (equal (opencode-session--truncate-retry-message "boom")
                   "boom"))))

(ert-deftest test-opencode-header/truncate-long-message ()
  "Long messages are clipped with an ellipsis."
  (let* ((opencode-session-header-retry-message-max 10)
         (input (make-string 30 ?x))
         (result (opencode-session--truncate-retry-message input)))
    (should (equal (length result) 10))
    (should (string-suffix-p "..." result))))

(ert-deftest test-opencode-header/truncate-nil ()
  "Nil messages stay nil."
  (should (null (opencode-session--truncate-retry-message nil))))

(ert-deftest test-opencode-header/truncate-empty ()
  "Empty strings stay nil."
  (should (null (opencode-session--truncate-retry-message ""))))

;;; retry-countdown-suffix

(ert-deftest test-opencode-header/countdown-attempt-only ()
  "Attempt without next renders as #N."
  (should (equal (opencode-session--retry-countdown-suffix 3 nil) "[#3]")))

(ert-deftest test-opencode-header/countdown-attempt-and-next ()
  "Attempt + future next renders countdown seconds."
  (let* ((next (+ (* 1000.0 (float-time)) 5000))
         (suffix (opencode-session--retry-countdown-suffix 1 next)))
    (should (string-match-p "\\`\\[#1, in [0-9]+s\\]\\'" suffix))))

(ert-deftest test-opencode-header/countdown-past-next ()
  "Past next clamps to 0s."
  (let* ((next (- (* 1000.0 (float-time)) 5000))
         (suffix (opencode-session--retry-countdown-suffix 2 next)))
    (should (string-match-p "in 0s" suffix))))

(ert-deftest test-opencode-header/countdown-empty ()
  "No attempt and no next yields nil."
  (should (null (opencode-session--retry-countdown-suffix nil nil))))

;;; header-retry-segment

(ert-deftest test-opencode-header/retry-segment-not-retry ()
  "Non-retry status produces no segment."
  (should (null (opencode-session--header-retry-segment
                 (opencode-status-create :type "idle"))))
  (should (null (opencode-session--header-retry-segment
                 (opencode-status-create :type "busy")))))

(ert-deftest test-opencode-header/retry-segment-retry ()
  "Retry status produces a fontified segment with the message."
  (let* ((status (opencode-status-create
                  :type "retry"
                  :attempt 1
                  :message "Rate Limited"))
         (segment (opencode-session--header-retry-segment status)))
    (should (stringp segment))
    (should (string-match-p "Rate Limited" segment))
    (should (eq (get-text-property 0 'face segment)
                'opencode-session-error-face))))

(ert-deftest test-opencode-header/retry-segment-no-message ()
  "Retry status with no message and no countdown returns nil."
  (let ((status (opencode-status-create :type "retry")))
    (should (null (opencode-session--header-retry-segment status)))))

;;; model-context-limit

(ert-deftest test-opencode-header/model-context-limit ()
  "Read the context limit from the flat V2 model list."
  (with-temp-buffer
    (setq-local opencode-session--connection
                (opencode-connection-create
                 :models '(((id . "gpt-6") (modelID . "gpt-6")
                            (providerID . "openai")
                            (limit . ((context . 200000)
                                      (input . 100000)
                                      (output . 100000)))))))
    (should (equal (opencode-session--model-context-limit "openai" "gpt-6")
                   200000))))

(ert-deftest test-opencode-header/model-context-limit-by-model-id ()
  "Match the model by modelID when the catalog id differs."
  (with-temp-buffer
    (setq-local opencode-session--connection
                (opencode-connection-create
                 :models '(((id . "gpt-6-fast") (modelID . "gpt-6")
                            (providerID . "openai")
                            (limit . ((context . 128000)))))))
    (should (equal (opencode-session--model-context-limit "openai" "gpt-6")
                   128000))))

(ert-deftest test-opencode-header/model-context-limit-missing ()
  "Return nil when the model is unknown."
  (with-temp-buffer
    (setq-local opencode-session--connection
                (opencode-connection-create :models nil))
    (should (null (opencode-session--model-context-limit "openai" "nope")))))

(provide 'emacs-opencode-session-header-test)

;;; emacs-opencode-session-header-test.el ends here
