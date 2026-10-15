;;; emacs-opencode-run-test.el --- Tests for opencode run wrapper  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-opencode-run)

;;; build-args

(ert-deftest test-opencode-run/build-args-bare ()
  "A bare prompt runs with a private server."
  (should (equal (opencode-run--build-args "the prompt")
                 '("run" "the prompt"))))

(ert-deftest test-opencode-run/build-args-model ()
  "The :model keyword adds a --model flag."
  (should (equal (opencode-run--build-args "the prompt" :model "anthropic/foo")
                 '("run" "--model" "anthropic/foo" "the prompt"))))

(ert-deftest test-opencode-run/build-args-agent ()
  "The :agent keyword adds an --agent flag."
  (should (equal (opencode-run--build-args "the prompt" :agent "build")
                 '("run" "--agent" "build" "the prompt"))))

(ert-deftest test-opencode-run/build-args-skip-permissions ()
  "The :dangerously-skip-permissions keyword auto-approves permissions."
  (should (equal (opencode-run--build-args
                  "the prompt" :dangerously-skip-permissions t)
                 '("run" "--auto" "the prompt"))))

(ert-deftest test-opencode-run/build-args-skip-permissions-nil ()
  "A nil :dangerously-skip-permissions omits the flag."
  (should (equal (opencode-run--build-args
                  "the prompt" :dangerously-skip-permissions nil)
                 '("run" "the prompt"))))

(ert-deftest test-opencode-run/build-args-nil-flags-omitted ()
  "Keyword flags with nil values are omitted."
  (should (equal (opencode-run--build-args "the prompt" :model nil :agent nil)
                 '("run" "the prompt"))))

(ert-deftest test-opencode-run/build-args-all-flags ()
  "All flags combine in a stable order before the prompt."
  (should (equal (opencode-run--build-args
                  "the prompt"
                  :model "anthropic/foo"
                  :agent "build"
                  :dangerously-skip-permissions t)
                 '("run"
                   "--model" "anthropic/foo"
                   "--agent" "build"
                   "--auto"
                   "the prompt"))))

(ert-deftest test-opencode-run/process-environment-sets-pwd ()
  "The CLI resolves its project from PWD, so it must match the directory."
  (let ((process-environment '("PWD=/elsewhere" "HOME=/home/me")))
    (let ((env (opencode-run--process-environment "/tmp/project/")))
      (should (member "PWD=/tmp/project" env))
      (should-not (member "PWD=/elsewhere" env)))))

;;; model references

(ert-deftest test-opencode-run/model-ref ()
  "Model strings parse into server model references."
  (should (equal (opencode-run--model-ref "openai/gpt-5")
                 '((providerID . "openai") (id . "gpt-5"))))
  (should (equal (opencode-run--model-ref "openrouter/a/b#high")
                 '((providerID . "openrouter") (id . "a/b") (variant . "high"))))
  (should-not (opencode-run--model-ref nil)))

;;; command output

(ert-deftest test-opencode-run/command-output-collects-final-turn ()
  "Command output is the assistant text after the last user prompt."
  (should (equal (opencode-run--command-output
                  '(((type . "user") (text . "old"))
                    ((type . "assistant") (content . (((type . "text") (text . "stale")))))
                    ((type . "user") (text . "/review"))
                    ((type . "assistant")
                     (content . (((type . "reasoning") (text . "hmm"))
                                 ((type . "text") (text . "first")))))
                    ((type . "assistant") (content . (((type . "text") (text . "second")))))
                    ((type . "idle"))))
                 "first\n\nsecond")))

(ert-deftest test-opencode-run/command-runs-through-session-api ()
  "The :command keyword runs the command in a new session and reports its text."
  (let ((default-directory "/tmp/project/")
        calls
        result)
    (cl-letf (((symbol-function 'opencode-run-server)
               (lambda (_directory on-ready) (funcall on-ready 'conn)))
              ((symbol-function 'opencode-client-session-create)
               (lambda (_conn &rest args)
                 (push (list 'create (plist-get args :agent) (plist-get args :permissions)) calls)
                 (funcall (plist-get args :success) :data '((id . "ses_1")))))
              ((symbol-function 'opencode-client-session-command)
               (lambda (_conn session-id command arguments &rest args)
                 (push (list 'command session-id command arguments) calls)
                 (funcall (plist-get args :success))))
              ((symbol-function 'opencode-client-session-wait)
               (lambda (_conn _session-id &rest args)
                 (funcall (plist-get args :success))))
              ((symbol-function 'opencode-client-session-messages)
               (lambda (_conn _session-id &rest args)
                 (funcall (plist-get args :success)
                          :data '(((type . "user") (text . "x"))
                                  ((type . "assistant")
                                   (content . (((type . "text") (text . "done\n")))))))))
              ((symbol-function 'opencode-client-session-get)
               (lambda (_conn _session-id &rest args)
                 (funcall (plist-get args :success) :data '((id . "ses_1"))))))
      (opencode-run "HEAD~1" (lambda (output success-p) (setq result (list output success-p)))
                    :command "review" :agent "build" :dangerously-skip-permissions t))
    (should (equal result '("done" t)))
    (should (equal (reverse calls)
                   `((create "build" [((action . "*") (resource . "*") (effect . "allow"))])
                     (command "ses_1" "review" "HEAD~1"))))))

(ert-deftest test-opencode-run/command-failure-reports-error ()
  "A failed command reports failure to the callback."
  (let ((default-directory "/tmp/project/")
        result)
    (cl-letf (((symbol-function 'opencode-run-server)
               (lambda (_directory on-ready) (funcall on-ready 'conn)))
              ((symbol-function 'opencode-client-session-create)
               (lambda (_conn &rest args)
                 (funcall (plist-get args :success) :data '((id . "ses_1")))))
              ((symbol-function 'opencode-client-session-command)
               (lambda (_conn _session-id _command _arguments &rest args)
                 (funcall (plist-get args :error) :error-thrown '(error . "boom")))))
      (opencode-run "x" (lambda (output success-p) (setq result (list output success-p)))
                    :command "missing"))
    (should (equal (cadr result) nil))
    (should (string-match-p "missing" (car result)))))

;;; invoke-callback

(ert-deftest test-opencode-run/invoke-callback-one-arg ()
  "A single-argument callback receives only the output."
  (let ((received :none))
    (opencode-run--invoke-callback
     (lambda (output) (setq received output))
     "the output" t)
    (should (equal received "the output"))))

(ert-deftest test-opencode-run/invoke-callback-two-args ()
  "A two-argument callback receives output and success-p."
  (let ((received :none))
    (opencode-run--invoke-callback
     (lambda (output success-p) (setq received (list output success-p)))
     "the output" t)
    (should (equal received '("the output" t)))))

(ert-deftest test-opencode-run/invoke-callback-rest-args ()
  "A &rest callback receives output and success-p."
  (let ((received :none))
    (opencode-run--invoke-callback
     (lambda (&rest args) (setq received args))
     "the output" nil)
    (should (equal received '("the output" nil)))))

(ert-deftest test-opencode-run/invoke-callback-optional-second ()
  "A callback with an optional second arg receives both values."
  (let ((received :none))
    (opencode-run--invoke-callback
     (lambda (output &optional success-p) (setq received (list output success-p)))
     "the output" t)
    (should (equal received '("the output" t)))))

(ert-deftest test-opencode-run/invoke-callback-nil ()
  "A nil callback is a no-op."
  (should-not (opencode-run--invoke-callback nil "the output" t)))

(ert-deftest test-opencode-run/invoke-callback-trims-trailing ()
  "Trailing whitespace and newlines are stripped from the output."
  (let ((received :none))
    (opencode-run--invoke-callback
     (lambda (output) (setq received output))
     "the output\n\n  " t)
    (should (equal received "the output"))))

(ert-deftest test-opencode-run/invoke-callback-trims-nil-output ()
  "A nil output is passed through without error."
  (let ((received :none))
    (opencode-run--invoke-callback
     (lambda (output) (setq received output))
     nil t)
    (should (null received))))

(provide 'emacs-opencode-run-test)

;;; emacs-opencode-run-test.el ends here
