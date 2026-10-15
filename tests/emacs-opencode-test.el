;;; emacs-opencode-test.el --- Tests for entry point  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-opencode)

;;; normalize-directory

(ert-deftest test-opencode/normalize-directory-trailing-slash ()
  "Ensure trailing slash is added."
  (let ((result (opencode--normalize-directory "/tmp/foo")))
    (should (string-suffix-p "/" result))))

(ert-deftest test-opencode/normalize-directory-expands ()
  "Expand relative paths."
  (let ((default-directory "/home/user/"))
    (let ((result (opencode--normalize-directory "projects/foo")))
      (should (file-name-absolute-p result))
      (should (string-suffix-p "/" result)))))

(ert-deftest test-opencode/normalize-directory-idempotent ()
  "Normalizing an already-normalized path is idempotent."
  (let ((path "/tmp/foo/"))
    (should (equal (opencode--normalize-directory path)
                   (opencode--normalize-directory
                    (opencode--normalize-directory path))))))

;;; session-from-data

(ert-deftest test-opencode/session-from-data ()
  "Create a session struct from server session info."
  (let* ((data '((id . "s1")
                 (projectID . "p1")
                 (location . ((directory . "/tmp/p")))
                 (title . "Test Session")
                 (agent . "plan")
                 (time . ((created . 1) (updated . 2)))))
         (session (opencode--session-from-data data)))
    (should (opencode-session-p session))
    (should (equal (opencode-session-id session) "s1"))
    (should (equal (opencode-session-project-id session) "p1"))
    (should (equal (opencode-session-directory session) "/tmp/p"))
    (should (equal (opencode-session-title session) "Test Session"))
    (should (equal (opencode-session-time-created session) 1))
    (should (equal (opencode-session-time-updated session) 2))
    (should (equal (opencode-session-info session) data))))

;;; session-label

(ert-deftest test-opencode/session-label-title-only ()
  "Use title as label."
  (should (equal (opencode--session-label '((title . "Hello")))
                 "Hello")))

(ert-deftest test-opencode/session-label-untitled ()
  "Use fallback for missing title."
  (should (equal (opencode--session-label '((id . "s1")))
                 "Untitled session")))

(ert-deftest test-opencode/session-label-with-identifiers ()
  "Include the ID when requested."
  (should (equal (opencode--session-label '((title . "Hello") (id . "s1")) t)
                 "Hello [s1]")))

(ert-deftest test-opencode/session-label-identifiers-without-slug ()
  "Only include ID when slug is nil."
  (let ((result (opencode--session-label
                 '((title . "Hello") (id . "s1"))
                 t)))
    (should (string-match-p "Hello" result))
    (should (string-match-p "\\[s1\\]" result))
    (should-not (string-match-p "(nil)" result))))

;;; session-items

(ert-deftest test-opencode/session-items-vector ()
  "Convert vector to list."
  (should (equal (opencode--session-items [1 2 3]) '(1 2 3))))

(ert-deftest test-opencode/session-items-list ()
  "Pass list through."
  (should (equal (opencode--session-items '(1 2)) '(1 2))))

(ert-deftest test-opencode/session-items-nil ()
  "Return nil for nil."
  (should (null (opencode--session-items nil))))

(ert-deftest test-opencode/session-items-other ()
  "Return nil for non-collection input."
  (should (null (opencode--session-items "string"))))

;;; session-choices

(ert-deftest test-opencode/session-choices-unique ()
  "Build unique completion choices."
  (let* ((items '(((title . "A") (id . "1"))
                  ((title . "B") (id . "2"))))
         (choices (opencode--session-choices items)))
    (should (= (length choices) 2))
    (should (equal (caar choices) "A"))
    (should (equal (caadr choices) "B"))))

(ert-deftest test-opencode/session-choices-disambiguate ()
  "Disambiguate duplicate titles with identifiers."
  (let* ((items '(((title . "Same") (id . "1") (slug . "same-1"))
                  ((title . "Same") (id . "2") (slug . "same-2"))))
         (choices (opencode--session-choices items)))
    (should (= (length choices) 2))
    ;; Labels should include disambiguating info
    (should-not (equal (car (nth 0 choices))
                       (car (nth 1 choices))))))

(ert-deftest test-opencode/select-session-excludes-subagents ()
  "Exclude sessions with a parent from session completion."
  (let (candidates selected-session)
    (cl-letf (((symbol-function 'opencode-client-location)
               (lambda (_connection &rest args)
                 (funcall (plist-get args :success)
                          :data '((project . ((id . "prj")))))))
              ((symbol-function 'opencode-client-sessions)
               (lambda (_connection &rest args)
                 (should (plist-get args :roots))
                 (should (equal (plist-get args :project) "prj"))
                 (funcall (plist-get args :success)
                          :data '(((id . "parent") (title . "Parent"))
                                  ((id . "child") (title . "Child")
                                   (parentID . "parent"))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _args)
                 (setq candidates (all-completions "" collection))
                 (car candidates))))
      (opencode--select-session
       nil "OpenCode session: "
       (lambda (session _info)
         (setq selected-session session))))
    (should (equal candidates '("Parent")))
    (should (equal (opencode-session-id selected-session) "parent"))))

(ert-deftest test-opencode/project-session-filter-global-uses-directory ()
  "Sessions outside a project are filtered by directory."
  (let ((conn (opencode-connection-create :directory "/tmp/loose/"))
        filter)
    (cl-letf (((symbol-function 'opencode-client-location)
               (lambda (_connection &rest args)
                 (funcall (plist-get args :success)
                          :data '((project . ((id . "global"))))))))
      (opencode--project-session-filter conn (lambda (result) (setq filter result))))
    (should (equal filter (list :directory (directory-file-name
                                             (file-truename "/tmp/loose/")))))))

(ert-deftest test-opencode/create-session-uses-default-agent ()
  "New sessions start with the default agent when the server has it."
  (let ((opencode-session-default-agent "plan")
        created)
    (cl-letf (((symbol-function 'opencode-session--ensure-agents)
               (lambda (_conn on-ready) (funcall on-ready '("build" "plan"))))
              ((symbol-function 'opencode-client-session-create)
               (lambda (_conn &rest args)
                 (setq created (plist-get args :agent))
                 (funcall (plist-get args :success) :data '((id . "s1"))))))
      (opencode--create-session 'conn #'ignore))
    (should (equal created "plan"))
    (let ((opencode-session-default-agent "missing"))
      (cl-letf (((symbol-function 'opencode-session--ensure-agents)
                 (lambda (_conn on-ready) (funcall on-ready '("build"))))
                ((symbol-function 'opencode-client-session-create)
                 (lambda (_conn &rest args)
                   (setq created (plist-get args :agent))
                   (funcall (plist-get args :success) :data '((id . "s1"))))))
        (opencode--create-session 'conn #'ignore))
      (should-not created))))

(ert-deftest test-opencode/send-context-captures-calling-buffer ()
  "Context comes from the buffer that ran the command, not a later one."
  (let (sent)
    (cl-letf (((symbol-function 'opencode-run-server)
               (lambda (_dir on-ready) (run-with-timer 0 nil on-ready 'conn)))
              ((symbol-function 'opencode--select-session)
               (lambda (_conn _prompt on-selected)
                 (funcall on-selected (opencode-session-create :id "s1") nil)))
              ((symbol-function 'opencode--ensure-session-buffer)
               (lambda (_session _conn on-ready)
                 (with-temp-buffer
                   (cl-letf (((symbol-function 'opencode-session-insert-input)
                              (lambda (input) (setq sent input)))
                             ((symbol-function 'opencode-session-send-input) #'ignore))
                     (funcall on-ready (current-buffer)))))))
      (with-temp-buffer
        (insert "unique context line\n")
        (opencode-send-context-to-session "/tmp/" "question"))
      (with-temp-buffer
        (insert "wrong buffer\n")
        (sit-for 0.05)))
    (should (string-match-p "unique context line" sent))
    (should-not (string-match-p "wrong buffer" sent))))

(provide 'emacs-opencode-test)

;;; emacs-opencode-test.el ends here
