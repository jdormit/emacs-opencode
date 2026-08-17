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
  "Create a session struct from an alist."
  (let* ((data '((id . "s1")
                 (slug . "my-session")
                 (version . 1)
                 (projectID . "p1")
                 (directory . "/tmp")
                 (title . "Test Session")
                 (time . ((created . "2024-01-01") (updated . "2024-01-02")))
                 (summary . "A summary")))
         (session (opencode--session-from-data data)))
    (should (opencode-session-p session))
    (should (equal (opencode-session-id session) "s1"))
    (should (equal (opencode-session-slug session) "my-session"))
    (should (equal (opencode-session-title session) "Test Session"))
    (should (equal (opencode-session-time-created session) "2024-01-01"))
    (should (equal (opencode-session-time-updated session) "2024-01-02"))
    (should (equal (opencode-session-summary session) "A summary"))
    ;; info should be the original data
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
  "Include slug and ID when requested."
  (let ((result (opencode--session-label
                 '((title . "Hello") (slug . "hello") (id . "s1"))
                 t)))
    (should (string-match-p "Hello" result))
    (should (string-match-p "(hello)" result))
    (should (string-match-p "\\[s1\\]" result))))

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
    (cl-letf (((symbol-function 'opencode-client-sessions)
               (lambda (_connection &rest args)
                 (should (plist-get args :roots))
                 (funcall (plist-get args :success)
                          :data [((id . "parent") (title . "Parent"))
                                 ((id . "child") (title . "Child")
                                  (parentID . "parent"))])))
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

(provide 'emacs-opencode-test)

;;; emacs-opencode-test.el ends here
