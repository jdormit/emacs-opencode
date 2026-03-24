;;; emacs-opencode-connection-test.el --- Tests for connection management  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-opencode-connection)

;;; base-url

(ert-deftest test-opencode-connection/base-url ()
  "Build a base URL from hostname and port."
  (should (equal (opencode-connection--base-url "127.0.0.1" 4096)
                 "http://127.0.0.1:4096")))

(ert-deftest test-opencode-connection/base-url-custom ()
  "Build a base URL with custom hostname."
  (should (equal (opencode-connection--base-url "example.com" 8080)
                 "http://example.com:8080")))

;;; process-environment

(ert-deftest test-opencode-connection/process-environment-set ()
  "Set an environment variable."
  (let ((process-environment '("HOME=/home/user" "PATH=/usr/bin")))
    (let ((result (opencode-connection--process-environment
                   '(("FOO" . "bar")))))
      (should (member "FOO=bar" result))
      (should (member "HOME=/home/user" result)))))

(ert-deftest test-opencode-connection/process-environment-override ()
  "Override an existing environment variable."
  (let ((process-environment '("FOO=old" "PATH=/usr/bin")))
    (let ((result (opencode-connection--process-environment
                   '(("FOO" . "new")))))
      (should (member "FOO=new" result))
      (should-not (member "FOO=old" result)))))

(ert-deftest test-opencode-connection/process-environment-unset ()
  "Unset an environment variable."
  (let ((process-environment '("FOO=bar" "PATH=/usr/bin")))
    (let ((result (opencode-connection--process-environment
                   '(("FOO" . nil)))))
      (should-not (cl-find-if (lambda (item) (string-prefix-p "FOO=" item))
                              result))
      (should (member "PATH=/usr/bin" result)))))

(ert-deftest test-opencode-connection/process-environment-empty ()
  "Empty environment alist returns a copy of process-environment."
  (let ((process-environment '("HOME=/home" "PATH=/usr/bin")))
    (let ((result (opencode-connection--process-environment nil)))
      (should (equal result process-environment))
      ;; Should be a copy, not the same object
      (should-not (eq result process-environment)))))

;;; connection struct

(ert-deftest test-opencode-connection/create-struct ()
  "Create a connection struct with fields."
  (let ((conn (opencode-connection-create
               :base-url "http://localhost:4096"
               :hostname "localhost"
               :port 4096
               :directory "/tmp/")))
    (should (opencode-connection-p conn))
    (should (equal (opencode-connection-base-url conn) "http://localhost:4096"))
    (should (equal (opencode-connection-hostname conn) "localhost"))
    (should (= (opencode-connection-port conn) 4096))
    (should (equal (opencode-connection-directory conn) "/tmp/"))))

;;; alive-p

(ert-deftest test-opencode-connection/alive-p-no-process ()
  "Return nil when no process exists."
  (let ((conn (opencode-connection-create)))
    (should (null (opencode-connection-alive-p conn)))))

(provide 'emacs-opencode-connection-test)

;;; emacs-opencode-connection-test.el ends here
