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

;;; password and server environment

(ert-deftest test-opencode-connection/generate-password ()
  "Generated passwords are non-empty, distinct sha256 hashes."
  (let ((passwords (cl-loop repeat 5 collect
                            (opencode-connection--generate-password))))
    (dolist (password passwords)
      (should (stringp password))
      (should (= (length password) 64)))
    (should (= (length (delete-dups (copy-sequence passwords))) 5))))

(ert-deftest test-opencode-connection/resolve-password-prefers-custom ()
  "`opencode-server-password' wins over the server environment."
  (let ((opencode-server-password "custom-pw")
        (opencode-server-environment '(("OPENCODE_PASSWORD" . "env-pw"))))
    (should (equal (opencode-connection--resolve-password) "custom-pw"))))

(ert-deftest test-opencode-connection/resolve-password-uses-environment ()
  "An OPENCODE_PASSWORD in the server environment is reused."
  (let ((opencode-server-password nil)
        (opencode-server-environment '(("OPENCODE_PASSWORD" . "env-pw"))))
    (should (equal (opencode-connection--resolve-password) "env-pw"))))

(ert-deftest test-opencode-connection/resolve-password-generates-fallback ()
  "A password is generated when nothing is configured."
  (let ((opencode-server-password nil)
        (opencode-server-environment nil))
    (let ((password (opencode-connection--resolve-password)))
      (should (stringp password))
      (should (= (length password) 64)))))

(ert-deftest test-opencode-connection/server-environment-injects-password ()
  "The connection password is exported as OPENCODE_PASSWORD."
  (let ((opencode-server-environment '(("FOO" . "bar"))))
    (should (equal (opencode-connection--server-environment
                    (opencode-connection-create :password "conn-pw"))
                   '(("OPENCODE_PASSWORD" . "conn-pw")
                     ("FOO" . "bar"))))))

(ert-deftest test-opencode-connection/server-environment-keeps-existing ()
  "A user-supplied OPENCODE_PASSWORD is not overridden."
  (let ((opencode-server-environment '(("OPENCODE_PASSWORD" . "user-pw"))))
    (should (equal (opencode-connection--server-environment
                    (opencode-connection-create :password "conn-pw"))
                   '(("OPENCODE_PASSWORD" . "user-pw"))))))

(ert-deftest test-opencode-connection/server-environment-without-password ()
  "No OPENCODE_PASSWORD is added when the connection has no password."
  (let ((opencode-server-environment '(("FOO" . "bar"))))
    (should (equal (opencode-connection--server-environment
                    (opencode-connection-create))
                   '(("FOO" . "bar"))))))

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

(ert-deftest test-opencode-connection/create-for-directory-basic-auth ()
  "New connections use the opencode user and the resolved password."
  (let ((opencode-server-password "fixed-pw"))
    (let ((conn (opencode-connection-create-for-directory
                 "/tmp/project" "127.0.0.1" 4096)))
      (should (equal (opencode-connection-username conn) "opencode"))
      (should (equal (opencode-connection-password conn) "fixed-pw"))
      (should (equal (opencode-connection-directory conn) "/tmp/project/")))))

;;; alive-p

(ert-deftest test-opencode-connection/alive-p-no-process ()
  "Return nil when no process exists."
  (let ((conn (opencode-connection-create)))
    (should (null (opencode-connection-alive-p conn)))))

;;; maybe-ready

(ert-deftest test-opencode-connection/maybe-ready-matches ()
  "Call ready callback when output contains the ready string."
  (let* ((conn (opencode-connection-create))
         (called nil)
         (fake-process (start-process "test-proc" nil "true")))
    (unwind-protect
        (progn
          ;; Stub out the provider/command fetches that fire on ready
          (cl-letf (((symbol-function 'opencode-connection-ensure-providers)
                     (lambda (&rest _) nil))
                    ((symbol-function 'opencode-connection-ensure-commands)
                     (lambda (&rest _) nil)))
            (opencode-connection--maybe-ready
             fake-process
             "server listening on http://127.0.0.1:4096"
             conn
             (lambda (_proc) (setq called t))))
          (should called))
      (when (process-live-p fake-process)
        (delete-process fake-process)))))

(ert-deftest test-opencode-connection/maybe-ready-no-match ()
  "Don't call callback when output doesn't contain ready string."
  (let* ((conn (opencode-connection-create))
         (called nil)
         (fake-process (start-process "test-proc" nil "true")))
    (unwind-protect
        (progn
          (opencode-connection--maybe-ready
           fake-process
           "some other output"
           conn
           (lambda (_proc) (setq called t)))
          (should (null called)))
      (when (process-live-p fake-process)
        (delete-process fake-process)))))

(provide 'emacs-opencode-connection-test)

;;; emacs-opencode-connection-test.el ends here
