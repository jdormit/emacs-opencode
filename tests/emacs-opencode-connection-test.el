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

;;; maybe-ready

(defmacro test-opencode-connection--with-process (var &rest body)
  "Bind VAR to a throwaway process while running BODY."
  (declare (indent 1))
  `(let ((,var (start-process "test-proc" nil "true")))
     (unwind-protect
         (cl-letf (((symbol-function 'opencode-connection-ensure-models)
                    (lambda (&rest _) nil))
                   ((symbol-function 'opencode-connection-ensure-commands)
                    (lambda (&rest _) nil)))
           ,@body)
       (when (process-live-p ,var)
         (delete-process ,var)))))

(ert-deftest test-opencode-connection/maybe-ready-matches ()
  "Call the ready callback once the server reports its URL."
  (test-opencode-connection--with-process fake-process
    (let ((conn (opencode-connection-create :password "pw"))
          (called nil))
      (opencode-connection--maybe-ready
       fake-process "server listening on http://127.0.0.1:4096\n"
       conn (lambda (_proc) (setq called t)))
      (should called))))

(ert-deftest test-opencode-connection/maybe-ready-split-across-chunks ()
  "The ready line may arrive in pieces."
  (test-opencode-connection--with-process fake-process
    (let ((conn (opencode-connection-create :password "pw"))
          (called nil))
      (opencode-connection--maybe-ready
       fake-process "server listen" conn (lambda (_proc) (setq called t)))
      (should-not called)
      (opencode-connection--maybe-ready
       fake-process "ing on http://127.0.0.1:4096\n" conn (lambda (_proc) (setq called t)))
      (should called))))

(ert-deftest test-opencode-connection/server-environment-sets-password ()
  "The server gets an explicit password that overrides an inherited one."
  (let ((process-environment '("OPENCODE_PASSWORD=inherited" "HOME=/h"))
        (opencode-server-environment '(("FOO" . "bar"))))
    (let ((env (opencode-connection--server-environment "chosen")))
      (should (member "OPENCODE_PASSWORD=chosen" env))
      (should-not (member "OPENCODE_PASSWORD=inherited" env))
      (should (member "FOO=bar" env)))))

(ert-deftest test-opencode-connection/generated-passwords-differ ()
  "Each server gets a fresh, non-trivial password."
  (let ((a (opencode-connection--generate-password))
        (b (opencode-connection--generate-password)))
    (should (>= (length a) 40))
    (should-not (equal a b))))

(ert-deftest test-opencode-connection/start-passes-password ()
  "Starting a server records the password it was given."
  (let ((conn (opencode-connection-create :directory "/tmp/" :hostname "127.0.0.1" :port 1))
        env)
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest _) (setq env process-environment) (make-process :name "x" :command '("true"))))
              ((symbol-function 'executable-find) (lambda (_) "/bin/true")))
      (let ((opencode-server-environment nil))
        (opencode-connection-start conn)))
    (should (equal (opencode-connection-username conn) "opencode"))
    (should (member (concat "OPENCODE_PASSWORD=" (opencode-connection-password conn)) env))))

(ert-deftest test-opencode-connection/empty-model-catalog-is-loaded ()
  "An empty model list counts as loaded and is not fetched again."
  (let ((conn (opencode-connection-create))
        (fetches 0))
    (cl-letf (((symbol-function 'opencode-client-models)
               (lambda (_conn &rest args) (cl-incf fetches) (funcall (plist-get args :success) :data nil)))
              ((symbol-function 'opencode-client-integrations)
               (lambda (_conn &rest args) (funcall (plist-get args :success) :data nil))))
      (opencode-connection-ensure-models conn)
      (opencode-connection-ensure-models conn)
      (should (= fetches 1))
      (should (eq (opencode-connection-models-state conn) :loaded))
      (opencode-connection-invalidate-models conn)
      (opencode-connection-ensure-models conn)
      (should (= fetches 2)))))

(ert-deftest test-opencode-connection/empty-command-list-is-loaded ()
  "An empty command list counts as loaded and is not fetched again."
  (let ((conn (opencode-connection-create))
        (fetches 0))
    (cl-letf (((symbol-function 'opencode-client-commands)
               (lambda (_conn &rest args) (cl-incf fetches) (funcall (plist-get args :success) :data nil))))
      (opencode-connection-ensure-commands conn)
      (opencode-connection-ensure-commands conn)
      (should (= fetches 1)))))

(ert-deftest test-opencode-connection/environment-password ()
  "Read a server password from the configured environment."
  (should (equal (opencode-connection--environment-password
                  '(("OPENCODE_SERVER_PASSWORD" . "legacy")))
                 "legacy"))
  (should (equal (opencode-connection--environment-password
                  '(("OPENCODE_PASSWORD" . "new")
                    ("OPENCODE_SERVER_PASSWORD" . "legacy")))
                 "new"))
  (should-not (opencode-connection--environment-password '(("FOO" . "bar")))))

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

(ert-deftest test-opencode-connection/invalidated-load-is-discarded ()
  "A catalog load that was invalidated while in flight does not mark it loaded."
  (let ((conn (opencode-connection-create))
        callbacks)
    (cl-letf (((symbol-function 'opencode-client-models)
               (lambda (_conn &rest args) (push (plist-get args :success) callbacks)))
              ((symbol-function 'opencode-client-integrations)
               (lambda (_conn &rest args) (push (plist-get args :success) callbacks))))
      (opencode-connection-ensure-models conn)
      (opencode-connection-invalidate-models conn)
      (dolist (callback callbacks) (funcall callback :data '(stale)))
      (should-not (opencode-connection-models-state conn))
      (should-not (opencode-connection-models conn)))))

(provide 'emacs-opencode-connection-test)

;;; emacs-opencode-connection-test.el ends here
