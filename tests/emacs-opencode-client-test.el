;;; emacs-opencode-client-test.el --- Tests for HTTP client  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-opencode-client)

(defmacro opencode-client-test--with-captured-request (url-var args-var &rest body)
  "Run BODY with `request' stubbed to capture its arguments.
URL-VAR and ARGS-VAR are bound to the URL and keyword arguments of the
last `request' call made during BODY."
  (declare (indent 2))
  `(let ((,url-var nil)
         (,args-var nil))
     (cl-letf (((symbol-function 'request)
                (lambda (&rest request-args)
                  (setq ,url-var (car request-args)
                        ,args-var (cdr request-args))
                  nil)))
       ,@body)))

(defun opencode-client-test--connection (&optional directory)
  "Return a connection for DIRECTORY pointed at a fake base URL."
  (opencode-connection-create
   :base-url "http://127.0.0.1:4096"
   :hostname "127.0.0.1"
   :port 4096
   :directory directory
   :timeout 10))

;;; directory header

(ert-deftest test-opencode-client/request-sends-directory-header ()
  "Every request includes an url-encoded x-opencode-directory header."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-request conn 'GET "/agent")
      (should (equal url "http://127.0.0.1:4096/agent"))
      (let ((headers (plist-get args :headers)))
        (should (equal (cdr (assoc "x-opencode-directory" headers))
                       (url-hexify-string "/tmp/project")))))))

(ert-deftest test-opencode-client/request-directory-header-strips-trailing-slash ()
  "Directory header value has no trailing slash."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-request conn 'GET "/agent")
      (let* ((headers (plist-get args :headers))
             (value (cdr (assoc "x-opencode-directory" headers))))
        (should-not (string-suffix-p "%2F" value))
        (should-not (string-suffix-p "/" value))))))

(ert-deftest test-opencode-client/request-no-directory-header-without-directory ()
  "No directory header when the connection has no directory."
  (let ((conn (opencode-client-test--connection nil)))
    (opencode-client-test--with-captured-request _url args
      (opencode-request conn 'GET "/agent")
      (should-not (assoc "x-opencode-directory" (plist-get args :headers))))))

(ert-deftest test-opencode-client/request-directory-header-with-json ()
  "Directory header coexists with the JSON content type header."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-request conn 'POST "/session/s1/abort" :json '((a . 1)))
      (let ((headers (plist-get args :headers)))
        (should (assoc "x-opencode-directory" headers))
        (should (equal (cdr (assoc "Content-Type" headers))
                       "application/json"))))))

(ert-deftest test-opencode-client/request-respects-caller-directory-header ()
  "An explicit x-opencode-directory header is not overridden."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-request conn 'GET "/agent"
                        :headers '(("x-opencode-directory" . "custom")))
      (let ((headers (plist-get args :headers)))
        (should (equal (cdr (assoc "x-opencode-directory" headers)) "custom"))
        (should (= 1 (cl-count "x-opencode-directory" headers
                               :key #'car :test #'equal)))))))

;;; session-create

(ert-deftest test-opencode-client/session-create-sends-no-body ()
  "Session creation posts /session with no request body."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-create conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/session"))
      (should (equal (plist-get args :type) "POST"))
      (should (null (plist-get args :data))))))

;;; sessions

(ert-deftest test-opencode-client/sessions-no-limit-omits-params ()
  "Listing sessions without a limit sends no query params."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-sessions conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/session"))
      (should (equal (plist-get args :type) "GET"))
      (should (null (plist-get args :params)))
      (should (null (plist-get args :data))))))

(ert-deftest test-opencode-client/sessions-forwards-limit ()
  "A limit is forwarded as a limit query parameter, not a request body."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-sessions conn :limit 1000
                                :success #'ignore :error #'ignore)
      (should (equal (plist-get args :params) '(("limit" . 1000))))
      (should (null (plist-get args :data))))))

(ert-deftest test-opencode-client/sessions-forwards-roots ()
  "Root-only session lists request server-side filtering."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-sessions conn :limit 1000 :roots t
                                :success #'ignore :error #'ignore)
      (should (equal (plist-get args :params)
                     '(("roots" . "true") ("limit" . 1000)))))))

;;; session-messages

(ert-deftest test-opencode-client/session-messages-no-limit-omits-params ()
  "Fetching messages without a limit sends no query params."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-messages conn "ses_1"
                                         :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/session/ses_1/message"))
      (should (equal (plist-get args :type) "GET"))
      (should (null (plist-get args :params)))
      (should (null (plist-get args :data))))))

(ert-deftest test-opencode-client/session-messages-forwards-limit ()
  "A limit is forwarded as a limit query parameter, not a request body."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-session-messages conn "ses_1" :limit 5
                                         :success #'ignore :error #'ignore)
      (should (equal (plist-get args :params) '(("limit" . 5))))
      (should (null (plist-get args :data))))))

;;; session-fork

(ert-deftest test-opencode-client/session-fork-without-message-sends-no-body ()
  "Forking a whole session posts no request body."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-fork conn "ses_1" :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/session/ses_1/fork"))
      (should (equal (plist-get args :type) "POST"))
      (should (null (plist-get args :data))))))

(ert-deftest test-opencode-client/session-fork-with-message-sends-message-id ()
  "Forking at a message sends MESSAGE-ID in the JSON body."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-session-fork conn "ses_1" :message-id "msg_1"
                                    :success #'ignore :error #'ignore)
      (should (equal (plist-get args :data)
                     (json-encode '((messageID . "msg_1"))))))))

;;; session-rename

(ert-deftest test-opencode-client/session-rename-sends-title ()
  "Renaming a session patches its title."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-rename conn "ses_1" "New title"
                                      :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/session/ses_1"))
      (should (equal (plist-get args :type) "PATCH"))
      (should (equal (plist-get args :data)
                     (json-encode '((title . "New title"))))))))

;;; session-compact

(ert-deftest test-opencode-client/session-compact-sends-model ()
  "Compacting a session posts selected model data."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-compact conn "ses_1" '("anthropic" . "claude")
                                       :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/session/ses_1/summarize"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data)
                     (json-encode '((providerID . "anthropic")
                                    (modelID . "claude")
                                    (auto . :json-false))))))))

;;; format-error

(ert-deftest test-opencode-client/format-error-status-and-tag ()
  "Format status code and server error tag."
  (let ((response (make-request-response :status-code 400)))
    (should (equal (opencode-client-format-error
                    (list :response response :data '((_tag . "BadRequest"))))
                   "HTTP 400: BadRequest"))))

(ert-deftest test-opencode-client/format-error-prefers-message-field ()
  "Prefer a server-provided message over the error tag."
  (let ((response (make-request-response :status-code 400)))
    (should (equal (opencode-client-format-error
                    (list :response response
                          :data '((_tag . "BadRequest")
                                  (message . "directory is invalid"))))
                   "HTTP 400: directory is invalid"))))

(ert-deftest test-opencode-client/format-error-status-only ()
  "Fall back to the status code alone."
  (let ((response (make-request-response :status-code 500)))
    (should (equal (opencode-client-format-error (list :response response))
                   "HTTP 500"))))

(ert-deftest test-opencode-client/format-error-error-thrown ()
  "Fall back to the thrown error when there is no response."
  (should (equal (opencode-client-format-error
                  (list :error-thrown '(error . "connection refused")))
                 "(error . connection refused)")))

(ert-deftest test-opencode-client/format-error-nil-without-detail ()
  "Return nil when no detail is available."
  (should (null (opencode-client-format-error nil))))

;;; vectorize-answers

(ert-deftest test-opencode-client/vectorize-answers-list-of-lists ()
  "Convert list of lists to vector of vectors."
  (let ((result (opencode--vectorize-answers '(("a" "b") ("c")))))
    (should (vectorp result))
    (should (= (length result) 2))
    (should (vectorp (aref result 0)))
    (should (equal (aref result 0) ["a" "b"]))
    (should (equal (aref result 1) ["c"]))))

(ert-deftest test-opencode-client/vectorize-answers-vector-input ()
  "Handle vector input."
  (let ((result (opencode--vectorize-answers [("a") ("b")])))
    (should (vectorp result))
    (should (= (length result) 2))))

(ert-deftest test-opencode-client/vectorize-answers-strings ()
  "Wrap plain strings in vectors."
  (let ((result (opencode--vectorize-answers '("hello" "world"))))
    (should (vectorp result))
    (should (equal (aref result 0) ["hello"]))
    (should (equal (aref result 1) ["world"]))))

(ert-deftest test-opencode-client/vectorize-answers-mixed ()
  "Handle mixed input types."
  (let ((result (opencode--vectorize-answers '(["a"] ("b") "c"))))
    (should (vectorp result))
    (should (equal (aref result 0) ["a"]))
    (should (equal (aref result 1) ["b"]))
    (should (equal (aref result 2) ["c"]))))

(ert-deftest test-opencode-client/vectorize-answers-nil ()
  "Handle nil input."
  (let ((result (opencode--vectorize-answers nil)))
    (should (vectorp result))
    (should (= (length result) 0))))

(provide 'emacs-opencode-client-test)

;;; emacs-opencode-client-test.el ends here
