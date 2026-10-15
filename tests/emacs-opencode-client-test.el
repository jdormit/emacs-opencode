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

(defun opencode-client-test--expected-directory (directory)
  "Return the header value expected for DIRECTORY."
  (url-hexify-string (directory-file-name (file-truename directory))))

(ert-deftest test-opencode-client/request-sends-directory-header ()
  "Every request includes an url-encoded x-opencode-directory header."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-request conn 'GET "/api/agent")
      (should (equal url "http://127.0.0.1:4096/api/agent"))
      (let ((headers (plist-get args :headers)))
        (should (equal (cdr (assoc "x-opencode-directory" headers))
                       (opencode-client-test--expected-directory "/tmp/project/")))))))

(ert-deftest test-opencode-client/request-directory-header-strips-trailing-slash ()
  "Directory header value has no trailing slash."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-request conn 'GET "/api/agent")
      (let* ((headers (plist-get args :headers))
             (value (cdr (assoc "x-opencode-directory" headers))))
        (should-not (string-suffix-p "%2F" value))
        (should-not (string-suffix-p "/" value))))))

(ert-deftest test-opencode-client/request-directory-header-resolves-symlinks ()
  "The directory header names the symlink target, as the server does."
  (let* ((target (make-temp-file "opencode-target" t))
         (link (concat target "-link")))
    (unwind-protect
        (progn
          (make-symbolic-link target link)
          (let ((conn (opencode-client-test--connection (file-name-as-directory link))))
            (opencode-client-test--with-captured-request _url args
              (opencode-request conn 'GET "/api/agent")
              (should (equal (cdr (assoc "x-opencode-directory" (plist-get args :headers)))
                             (url-hexify-string (file-truename target)))))))
      (delete-file link)
      (delete-directory target))))

(ert-deftest test-opencode-client/request-directory-from-calling-context ()
  "The shared connection routes requests using the calling directory."
  (let ((conn (opencode-client-test--connection nil)))
    (opencode-client-test--with-captured-request _url args
      (opencode-request conn 'GET "/api/agent")
      (should (equal (cdr (assoc "x-opencode-directory" (plist-get args :headers)))
                     (url-hexify-string (directory-file-name
                                         (file-truename default-directory))))))))

(ert-deftest test-opencode-client/request-directory-header-with-json ()
  "Directory header coexists with the JSON content type header."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-request conn 'POST "/api/session/s1/interrupt" :json '((a . 1)))
      (let ((headers (plist-get args :headers)))
        (should (assoc "x-opencode-directory" headers))
        (should (equal (cdr (assoc "Content-Type" headers))
                       "application/json"))))))

(ert-deftest test-opencode-client/request-respects-caller-directory-header ()
  "An explicit x-opencode-directory header is not overridden."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-request conn 'GET "/api/agent"
                        :headers '(("x-opencode-directory" . "custom")))
      (let ((headers (plist-get args :headers)))
        (should (equal (cdr (assoc "x-opencode-directory" headers)) "custom"))
        (should (= 1 (cl-count "x-opencode-directory" headers
                               :key #'car :test #'equal)))))))

(ert-deftest test-opencode-client/request-sends-basic-auth ()
  "Requests authenticate with the connection's credentials."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (setf (opencode-connection-username conn) "opencode")
    (setf (opencode-connection-password conn) "pw")
    (opencode-client-test--with-captured-request _url args
      (opencode-request conn 'GET "/api/info")
      (should-not (plist-get args :auth))
      (should (equal (cdr (assoc "Authorization" (plist-get args :headers)))
                     (concat "Basic " (base64-encode-string "opencode:pw" t)))))))

;;; response unwrapping

(ert-deftest test-opencode-client/unwrap-passes-data-field ()
  "Wrapped success callbacks receive the response data field."
  (let ((conn (opencode-client-test--connection "/tmp/project/"))
        received)
    (opencode-client-test--with-captured-request _url args
      (opencode-client-agents conn :success (lambda (&rest cb-args)
                                              (setq received (plist-get cb-args :data))))
      (funcall (plist-get args :success)
               :data '((location . ((directory . "/tmp")))
                       (data . (((id . "build")))))))
    (should (equal received '(((id . "build")))))))

;;; health and location

(ert-deftest test-opencode-client/health-hits-info ()
  "Health checks use the server info endpoint."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-health conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/info"))
      (should (equal (plist-get args :type) "GET")))))

;;; session-create

(ert-deftest test-opencode-client/session-create-sends-location ()
  "Session creation names the connection directory as its location."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-create conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data)
                     (json-encode
                      `((location . ((directory . ,(directory-file-name
                                                    (file-truename "/tmp/project/"))))))))))))

(ert-deftest test-opencode-client/session-create-sends-agent-and-model ()
  "Session creation forwards the initial agent and model."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-session-create
       conn :agent "plan" :model '((providerID . "openai") (id . "gpt"))
       :success #'ignore :error #'ignore)
      (let ((body (json-read-from-string (plist-get args :data))))
        (should (equal (alist-get 'agent body) "plan"))
        (should (equal (alist-get 'model body)
                       '((providerID . "openai") (id . "gpt"))))))))

;;; sessions

(ert-deftest test-opencode-client/sessions-no-limit-omits-params ()
  "Listing sessions without filters sends no query params."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-sessions conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session"))
      (should (equal (plist-get args :type) "GET"))
      (should (null (plist-get args :params)))
      (should (null (plist-get args :data))))))

(ert-deftest test-opencode-client/sessions-forwards-filters ()
  "Root, limit, and project filters become query parameters."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-sessions conn :limit 1000 :roots t :project "prj"
                                :success #'ignore :error #'ignore)
      (should (equal (plist-get args :params)
                     '(("parentID" . "null") ("limit" . 1000) ("project" . "prj")))))))

(ert-deftest test-opencode-client/sessions-forwards-directory ()
  "A directory filter becomes a query parameter."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-sessions conn :directory "/tmp/project"
                                :success #'ignore :error #'ignore)
      (should (equal (plist-get args :params) '(("directory" . "/tmp/project")))))))

(ert-deftest test-opencode-client/sessions-forwards-parent ()
  "A parent filter lists that session's children."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-sessions conn :parent "ses_1" :limit 500
                                :success #'ignore :error #'ignore)
      (should (equal (plist-get args :params)
                     '(("parentID" . "ses_1") ("limit" . 500)))))))

;;; session-messages

(ert-deftest test-opencode-client/session-messages-pages-through-history ()
  "History loading follows cursors until a short page arrives."
  (let* ((conn (opencode-client-test--connection "/tmp/project/"))
         (opencode-client--message-page-size 2)
         (calls nil)
         (result nil)
         (pages (list '((data . (((id . "m1")) ((id . "m2"))))
                        (cursor . ((next . "c1"))))
                      '((data . (((id . "m3"))))
                        (cursor . ((next . "c2")))))))
    (cl-letf (((symbol-function 'request)
               (lambda (url &rest args)
                 (push (cons url (plist-get args :params)) calls)
                 (funcall (plist-get args :success) :data (pop pages)))))
      (opencode-client-session-messages
       conn "ses_1" :success (lambda (&rest args) (setq result (plist-get args :data)))))
    (should (equal (mapcar (lambda (m) (alist-get 'id m)) result) '("m1" "m2" "m3")))
    (setq calls (nreverse calls))
    (should (equal (car (car calls)) "http://127.0.0.1:4096/api/session/ses_1/message"))
    (should (equal (cdr (car calls)) '(("limit" . 2) ("order" . "asc"))))
    (should (equal (cdr (cadr calls)) '(("limit" . 2) ("cursor" . "c1"))))))

;;; session-message

(ert-deftest test-opencode-client/session-message-fetches-single-message ()
  "Fetching one message hits the message endpoint with both IDs."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-message conn "ses_1" "msg_1"
                                        :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/message/msg_1"))
      (should (equal (plist-get args :type) "GET"))
      (should (null (plist-get args :data))))))

;;; session-fork

(ert-deftest test-opencode-client/session-fork-without-message-sends-empty-object ()
  "Forking a whole session posts an empty JSON object."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-fork conn "ses_1" :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/fork"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data) "{}")))))

(ert-deftest test-opencode-client/session-fork-with-message-sends-boundary ()
  "Forking at a message sends it as the fork boundary."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-session-fork conn "ses_1" :message-id "msg_1"
                                    :success #'ignore :error #'ignore)
      (should (equal (plist-get args :data)
                     (json-encode '((before . "msg_1"))))))))

;;; session-rename

(ert-deftest test-opencode-client/session-rename-sends-title ()
  "Renaming a session patches its title."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-rename conn "ses_1" "New title"
                                      :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1"))
      (should (equal (plist-get args :type) "PATCH"))
      (should (equal (plist-get args :data)
                     (json-encode '((title . "New title"))))))))

;;; session-compact

(ert-deftest test-opencode-client/session-compact-posts-empty-object ()
  "Compacting a session needs no model."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-compact conn "ses_1" :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/compact"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data) "{}")))))

;;; agent and model switching

(ert-deftest test-opencode-client/session-switch-agent ()
  "Switching agents posts the agent name."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-switch-agent conn "ses_1" "plan")
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/agent"))
      (should (equal (plist-get args :data) (json-encode '((agent . "plan"))))))))

(ert-deftest test-opencode-client/session-switch-model ()
  "Switching models posts the model reference."
  (let ((conn (opencode-client-test--connection "/tmp/project/"))
        (model '((providerID . "openai") (id . "gpt") (variant . "high"))))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-switch-model conn "ses_1" model)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/model"))
      (should (equal (plist-get args :data) (json-encode `((model . ,model))))))))

;;; prompt, command, shell

(ert-deftest test-opencode-client/session-prompt-sends-text-and-agents ()
  "Prompts carry text and mentioned agents."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-prompt conn "ses_1" "hi @explore" :agents '("explore"))
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/prompt"))
      (should (equal (plist-get args :data)
                     (json-encode '((text . "hi @explore")
                                    (agents . [((name . "explore"))]))))))))

(ert-deftest test-opencode-client/session-prompt-omits-empty-agents ()
  "Prompts without mentions send only text."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-session-prompt conn "ses_1" "hi")
      (should (equal (plist-get args :data) (json-encode '((text . "hi"))))))))

(ert-deftest test-opencode-client/session-command-sends-name-and-text ()
  "Commands send their name and argument text."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-command conn "ses_1" "review" "HEAD")
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/command"))
      (should (equal (plist-get args :data)
                     (json-encode '((name . "review") (text . "HEAD"))))))))

(ert-deftest test-opencode-client/session-shell-sends-command ()
  "Shell prompts send the command string."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-shell conn "ses_1" "ls")
      (should (plist-member args :timeout))
      (should-not (plist-get args :timeout))
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/shell"))
      (should (equal (plist-get args :data) (json-encode '((command . "ls"))))))))

(ert-deftest test-opencode-client/session-interrupt ()
  "Interrupting posts to the interrupt endpoint."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-interrupt conn "ses_1")
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/interrupt"))
      (should (equal (plist-get args :type) "POST")))))

(ert-deftest test-opencode-client/session-background ()
  "Backgrounding posts to the background endpoint."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-background conn "ses_1")
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/background"))
      (should (equal (plist-get args :type) "POST")))))

;;; permissions and forms

(ert-deftest test-opencode-client/permission-reply-sends-decision ()
  "Permission replies are scoped to the session and send a decision."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-permission-reply conn "ses_1" "per_1" "always")
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/permission/per_1/reply"))
      (should (equal (plist-get args :data) (json-encode '((decision . "always"))))))))

(ert-deftest test-opencode-client/form-reply-sends-answer ()
  "Form replies send the answer object."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-form-reply conn "ses_1" "frm_1" '((q0 . "Blue")))
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/form/frm_1/reply"))
      (should (equal (plist-get args :data)
                     (json-encode '((answer . ((q0 . "Blue"))))))))))

(ert-deftest test-opencode-client/form-cancel-deletes-form ()
  "Cancelling a form deletes it."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-form-cancel conn "ses_1" "frm_1")
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/form/frm_1"))
      (should (equal (plist-get args :type) "DELETE")))))

;;; integrations

(ert-deftest test-opencode-client/integration-connect-key ()
  "Key connections post the key to the integration."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-integration-connect-key conn "anthropic" "sk-1")
      (should (equal url "http://127.0.0.1:4096/api/integration/anthropic/connect/key"))
      (should (equal (plist-get args :data) (json-encode '((key . "sk-1"))))))))

(ert-deftest test-opencode-client/integration-oauth-connect ()
  "OAuth connections name the method and form answer."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-integration-oauth-connect conn "github-copilot" "device"
                                                 :answer '((deploymentType . "github.com")))
      (should (equal url "http://127.0.0.1:4096/api/integration/github-copilot/connect/oauth"))
      (should (equal (plist-get args :data)
                     (json-encode '((methodID . "device")
                                    (answer . ((deploymentType . "github.com"))))))))))

(ert-deftest test-opencode-client/integration-oauth-complete ()
  "Code-based OAuth completion sends the code."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-integration-oauth-complete conn "openai" "con_1" :code "abc")
      (should (equal url "http://127.0.0.1:4096/api/integration/openai/connect/oauth/con_1/complete"))
      (should (equal (plist-get args :data) (json-encode '((code . "abc"))))))))

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

(provide 'emacs-opencode-client-test)

;;; emacs-opencode-client-test.el ends here
