;;; emacs-opencode-sse-test.el --- Tests for SSE handling  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-opencode-sse)

;;; extract-event-type

(ert-deftest test-opencode-sse/extract-event-type-simple ()
  "Extract type from a well-formed JSON string."
  (should (equal (opencode-sse--extract-event-type
                  "{\"type\":\"session.created\",\"data\":{}}")
                 "session.created")))

(ert-deftest test-opencode-sse/extract-event-type-missing ()
  "Return nil when type field is absent."
  (should (null (opencode-sse--extract-event-type "{\"data\":{}}"))))

(ert-deftest test-opencode-sse/extract-event-type-not-first ()
  "Return nil when type is not the first key."
  (should (null (opencode-sse--extract-event-type
                 "{\"data\":{},\"type\":\"foo\"}"))))

(ert-deftest test-opencode-sse/extract-event-type-empty-string ()
  "Return nil for empty input."
  (should (null (opencode-sse--extract-event-type ""))))

(ert-deftest test-opencode-sse/extract-event-type-after-envelope-fields ()
  "Extract the type that follows the event ID and timestamp."
  (should (equal (opencode-sse--extract-event-type
                  "{\"id\":\"evt_1\",\"created\":1,\"type\":\"session.text.delta\",\"data\":{\"type\":\"x\"}}")
                 "session.text.delta")))

;;; strip-data-prefix

(ert-deftest test-opencode-sse/strip-data-prefix ()
  "Strip \"data: \" prefix from a string."
  (should (equal (opencode-sse--strip-data-prefix
                  "data: {\"type\":\"foo\"}")
                 "{\"type\":\"foo\"}"))
  ;; No prefix — return unchanged.
  (should (equal (opencode-sse--strip-data-prefix "{\"type\":\"foo\"}")
                 "{\"type\":\"foo\"}")))

;;; build-url

(ert-deftest test-opencode-sse/build-url ()
  "Build the /api/event URL from a connection."
  (let ((conn (opencode-connection-create
               :base-url "http://localhost:4096")))
    (should (equal (opencode-sse--build-url conn)
                   "http://localhost:4096/api/event"))))

(ert-deftest test-opencode-sse/build-url-trailing-slash ()
  "Strip trailing slash from base URL before appending /api/event."
  (let ((conn (opencode-connection-create
               :base-url "http://localhost:4096/")))
    (should (equal (opencode-sse--build-url conn)
                   "http://localhost:4096/api/event"))))

;;; auth-header

(ert-deftest test-opencode-sse/auth-header-with-credentials ()
  "Build Basic auth header when password is set."
  (let ((conn (opencode-connection-create
               :username "user"
               :password "pass")))
    (should (string-prefix-p "Authorization: Basic "
                             (opencode-sse--auth-header conn)))))

(ert-deftest test-opencode-sse/auth-header-no-password ()
  "Return nil when no password is set."
  (let ((conn (opencode-connection-create)))
    (should (null (opencode-sse--auth-header conn)))))

(ert-deftest test-opencode-sse/auth-header-default-username ()
  "Default username to \"opencode\" when only password is set."
  (let* ((conn (opencode-connection-create :password "secret"))
         (header (opencode-sse--auth-header conn)))
    (should (stringp header))
    ;; Decode to verify default username
    (string-match "Basic \\(.+\\)" header)
    (let ((decoded (base64-decode-string (match-string 1 header))))
      (should (string-prefix-p "opencode:" decoded)))))

;;; SSE state helpers

(ert-deftest test-opencode-sse/state-read-write ()
  "Read and write SSE parse state."
  (let ((conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (should (null (opencode-sse--read-state conn :fragments)))
    (opencode-sse--write-state conn :fragments '("partial"))
    (should (equal (opencode-sse--read-state conn :fragments) '("partial")))))

(ert-deftest test-opencode-sse/clear-event ()
  "Clear event resets fragments, trailing-nl, and dropping."
  (let ((conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse--write-state conn :fragments '("some data"))
    (opencode-sse--write-state conn :trailing-nl t)
    (opencode-sse--write-state conn :dropping t)
    (opencode-sse--clear-event conn)
    (should (null (opencode-sse--read-state conn :fragments)))
    (should (null (opencode-sse--read-state conn :trailing-nl)))
    (should (null (opencode-sse--read-state conn :dropping)))))

;;; find-boundary

(ert-deftest test-opencode-sse/find-boundary-within-chunk ()
  "Find boundary within a single chunk."
  (let ((conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    ;; Returns (PRE-END . REST-START) cons.
    (should (equal (opencode-sse--find-boundary conn "data: {}\n\n") '(8 . 10)))
    (should (null (opencode-sse--find-boundary conn "data: {}\n")))))

(ert-deftest test-opencode-sse/find-boundary-split-across-chunks ()
  "Detect boundary split across two chunks via trailing-nl flag."
  (let ((conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    ;; Previous chunk ended with \n.
    (opencode-sse--write-state conn :trailing-nl t)
    ;; This chunk starts with \n — split boundary: (0 . 1).
    (should (equal (opencode-sse--find-boundary conn "\nrest") '(0 . 1)))
    ;; No trailing-nl — no boundary even though chunk starts with \n.
    (opencode-sse--write-state conn :trailing-nl nil)
    (should (null (opencode-sse--find-boundary conn "\nrest")))))

(ert-deftest test-opencode-sse/find-boundary-empty-chunk ()
  "Empty chunk returns nil."
  (let ((conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (should (null (opencode-sse--find-boundary conn "")))))

;;; process-chunk — complete events

(ert-deftest test-opencode-sse/process-chunk-complete-event ()
  "A complete SSE event dispatches to the handler."
  (let ((opencode-sse--handlers nil)
        (dispatched nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler
     "session.created"
     (lambda (_event data _meta)
       (setq dispatched data)))
    (opencode-sse--process-chunk
     conn
     "data: {\"type\":\"session.created\",\"value\":42}\n\n")
    (should dispatched)
    (should (equal (alist-get 'value dispatched) 42))))

(ert-deftest test-opencode-sse/process-chunk-split-across-chunks ()
  "An event split across two chunks is assembled correctly."
  (let ((opencode-sse--handlers nil)
        (dispatched nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler
     "msg.update"
     (lambda (_event data _meta)
       (setq dispatched data)))
    (opencode-sse--process-chunk conn "data: {\"type\":\"msg.up")
    (should (null dispatched))
    (opencode-sse--process-chunk conn "date\",\"x\":1}\n\n")
    (should dispatched)
    (should (equal (alist-get 'x dispatched) 1))))

(ert-deftest test-opencode-sse/process-chunk-boundary-split-across-chunks ()
  "Event boundary \\n\\n split across two chunks is detected."
  (let ((opencode-sse--handlers nil)
        (dispatched nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler
     "split.test"
     (lambda (_event data _meta)
       (setq dispatched data)))
    ;; First chunk: data line ending with \n (first \n of boundary).
    (opencode-sse--process-chunk
     conn "data: {\"type\":\"split.test\",\"ok\":true}\n")
    (should (null dispatched))
    ;; Second chunk starts with \n (second \n of boundary).
    (opencode-sse--process-chunk conn "\n")
    (should dispatched)
    (should (equal (alist-get 'ok dispatched) t))))

(ert-deftest test-opencode-sse/process-chunk-two-events-one-chunk ()
  "Two complete events in a single chunk both dispatch."
  (let ((opencode-sse--handlers nil)
        (results nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler
     "ev.one"
     (lambda (_event data _meta) (push (cons 'one data) results)))
    (opencode-sse-register-handler
     "ev.two"
     (lambda (_event data _meta) (push (cons 'two data) results)))
    (opencode-sse--process-chunk
     conn
     (concat "data: {\"type\":\"ev.one\",\"n\":1}\n\n"
             "data: {\"type\":\"ev.two\",\"n\":2}\n\n"))
    (should (= (length results) 2))
    ;; Results are pushed, so newest first.
    (should (equal (alist-get 'n (cdr (nth 0 results))) 2))
    (should (equal (alist-get 'n (cdr (nth 1 results))) 1))))

;;; process-chunk — unhandled events

(ert-deftest test-opencode-sse/unhandled-event-not-dispatched ()
  "Events without registered handlers are not dispatched."
  (let ((opencode-sse--handlers nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    ;; No handler registered for "unknown.event"
    (opencode-sse--process-chunk
     conn
     "data: {\"type\":\"unknown.event\",\"data\":\"large payload\"}\n\n")
    ;; State should be clean after the event.
    (should (null (opencode-sse--read-state conn :fragments)))))

(ert-deftest test-opencode-sse/unhandled-event-multi-chunk ()
  "An unhandled event split across chunks drops without accumulating."
  (let ((opencode-sse--handlers nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    ;; No handler registered for "session.diff".
    (opencode-sse--process-chunk
     conn "data: {\"type\":\"session.diff\",\"patch\":[{\"op\":\"replace\"")
    ;; First chunk triggers drop mode — no fragments accumulated.
    (should (opencode-sse--read-state conn :dropping))
    (should (null (opencode-sse--read-state conn :fragments)))
    ;; More chunks are discarded.
    (opencode-sse--process-chunk conn ",\"path\":\"/files\",\"value\":\"")
    (opencode-sse--process-chunk conn "...more data...")
    (should (null (opencode-sse--read-state conn :fragments)))
    ;; Boundary ends it — state fully cleared.
    (opencode-sse--process-chunk conn "\"}\n\n")
    (should (null (opencode-sse--read-state conn :fragments)))
    (should (null (opencode-sse--read-state conn :dropping)))))

(ert-deftest test-opencode-sse/unhandled-event-then-handled ()
  "After dropping an unhandled event, handled events dispatch correctly."
  (let ((opencode-sse--handlers nil)
        (dispatched nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler
     "message.created"
     (lambda (_event data _meta)
       (setq dispatched data)))
    ;; Unhandled event with split boundary — enters drop mode.
    (opencode-sse--process-chunk
     conn "data: {\"type\":\"session.diff\",\"big\":true}\n")
    (should (opencode-sse--read-state conn :dropping))
    ;; Boundary for unhandled event + start of handled event.
    (opencode-sse--process-chunk
     conn "\ndata: {\"type\":\"message.created\",\"id\":7}\n\n")
    (should-not (opencode-sse--read-state conn :dropping))
    (should dispatched)
    (should (equal (alist-get 'id dispatched) 7))))

(ert-deftest test-opencode-sse/unhandled-event-large-payload ()
  "A large unhandled event with many chunks does not accumulate fragments."
  (let ((opencode-sse--handlers nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    ;; First chunk triggers drop mode.
    (opencode-sse--process-chunk conn "data: {\"type\":\"session.diff\",\"data\":\"")
    (should (opencode-sse--read-state conn :dropping))
    ;; Simulate many chunks — none are accumulated.
    (dotimes (_ 100)
      (opencode-sse--process-chunk conn (make-string 1024 ?x)))
    (should (null (opencode-sse--read-state conn :fragments)))
    ;; Boundary ends it.
    (opencode-sse--process-chunk conn "\"}\n\n")
    (should (null (opencode-sse--read-state conn :fragments)))
    (should (null (opencode-sse--read-state conn :dropping)))))

(ert-deftest test-opencode-sse/multiple-events-after-unhandled ()
  "After an unhandled event, multiple subsequent events dispatch correctly."
  (let ((opencode-sse--handlers nil)
        (results nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler
     "good.event"
     (lambda (_event data _meta) (push data results)))
    ;; Unhandled event, then two handled events in one chunk.
    (opencode-sse--process-chunk
     conn
     (concat "data: {\"type\":\"skip.me\"}\n\n"
             "data: {\"type\":\"good.event\",\"n\":1}\n\n"
             "data: {\"type\":\"good.event\",\"n\":2}\n\n"))
    (should (= (length results) 2))
    (should (equal (alist-get 'n (nth 0 results)) 2))
    (should (equal (alist-get 'n (nth 1 results)) 1))))

;;; process-chunk — fragment accumulation

(ert-deftest test-opencode-sse/chunk-accumulation-no-newline ()
  "Chunks without newlines accumulate as a reversed fragment list."
  (let ((opencode-sse--handlers nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler "msg.updated" (lambda (_e _d _m)))
    ;; Simulate a large data line arriving in multiple chunks.
    (opencode-sse--process-chunk conn "data: {\"type\":\"msg")
    (should (equal (opencode-sse--read-state conn :fragments)
                   '("data: {\"type\":\"msg")))
    (opencode-sse--process-chunk conn ".updated\",\"big\":\"")
    (should (equal (opencode-sse--read-state conn :fragments)
                   '(".updated\",\"big\":\"" "data: {\"type\":\"msg")))
    (opencode-sse--process-chunk conn "payload\"}")
    (should (equal (opencode-sse--read-state conn :fragments)
                   '("payload\"}" ".updated\",\"big\":\"" "data: {\"type\":\"msg")))))

(ert-deftest test-opencode-sse/chunk-accumulation-then-boundary ()
  "Accumulated chunks are joined and dispatched when boundary arrives."
  (let ((opencode-sse--handlers nil)
        (dispatched nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler
     "msg.updated"
     (lambda (_event data _meta) (setq dispatched data)))
    ;; Three chunks without newlines.
    (opencode-sse--process-chunk conn "data: {\"type\":\"msg")
    (opencode-sse--process-chunk conn ".updated\",\"x\":")
    (opencode-sse--process-chunk conn "99}")
    (should (null dispatched))
    ;; Boundary to finalize the event.
    (opencode-sse--process-chunk conn "\n\n")
    (should dispatched)
    (should (equal (alist-get 'x dispatched) 99))))

(ert-deftest test-opencode-sse/chunk-accumulation-many-chunks ()
  "A large number of small chunks accumulate correctly."
  (let ((opencode-sse--handlers nil)
        (dispatched nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler
     "big.event"
     (lambda (_event data _meta) (setq dispatched data)))
    (opencode-sse--process-chunk conn "data: {\"type\":\"big.event\",\"v\":\"")
    (dotimes (_ 100)
      (opencode-sse--process-chunk conn "x"))
    (opencode-sse--process-chunk conn "\"}\n\n")
    (should dispatched)
    (should (= (length (alist-get 'v dispatched)) 100))))

(ert-deftest test-opencode-sse/chunk-boundary-in-middle-of-chunk ()
  "A chunk containing the boundary in the middle correctly splits."
  (let ((opencode-sse--handlers nil)
        (dispatched nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler
     "mid.test"
     (lambda (_event data _meta) (setq dispatched data)))
    ;; First chunk has no newline.
    (opencode-sse--process-chunk conn "data: {\"type\":\"mid.t")
    (should (equal (opencode-sse--read-state conn :fragments)
                   '("data: {\"type\":\"mid.t")))
    ;; Second chunk contains boundary mid-chunk, plus start of next data.
    (opencode-sse--process-chunk conn "est\",\"a\":1}\n\ndata: {\"type\":\"mid.t")
    ;; The first event should have dispatched.
    (should dispatched)
    (should (equal (alist-get 'a dispatched) 1))
    ;; The remainder after the boundary should start accumulating.
    (should (equal (opencode-sse--read-state conn :fragments)
                   '("data: {\"type\":\"mid.t")))))

(ert-deftest test-opencode-sse/chunk-boundary-exactly-at-end ()
  "State is clean when a chunk ends exactly at the boundary."
  (let ((opencode-sse--handlers nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler "exact.test" (lambda (_e _d _m)))
    (opencode-sse--process-chunk conn "data: {\"type\":\"exact.test\"}\n\n")
    (should (null (opencode-sse--read-state conn :fragments)))
    (should (null (opencode-sse--read-state conn :trailing-nl)))))

;;; trailing-nl flag

(ert-deftest test-opencode-sse/trailing-nl-flag-set-correctly ()
  "The trailing-nl flag tracks whether the last chunk ended with newline."
  (let ((opencode-sse--handlers nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler "trail.test" (lambda (_e _d _m)))
    ;; Chunk without trailing newline.
    (opencode-sse--process-chunk conn "data: {\"type\":\"trail.test\"")
    (should-not (opencode-sse--read-state conn :trailing-nl))
    ;; Chunk with trailing newline but no boundary.
    (opencode-sse--process-chunk conn ",\"x\":1}\n")
    (should (opencode-sse--read-state conn :trailing-nl))))

(ert-deftest test-opencode-sse/trailing-nl-not-false-positive ()
  "A chunk that does not end with \\n has trailing-nl unset."
  (let ((opencode-sse--handlers nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler "fp.test" (lambda (_e _d _m)))
    (opencode-sse--process-chunk conn "data: {\"type\":\"fp.test\",\"x\":1}")
    (should-not (opencode-sse--read-state conn :trailing-nl))))

;;; v2 framing

(ert-deftest test-opencode-sse/heartbeat-comment-ignored ()
  "Heartbeat comment frames are skipped without error."
  (let ((opencode-sse--handlers nil)
        (conn (opencode-connection-create))
        (calls 0))
    (opencode-sse-register-handler "hb.test" (lambda (&rest _) (cl-incf calls)))
    (opencode-sse--process-chunk conn ": heartbeat\n\n")
    (opencode-sse--process-chunk conn "data: {\"id\":\"e\",\"type\":\"hb.test\",\"data\":{}}\n\n")
    (should (= calls 1))))

(ert-deftest test-opencode-sse/type-after-metadata-object ()
  "Events whose metadata object precedes the type are still dispatched."
  (let ((opencode-sse--handlers nil)
        (conn (opencode-connection-create))
        received)
    (opencode-sse-register-handler "meta.first" (lambda (_e data _m) (setq received data)))
    (opencode-sse--process-chunk
     conn "data: {\"id\":\"e\",\"metadata\":{\"a\":1},\"type\":\"meta.first\",\"data\":{\"x\":2}}\n\n")
    (should (equal (alist-get 'x (alist-get 'data received)) 2))))

;;; reconnection

(ert-deftest test-opencode-sse/event-meta-flags-reconnect ()
  "Only a second server.connected on a connection is a reconnect."
  (let ((conn (opencode-connection-create)))
    (should-not (plist-get (opencode-sse--event-meta conn "server.connected") :reconnect))
    (should (opencode-connection-sse-connected conn))
    (should (plist-get (opencode-sse--event-meta conn "server.connected") :reconnect))
    (should-not (plist-get (opencode-sse--event-meta conn "session.status") :reconnect))
    (should (eq (plist-get (opencode-sse--event-meta conn "x") :connection) conn))))

(ert-deftest test-opencode-sse/backoff-resets-after-stable-stream ()
  "Backoff restarts only after a stream stayed connected for a while."
  (let* ((proc (start-process "oc-sse-test" nil "true"))
         (conn (opencode-connection-create :sse-process proc :sse-retry-count 4)))
    (unwind-protect
        (cl-letf (((symbol-function 'opencode-connection-alive-p) (lambda (_) t))
                  ((symbol-function 'run-at-time) (lambda (&rest _) 'timer)))
          (opencode-sse--event-meta conn "server.connected")
          (opencode-sse--handle-exit conn proc)
          (should (= (opencode-connection-sse-retry-count conn) 5))
          (process-put proc 'opencode-connected-at (- (float-time) 3600))
          (opencode-sse--handle-exit conn proc)
          (should (= (opencode-connection-sse-retry-count conn) 1)))
      (delete-process proc))))

(ert-deftest test-opencode-sse/failing-handler-does-not-block-others ()
  "An error in one handler still lets later handlers and events run."
  (let ((opencode-sse--handlers nil)
        (seen nil))
    (opencode-sse-register-handler "x" (lambda (&rest _) (push 'ok seen)))
    (opencode-sse-register-handler "x" (lambda (&rest _) (error "Boom")))
    (cl-letf (((symbol-function 'message) #'ignore))
      (opencode-sse--dispatch "x" nil))
    (should (equal seen '(ok)))))

(ert-deftest test-opencode-sse/reconnect-delay-backs-off ()
  "Reconnect delays double up to the maximum."
  (should (= (opencode-sse--reconnect-delay 1) 1))
  (should (= (opencode-sse--reconnect-delay 3) 4))
  (should (= (opencode-sse--reconnect-delay 20) opencode-sse-reconnect-max-delay)))

(ert-deftest test-opencode-sse/handle-exit-schedules-reconnect ()
  "An unexpected stream exit schedules a reconnect while the server lives."
  (let* ((proc (start-process "oc-sse-test" nil "true"))
         (conn (opencode-connection-create :sse-process proc))
         scheduled)
    (unwind-protect
        (cl-letf (((symbol-function 'opencode-connection-alive-p) (lambda (_) t))
                  ((symbol-function 'run-at-time)
                   (lambda (delay _repeat fn &rest args)
                     (setq scheduled (list delay fn args))
                     'timer)))
          (opencode-sse--handle-exit conn proc))
      (delete-process proc))
    (should (equal scheduled (list 1 #'opencode-sse--reconnect (list conn proc))))
    (should (= (opencode-connection-sse-retry-count conn) 1))))

(ert-deftest test-opencode-sse/handle-exit-ignores-deliberate-close ()
  "Closing a stream on purpose does not reconnect."
  (let ((conn (opencode-connection-create :sse-process nil))
        scheduled)
    (cl-letf (((symbol-function 'opencode-connection-alive-p) (lambda (_) t))
              ((symbol-function 'run-at-time) (lambda (&rest _) (setq scheduled t))))
      (opencode-sse--handle-exit conn 'proc))
    (should-not scheduled)))

(ert-deftest test-opencode-sse/handle-exit-ignores-dead-server ()
  "No reconnect once the server process is gone."
  (let ((conn (opencode-connection-create :sse-process 'proc))
        scheduled)
    (cl-letf (((symbol-function 'opencode-connection-alive-p) (lambda (_) nil))
              ((symbol-function 'run-at-time) (lambda (&rest _) (setq scheduled t))))
      (opencode-sse--handle-exit conn 'proc))
    (should-not scheduled)))

;;; handler registration

(ert-deftest test-opencode-sse/register-and-dispatch ()
  "Register a handler and dispatch to it."
  (let ((opencode-sse--handlers nil)
        (result nil))
    (opencode-sse-register-handler
     "test.event"
     (lambda (_event data _meta) (setq result data)))
    (opencode-sse--dispatch "test.event" '((key . "value")))
    (should (equal (alist-get 'key result) "value"))))

(ert-deftest test-opencode-sse/dispatch-passes-meta ()
  "Dispatch forwards the META plist (with :connection) to handlers."
  (let ((opencode-sse--handlers nil)
        (received-meta :unset))
    (opencode-sse-register-handler
     "meta.event"
     (lambda (_event _data meta) (setq received-meta meta)))
    (opencode-sse--dispatch "meta.event" nil '(:connection conn-a))
    (should (eq (plist-get received-meta :connection) 'conn-a))))

(ert-deftest test-opencode-sse/dispatch-meta-defaults-nil ()
  "Dispatch passes nil META when none is provided."
  (let ((opencode-sse--handlers nil)
        (received-meta :unset))
    (opencode-sse-register-handler
     "meta.default"
     (lambda (_event _data meta) (setq received-meta meta)))
    (opencode-sse--dispatch "meta.default" nil)
    (should (null received-meta))))

(ert-deftest test-opencode-sse/unregister-handlers ()
  "Unregister removes all handlers for an event."
  (let ((opencode-sse--handlers nil)
        (called nil))
    (opencode-sse-register-handler
     "remove.me"
     (lambda (_event _data _meta) (setq called t)))
    (opencode-sse-unregister-handlers "remove.me")
    (opencode-sse--dispatch "remove.me" nil)
    (should (null called))))

(ert-deftest test-opencode-sse/duplicate-handler-not-added ()
  "Registering the same handler twice does not duplicate it."
  (let ((opencode-sse--handlers nil)
        (count 0))
    (let ((handler (lambda (_event _data _meta) (setq count (1+ count)))))
      (opencode-sse-register-handler "dup.test" handler)
      (opencode-sse-register-handler "dup.test" handler)
      (opencode-sse--dispatch "dup.test" nil)
      (should (= count 1)))))

;;; Edge cases

(ert-deftest test-opencode-sse/event-with-no-handler-not-parsed ()
  "Events with no handler are not JSON-parsed at finalize."
  (let ((opencode-sse--handlers nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler "other.event" (lambda (_e _d _m)))
    ;; Feed an event whose type is "no.handler" — no handler registered.
    (opencode-sse--process-chunk
     conn
     "data: {\"type\":\"no.handler\",\"x\":1}\n\n")
    ;; State is clean and no error was thrown.
    (should (null (opencode-sse--read-state conn :fragments)))))

(ert-deftest test-opencode-sse/boundary-no-trailing-nl ()
  "Boundary in chunk when prev chunk had no trailing newline."
  (let ((opencode-sse--handlers nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler "bn.test" (lambda (_e _d _m)))
    ;; Chunk does not end with \n.
    (opencode-sse--process-chunk conn "data: {\"type\":\"bn.test\",\"x\":1}")
    (should-not (opencode-sse--read-state conn :trailing-nl))
    ;; Next chunk has the full boundary.
    (opencode-sse--process-chunk conn "\n\n")
    (should (null (opencode-sse--read-state conn :fragments)))))

(ert-deftest test-opencode-sse/handled-event-multi-chunk-no-newlines ()
  "A handled event arriving without newlines accumulates and dispatches."
  (let ((opencode-sse--handlers nil)
        (dispatched nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler
     "msg.updated"
     (lambda (_event data _meta) (setq dispatched data)))
    (opencode-sse--process-chunk conn "data: {\"type\":\"msg.updated\",\"x\":")
    (should (opencode-sse--read-state conn :fragments))
    (opencode-sse--process-chunk conn "42}")
    (opencode-sse--process-chunk conn "\n\n")
    (should dispatched)
    (should (= (alist-get 'x dispatched) 42))))

(ert-deftest test-opencode-sse/process-chunk-passes-connection-in-meta ()
  "The curl path threads the originating connection into handler META."
  (let ((opencode-sse--handlers nil)
        (received-conn :unset)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-state conn)
    (opencode-sse-register-handler
     "conn.test"
     (lambda (_event _data meta) (setq received-conn (plist-get meta :connection))))
    (opencode-sse--process-chunk conn "data: {\"type\":\"conn.test\",\"x\":1}\n\n")
    (should (eq received-conn conn))))

;;; Bridge backend

(ert-deftest test-opencode-sse/bridge-parse-line-valid ()
  "Parse a valid JSON line from the bridge."
  (let ((result (opencode-sse--bridge-parse-line
                 "{\"type\":\"session.created\",\"properties\":{\"sessionID\":\"abc\"}}")))
    (should result)
    (should (equal (alist-get 'type result) "session.created"))
    (should (equal (alist-get 'sessionID (alist-get 'properties result)) "abc"))))

(ert-deftest test-opencode-sse/bridge-parse-line-invalid ()
  "Return nil for invalid JSON."
  (should (null (opencode-sse--bridge-parse-line "not json")))
  (should (null (opencode-sse--bridge-parse-line ""))))

(ert-deftest test-opencode-sse/bridge-parse-line-types ()
  "Parse JSON with null, false, arrays, and nested objects."
  (let ((result (opencode-sse--bridge-parse-line
                 "{\"a\":null,\"b\":false,\"c\":[1,2],\"d\":{\"e\":\"f\"}}")))
    (should result)
    (should (null (alist-get 'a result)))
    (should (null (alist-get 'b result)))
    (should (equal (alist-get 'c result) '(1 2)))
    (should (equal (alist-get 'e (alist-get 'd result)) "f"))))

(ert-deftest test-opencode-sse/bridge-initialize-state ()
  "Bridge state initializes with an empty line buffer."
  (let ((conn (opencode-connection-create)))
    (opencode-sse--initialize-bridge-state conn)
    (should (equal (plist-get (opencode-connection-sse-state conn) :line-buffer) ""))))

(ert-deftest test-opencode-sse/bridge-process-complete-line ()
  "A complete JSON line dispatches to the handler."
  (let ((opencode-sse--handlers nil)
        (dispatched nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-bridge-state conn)
    (opencode-sse-register-handler
     "session.created"
     (lambda (_event data _meta) (setq dispatched data)))
    (opencode-sse--bridge-process-output
     conn
     "{\"type\":\"session.created\",\"properties\":{\"id\":1}}\n")
    (should dispatched)
    (should (equal (alist-get 'type dispatched) "session.created"))
    ;; Line buffer should be empty after processing.
    (should (equal (plist-get (opencode-connection-sse-state conn) :line-buffer) ""))))

(ert-deftest test-opencode-sse/bridge-process-passes-connection-in-meta ()
  "The bridge path threads the originating connection into handler META."
  (let ((opencode-sse--handlers nil)
        (received-conn :unset)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-bridge-state conn)
    (opencode-sse-register-handler
     "conn.bridge"
     (lambda (_event _data meta) (setq received-conn (plist-get meta :connection))))
    (opencode-sse--bridge-process-output
     conn
     "{\"type\":\"conn.bridge\",\"properties\":{\"id\":1}}\n")
    (should (eq received-conn conn))))

(ert-deftest test-opencode-sse/bridge-process-split-across-calls ()
  "Data split across two process-output calls is assembled correctly."
  (let ((opencode-sse--handlers nil)
        (dispatched nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-bridge-state conn)
    (opencode-sse-register-handler
     "msg.update"
     (lambda (_event data _meta) (setq dispatched data)))
    ;; First call: incomplete line (no newline).
    (opencode-sse--bridge-process-output
     conn "{\"type\":\"msg.up")
    (should (null dispatched))
    (should (equal (plist-get (opencode-connection-sse-state conn) :line-buffer)
                   "{\"type\":\"msg.up"))
    ;; Second call: rest of line with newline.
    (opencode-sse--bridge-process-output
     conn "date\",\"x\":1}\n")
    (should dispatched)
    (should (equal (alist-get 'x dispatched) 1))
    (should (equal (plist-get (opencode-connection-sse-state conn) :line-buffer) ""))))

(ert-deftest test-opencode-sse/bridge-process-multiple-lines ()
  "Multiple complete lines in one output are all dispatched."
  (let ((opencode-sse--handlers nil)
        (results nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-bridge-state conn)
    (opencode-sse-register-handler
     "ev.one"
     (lambda (_event data _meta) (push (cons 'one data) results)))
    (opencode-sse-register-handler
     "ev.two"
     (lambda (_event data _meta) (push (cons 'two data) results)))
    (opencode-sse--bridge-process-output
     conn
     (concat "{\"type\":\"ev.one\",\"n\":1}\n"
             "{\"type\":\"ev.two\",\"n\":2}\n"))
    (should (= (length results) 2))
    ;; Results are pushed, newest first.
    (should (equal (alist-get 'n (cdr (nth 0 results))) 2))
    (should (equal (alist-get 'n (cdr (nth 1 results))) 1))))

(ert-deftest test-opencode-sse/bridge-process-trailing-incomplete ()
  "A trailing incomplete line is buffered for the next call."
  (let ((opencode-sse--handlers nil)
        (dispatched nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-bridge-state conn)
    (opencode-sse-register-handler
     "ev.one"
     (lambda (_event data _meta) (setq dispatched data)))
    ;; Complete line followed by incomplete line.
    (opencode-sse--bridge-process-output
     conn
     "{\"type\":\"ev.one\",\"n\":1}\n{\"type\":\"ev.on")
    (should dispatched)
    (should (equal (alist-get 'n dispatched) 1))
    (should (equal (plist-get (opencode-connection-sse-state conn) :line-buffer)
                   "{\"type\":\"ev.on"))))

(ert-deftest test-opencode-sse/bridge-process-empty-lines-ignored ()
  "Empty lines (from consecutive newlines) are silently ignored."
  (let ((opencode-sse--handlers nil)
        (dispatched nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-bridge-state conn)
    (opencode-sse-register-handler
     "test.event"
     (lambda (_event data _meta) (setq dispatched data)))
    (opencode-sse--bridge-process-output
     conn
     "\n\n{\"type\":\"test.event\",\"x\":1}\n\n")
    (should dispatched)
    (should (equal (alist-get 'x dispatched) 1))))

(ert-deftest test-opencode-sse/bridge-process-invalid-json-skipped ()
  "Invalid JSON lines are skipped without error."
  (let ((opencode-sse--handlers nil)
        (dispatched nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-bridge-state conn)
    (opencode-sse-register-handler
     "good.event"
     (lambda (_event data _meta) (setq dispatched data)))
    (opencode-sse--bridge-process-output
     conn
     "not valid json\n{\"type\":\"good.event\",\"x\":42}\n")
    (should dispatched)
    (should (equal (alist-get 'x dispatched) 42))))

(ert-deftest test-opencode-sse/bridge-process-no-type-skipped ()
  "JSON lines without a type field are silently skipped."
  (let ((opencode-sse--handlers nil)
        (dispatched nil)
        (conn (opencode-connection-create)))
    (opencode-sse--initialize-bridge-state conn)
    (opencode-sse-register-handler
     "real.event"
     (lambda (_event data _meta) (setq dispatched data)))
    (opencode-sse--bridge-process-output
     conn
     "{\"no_type\":\"here\"}\n{\"type\":\"real.event\",\"v\":1}\n")
    (should dispatched)
    (should (equal (alist-get 'v dispatched) 1))))

(ert-deftest test-opencode-sse/bridge-events-arg ()
  "Build comma-separated event type string from handler registry."
  (let ((opencode-sse--handlers nil))
    (opencode-sse-register-handler "a.event" #'ignore)
    (opencode-sse-register-handler "b.event" #'ignore)
    (let ((arg (opencode-sse--bridge-events-arg)))
      ;; Both event names should be present.
      (should (string-match-p "a\\.event" arg))
      (should (string-match-p "b\\.event" arg))
      ;; Comma-separated.
      (should (string-match-p "," arg)))))

(ert-deftest test-opencode-sse/bridge-auth-token ()
  "Build Base64 auth token from connection credentials."
  (let ((conn (opencode-connection-create :username "user" :password "pass")))
    (should (stringp (opencode-sse--bridge-auth-token conn))))
  ;; No password — nil.
  (let ((conn (opencode-connection-create)))
    (should (null (opencode-sse--bridge-auth-token conn)))))

(ert-deftest test-opencode-sse/bridge-auth-token-default-user ()
  "Default username to \"opencode\" when only password is set."
  (let* ((conn (opencode-connection-create :password "secret"))
         (token (opencode-sse--bridge-auth-token conn)))
    (should (stringp token))
    (let ((decoded (base64-decode-string token)))
      (should (string-prefix-p "opencode:" decoded)))))

(ert-deftest test-opencode-sse/resolve-backend-auto-with-runtime ()
  "Auto backend resolves to bridge when a JS runtime is available."
  (let ((opencode-sse-backend 'auto))
    (cl-letf (((symbol-function 'opencode-sse--find-js-runtime)
               (lambda () "/usr/bin/node")))
      (should (eq (opencode-sse--resolve-backend) 'bridge)))))

(ert-deftest test-opencode-sse/resolve-backend-auto-no-runtime ()
  "Auto backend falls back to curl when no JS runtime is available."
  (let ((opencode-sse-backend 'auto))
    (cl-letf (((symbol-function 'opencode-sse--find-js-runtime)
               (lambda () nil)))
      (should (eq (opencode-sse--resolve-backend) 'curl)))))

(ert-deftest test-opencode-sse/resolve-backend-forced-curl ()
  "Forced curl backend always returns curl."
  (let ((opencode-sse-backend 'curl))
    (should (eq (opencode-sse--resolve-backend) 'curl))))

(ert-deftest test-opencode-sse/resolve-backend-forced-bridge-no-runtime ()
  "Forced bridge backend errors when no runtime is available."
  (let ((opencode-sse-backend 'bridge))
    (cl-letf (((symbol-function 'opencode-sse--find-js-runtime)
               (lambda () nil)))
      (should-error (opencode-sse--resolve-backend)))))

(ert-deftest test-opencode-sse/bridge-build-command ()
  "Build the bridge command with correct arguments."
  (let ((opencode-sse--handlers nil)
        (conn (opencode-connection-create
               :base-url "http://localhost:4096"
               :password "secret")))
    (opencode-sse-register-handler "test.event" #'ignore)
    (cl-letf (((symbol-function 'opencode-sse--find-js-runtime)
               (lambda () "/usr/bin/node")))
      (let ((cmd (opencode-sse--build-bridge-command conn)))
        ;; Runtime.
        (should (equal (nth 0 cmd) "/usr/bin/node"))
        ;; Script path.
        (should (string-suffix-p "opencode-sse-bridge.js" (nth 1 cmd)))
        ;; URL arg.
        (should (member "--url" cmd))
        (should (equal (nth (1+ (cl-position "--url" cmd :test #'equal)) cmd)
                       "http://localhost:4096/api/event"))
        ;; Events arg.
        (should (member "--events" cmd))
        ;; Auth arg.
        (should (member "--auth" cmd))))))

(ert-deftest test-opencode-sse/bridge-build-command-no-auth ()
  "Bridge command omits --auth when no password is set."
  (let ((opencode-sse--handlers nil)
        (conn (opencode-connection-create :base-url "http://localhost:4096")))
    (opencode-sse-register-handler "test.event" #'ignore)
    (cl-letf (((symbol-function 'opencode-sse--find-js-runtime)
               (lambda () "/usr/bin/bun")))
      (let ((cmd (opencode-sse--build-bridge-command conn)))
        (should-not (member "--auth" cmd))))))

(provide 'emacs-opencode-sse-test)

;;; emacs-opencode-sse-test.el ends here
