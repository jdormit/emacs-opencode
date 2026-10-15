;;; emacs-opencode-service-integration.el --- Isolated service check -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-opencode)

(defconst opencode-integration--plugin
  (expand-file-name "fixtures/codemode" (file-name-directory (or load-file-name buffer-file-name)))
  "Code Mode test plugin directory.")

(defun opencode-integration--wait (predicate)
  "Wait up to 60 seconds for PREDICATE."
  (let ((deadline (+ (float-time) 60)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should (funcall predicate))))

(ert-deftest test-opencode-service-integration/shared-service ()
  "Render mock replies in two directories, then recover from service restart."
  :tags '(:integration)
  (should (equal (getenv "XDG_STATE_HOME") "/tmp/oc2-emacs/state"))
  (let ((opencode--service-connection nil)
        (opencode-session--buffers (make-hash-table :test 'equal))
        (opencode-session-default-agent "build")
        (first (make-temp-file "oc2-emacs-first-" t))
        (second (make-temp-file "oc2-emacs-second-" t))
        connections buffers)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer)))
          (dolist (directory (list first second))
            (let (ready)
              (opencode-run-server directory (lambda (conn) (push conn connections) (setq ready t)))
              (opencode-integration--wait (lambda () ready))))
          (should (eq (car connections) (cadr connections)))
          (let ((conn (car connections)))
            (opencode-integration--wait (lambda () (opencode-connection-sse-connected conn)))
            (let ((stream (opencode-connection-sse-process conn)))
              (dolist (directory (list first second))
                (let (buffer)
                  (opencode-run-server
                   directory
                   (lambda (connection)
                     (opencode--create-session
                      connection
                      (lambda (session)
                        (setq buffer (opencode-session-open session connection))
                        (push buffer buffers)))))
                  (opencode-integration--wait (lambda () buffer))
                  (with-current-buffer buffer
                    (should (equal (file-truename default-directory)
                                   (file-name-as-directory (file-truename directory))))
                    (opencode-session-insert-input "shared service check")
                    (opencode-session-send-input))
                  (opencode-integration--wait
                   (lambda () (with-current-buffer buffer
                                (string-match-p "mock reply: shared service check" (buffer-string)))))))
              (should (eq stream (opencode-connection-sse-process conn))))
            (let* ((old-pid (alist-get 'pid (opencode-connection-registration conn)))
                   (restart (make-process :name "oc2-restart" :buffer " *oc2-restart*"
                                          :command (list opencode-server-command "service" "restart")
                                          :noquery t)))
              (opencode-integration--wait (lambda () (not (process-live-p restart))))
              (should (zerop (process-exit-status restart)))
              (opencode-integration--wait
               (lambda () (and (not (equal old-pid (alist-get 'pid (opencode-connection-registration conn))))
                               (process-live-p (opencode-connection-sse-process conn)))))
              (with-current-buffer (car buffers)
                (opencode-session-insert-input "after restart")
                (opencode-session-send-input))
              (opencode-integration--wait
               (lambda () (with-current-buffer (car buffers)
                            (string-match-p "mock reply: after restart" (buffer-string))))))))
      (opencode-disconnect)
      (dolist (buffer buffers) (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory first t)
      (delete-directory second t))))

(ert-deftest test-opencode-service-integration/codemode ()
  "Show real nested execute calls live and after reloading history."
  :tags '(:integration)
  (should (equal (getenv "XDG_STATE_HOME") "/tmp/oc2-emacs/state"))
  (let ((opencode--service-connection nil)
        (opencode-session--buffers (make-hash-table :test 'equal))
        (opencode-session-default-agent "build")
        (opencode-sse--handlers (copy-tree opencode-sse--handlers))
        (directory (make-temp-file "oc2-emacs-codemode-" t))
        buffer running)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buf &rest _) buf)))
          (with-temp-file (expand-file-name "opencode.json" directory)
            (insert (json-encode `((plugins . [,opencode-integration--plugin])))))
          (opencode-sse-register-handler
           "session.tool.progress"
           (lambda (_event data _meta)
             (dolist (call (alist-get 'toolCalls (alist-get 'metadata (alist-get 'data data))))
               (when (equal (alist-get 'status call) "running") (setq running t)))))
          (opencode-run-server
           directory
           (lambda (connection)
             (opencode--create-session
              connection
              (lambda (session)
                (setq buffer (opencode-session-open session connection))))))
          (opencode-integration--wait (lambda () buffer))
          (with-current-buffer buffer
            (opencode-session-insert-input
             (concat "CALL_TOOL execute "
                     (json-encode
                      '((code . "return await Promise.allSettled([tools.emacs_probe({value: 'nested argument'}), tools.emacs_probe({value: 'fail'})])")))))
            (opencode-session-send-input))
          (opencode-integration--wait
           (lambda () (with-current-buffer buffer
                        (and (string-match-p "emacs_probe \\[completed\\]" (buffer-string))
                             (string-match-p "emacs_probe \\[error\\]" (buffer-string))))))
          (should running)
          (with-current-buffer buffer
            (should (string-match-p (regexp-quote "{\"value\":\"nested argument\"}") (buffer-string)))
            (let* ((message (cl-find-if
                             (lambda (msg) (cl-some (lambda (entry)
                                                     (equal (opencode-message-part-tool (cdr entry)) "execute"))
                                                   (opencode-message-parts msg)))
                             opencode-session--messages))
                   (part (cl-find "execute" (mapcar #'cdr (opencode-message-parts message))
                                  :key #'opencode-message-part-tool :test #'equal)))
              (opencode-session--set-tool-drawer-open (opencode-message-part-id part) t)
              (opencode-session--fetch-tool-output message (opencode-message-part-id part))))
          (opencode-integration--wait
           (lambda () (with-current-buffer buffer
                        (string-match-p "probe result: nested argument" (buffer-string)))))
          (let (loaded)
            (with-current-buffer buffer
              (opencode-session--load-history opencode-session--connection opencode-session--session
                                              buffer (lambda (_) (setq loaded t))))
            (opencode-integration--wait (lambda () loaded)))
          (with-current-buffer buffer
            (should (string-match-p "emacs_probe \\[completed\\]" (buffer-string)))
            (should (string-match-p "emacs_probe \\[error\\]" (buffer-string)))
            (should (string-match-p "probe result: nested argument" (buffer-string)))))
      (opencode-disconnect)
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory directory t))))

(provide 'emacs-opencode-service-integration)
;;; emacs-opencode-service-integration.el ends here
