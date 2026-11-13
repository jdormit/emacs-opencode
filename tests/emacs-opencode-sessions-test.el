;;; emacs-opencode-sessions-test.el --- Tests for the session overview  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-opencode)
(require 'emacs-opencode-sessions)

(defun opencode-sessions-test--info (id &rest props)
  "Return session info for ID.
PROPS accepts :project, :directory, :title, :cost, :updated, :idle,
:viewed, :outcome, :parent and :archived."
  (let ((time `((created . 1000)
                (updated . ,(or (plist-get props :updated) 2000))
                ,@(when (plist-get props :idle) `((idle . ,(plist-get props :idle))))
                ,@(when (plist-get props :viewed) `((viewed . ,(plist-get props :viewed))))
                ,@(when (plist-get props :archived) `((archived . ,(plist-get props :archived)))))))
    `((id . ,id)
      (projectID . ,(or (plist-get props :project) "p1"))
      (title . ,(or (plist-get props :title) (concat "Session " id)))
      (location . ((directory . ,(or (plist-get props :directory) "/tmp/p1"))))
      (cost . ,(or (plist-get props :cost) 0))
      (time . ,time)
      ,@(when (plist-get props :outcome) `((outcome . ,(plist-get props :outcome))))
      ,@(when (plist-get props :parent) `((parentID . ,(plist-get props :parent)))))))

(defmacro opencode-sessions-test--with-tracker (infos &rest body)
  "Run BODY with a fresh tracker listing the session INFOS.
Re-rendering is stubbed out."
  (declare (indent 1))
  `(let ((opencode-sessions--tracker (opencode-sessions--tracker-create :connection 'conn)))
     (dolist (info ,infos)
       (opencode-sessions--add info))
     (cl-letf (((symbol-function 'opencode-sessions--changed) #'ignore))
       ,@body)))

(defun opencode-sessions-test--table (slot)
  "Return the tracker's hash table in SLOT."
  (cl-struct-slot-value 'opencode-sessions--tracker slot opencode-sessions--tracker))

(defun opencode-sessions-test--event (type payload)
  "Dispatch an event of TYPE carrying PAYLOAD to the overview."
  (opencode-sessions--handle-event type `((data . ,payload)) '(:connection conn)))

;;; Session state

(ert-deftest test-opencode-sessions/state-idle-when-read ()
  "A session whose latest idle transition was viewed has no state."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1" :idle 10 :viewed 10))
    (should-not (opencode-sessions--state "s1"))))

(ert-deftest test-opencode-sessions/state-done-when-unread ()
  "A session that went idle after it was last viewed is done."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1" :idle 20 :viewed 10)
            (opencode-sessions-test--info "s2" :idle 20))
    (should (eq (opencode-sessions--state "s1") 'done))
    (should (eq (opencode-sessions--state "s2") 'done))))

(ert-deftest test-opencode-sessions/state-reflects-outcome ()
  "Unread failed and interrupted runs are reported as such."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1" :idle 20 :outcome "failed")
            (opencode-sessions-test--info "s2" :idle 20 :outcome "interrupted")
            (opencode-sessions-test--info "s3" :idle 20 :viewed 30 :outcome "failed"))
    (should (eq (opencode-sessions--state "s1") 'failed))
    (should (eq (opencode-sessions--state "s2") 'stopped))
    (should-not (opencode-sessions--state "s3"))))

(ert-deftest test-opencode-sessions/state-running-overrides-unread ()
  "A running session is running even when its last run is unread."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1" :idle 20)
            (opencode-sessions-test--info "s2"))
    (puthash "s1" "busy" (opencode-sessions-test--table 'running))
    (puthash "s2" "retry" (opencode-sessions-test--table 'running))
    (should (eq (opencode-sessions--state "s1") 'running))
    (should (eq (opencode-sessions--state "s2") 'retrying))))

(ert-deftest test-opencode-sessions/state-counts-subagent-prompts ()
  "Prompts from a subagent session make its top-level session wait on you."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1") (opencode-sessions-test--info "s2"))
    (puthash "child" "s1" (opencode-sessions-test--table 'parents))
    (puthash "grandchild" "child" (opencode-sessions-test--table 'parents))
    (puthash "s1" "busy" (opencode-sessions-test--table 'running))
    (opencode-sessions--add-prompt "per_1" "grandchild" 'permission)
    (opencode-sessions--add-prompt "frm_1" "s2" 'question)
    (should (eq (opencode-sessions--state "s1") 'permission))
    (should (eq (opencode-sessions--state "s2") 'question))))

;;; Rows and groups

(ert-deftest test-opencode-sessions/groups-by-project-most-urgent-first ()
  "Sessions are grouped by project, and both groups and rows put urgent work first."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "a-old" :project "pa" :updated 100)
            (opencode-sessions-test--info "a-new" :project "pa" :updated 900)
            (opencode-sessions-test--info "b-idle" :project "pb" :updated 950)
            (opencode-sessions-test--info "b-ask" :project "pb" :updated 50))
    (puthash "pa" "alpha" (opencode-sessions-test--table 'projects))
    (puthash "pb" "beta" (opencode-sessions-test--table 'projects))
    (opencode-sessions--add-prompt "per_1" "b-ask" 'permission)
    (let ((groups (opencode-sessions--groups)))
      (should (equal (mapcar (lambda (group) (substring-no-properties (car group))) groups)
                     '("beta" "alpha")))
      (should (equal (mapcar #'car (cdar groups)) '("b-ask" "b-idle")))
      (should (equal (mapcar #'car (cdadr groups)) '("a-new" "a-old"))))))

(defun opencode-sessions-test--columns (id)
  "Return the text of the columns in the row of session ID."
  (mapcar #'substring-no-properties
          (append (cadr (opencode-sessions--entry
                         (gethash id (opencode-sessions-test--table 'sessions))))
                  nil)))

(defun opencode-sessions-test--set-changes (id &rest roots)
  "Record that session ID changed ROOTS, each (DIRECTORY FILES COUNTS)."
  (puthash id
           (opencode-sessions--work-create
            :updated 2000
            :roots (mapcar (lambda (root)
                             (opencode-changes-root-create :directory (nth 0 root)
                                                           :files (nth 1 root)))
                           roots)
            :stats (cl-loop for root in roots
                            when (nth 2 root) collect (cons (nth 0 root) (nth 2 root))))
           (opencode-sessions-test--table 'changes)))

(ert-deftest test-opencode-sessions/entry-columns ()
  "A row shows the state, title, directory, changes, cost and age."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1" :title "Fix bug" :directory "/tmp/p1"
                                          :cost 1.234 :updated 1000 :idle 900))
    (opencode-sessions-test--set-changes "s1" '("/tmp/p1/" ("a.el") (12 . 3)))
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) 3601.0)))
      (should (equal (opencode-sessions-test--columns "s1")
                     '("done" "Fix bug" "/tmp/p1" "+12 -3" "$1.23" "1h"))))))

(ert-deftest test-opencode-sessions/entry-shows-where-work-happened ()
  "The directory is the worktree the session changed, and changes add up."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1" :directory "/tmp/p1")
            (opencode-sessions-test--info "s2" :directory "/tmp/p1")
            (opencode-sessions-test--info "s3" :directory "/tmp/p1"))
    (opencode-sessions-test--set-changes "s1" '("/tmp/wt/a/" ("x.el") (5 . 1)))
    (opencode-sessions-test--set-changes "s2"
                                         '("/tmp/wt/a/" ("x.el") (5 . 1))
                                         '("/tmp/wt/b/" ("y.el") (2 . 2)))
    (opencode-sessions-test--set-changes "s3")
    (should (equal (seq-subseq (opencode-sessions-test--columns "s1") 2 4)
                   '("/tmp/wt/a" "+5 -1")))
    (should (equal (seq-subseq (opencode-sessions-test--columns "s2") 2 4)
                   '("/tmp/wt/a +1" "+7 -3")))
    (should (equal (seq-subseq (opencode-sessions-test--columns "s3") 2 4)
                   '("/tmp/p1" "")))))

(ert-deftest test-opencode-sessions/archived-sessions-are-not-listed ()
  "Archived sessions are left out of the overview."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1" :archived 5))
    (should-not (gethash "s1" (opencode-sessions-test--table 'sessions)))))

(ert-deftest test-opencode-sessions/format-age ()
  "Ages are rounded down to the largest whole unit."
  (cl-letf (((symbol-function 'float-time) (lambda (&rest _) 200000.0)))
    (should (equal (opencode-sessions--format-age 199990000) "now"))
    (should (equal (opencode-sessions--format-age 199700000) "5m"))
    (should (equal (opencode-sessions--format-age 192800000) "2h"))
    (should (equal (opencode-sessions--format-age 27200000) "2d"))
    (should (equal (opencode-sessions--format-age nil) ""))))

(ert-deftest test-opencode-sessions/parse-shortstat ()
  "Line counts are read from git's short diff summary."
  (should (equal (opencode-sessions--parse-shortstat
                  " 2 files changed, 10 insertions(+), 3 deletions(-)\n")
                 '(10 . 3)))
  (should (equal (opencode-sessions--parse-shortstat " 1 file changed, 1 insertion(+)\n")
                 '(1 . 0)))
  (should (equal (opencode-sessions--parse-shortstat "") '(0 . 0))))

(defmacro opencode-sessions-test--with-clone (&rest body)
  "Run BODY in a clone on branch feature, tracking origin/main.
The clone's tracked files are f and g.  The variable `clone' names it and
`git' runs Git in it, returning its output."
  (declare (indent 0))
  `(let* ((origin (file-name-as-directory (make-temp-file "oc-sessions-origin" t)))
          (clone (concat (directory-file-name origin) "-clone/")))
     (cl-flet ((git (&rest args)
                 (with-temp-buffer
                   (let ((default-directory (if (file-directory-p clone) clone origin)))
                     (apply #'process-file "git" nil t nil
                            "-c" "user.name=t" "-c" "user.email=t@t" args))
                   (buffer-string))))
       (unwind-protect
           (progn
             (git "init" "-q" "-b" "main")
             (write-region "a\n" nil (expand-file-name "f" origin))
             (write-region "a\n" nil (expand-file-name "g" origin))
             (git "add" "f" "g")
             (git "commit" "-qm" "init")
             (git "clone" "-q" origin clone)
             (git "checkout" "-qb" "feature" "--track" "origin/main")
             ,@body)
         (delete-directory origin t)
         (delete-directory clone t)))))

(defun opencode-sessions-test--count (root)
  "Count ROOT's changes for a session and return them once Git finishes."
  (let ((work (opencode-sessions--work-create)))
    (opencode-sessions--count work root)
    (with-timeout (5 (error "Timed out waiting for git"))
      (while (not (opencode-sessions--work-stats work))
        (accept-process-output nil 0.05)))
    (cdar (opencode-sessions--work-stats work))))

(ert-deftest test-opencode-sessions/count-session-files ()
  "Only the session's files count, against the merge base with the default branch."
  (skip-unless (executable-find "git"))
  (opencode-sessions-test--with-clone
    (write-region "a\nb\nc\n" nil (expand-file-name "f" clone))
    (git "commit" "-qam" "more")
    (write-region "a\nb\n" nil (expand-file-name "f" clone))
    (write-region "a\nothers\n" nil (expand-file-name "g" clone))
    (opencode-sessions-test--with-tracker nil
      (should (equal (opencode-sessions-test--count
                      (opencode-changes-root-create :directory clone :files '("f")))
                     '(1 . 0))))))

(ert-deftest test-opencode-sessions/count-new-files-without-touching-the-index ()
  "Untracked files the session wrote count, and Git's index is left alone."
  (skip-unless (executable-find "git"))
  (opencode-sessions-test--with-clone
    (write-region "one\ntwo\n" nil (expand-file-name "new" clone))
    (write-region "a\nb\n" nil (expand-file-name "f" clone))
    (let ((status (git "status" "--porcelain"))
          (index (with-temp-buffer
                   (insert-file-contents-literally (expand-file-name ".git/index" clone))
                   (buffer-string))))
      (opencode-sessions-test--with-tracker nil
        (should (equal (opencode-sessions-test--count
                        (opencode-changes-root-create :directory clone
                                                      :files '("f" "new" "gone")))
                       '(3 . 0))))
      (should (equal (git "status" "--porcelain") status))
      (should (equal (with-temp-buffer
                       (insert-file-contents-literally (expand-file-name ".git/index" clone))
                       (buffer-string))
                     index)))))

(ert-deftest test-opencode-sessions/count-whole-worktree-without-files ()
  "A worktree whose changed files OpenCode did not record counts in full."
  (skip-unless (executable-find "git"))
  (opencode-sessions-test--with-clone
    (write-region "a\nb\n" nil (expand-file-name "f" clone))
    (write-region "a\nc\n" nil (expand-file-name "g" clone))
    (opencode-sessions-test--with-tracker nil
      (should (equal (opencode-sessions-test--count
                      (opencode-changes-root-create :directory clone))
                     '(2 . 0))))))

;;; Finding changes

(defmacro opencode-sessions-test--with-fetch-stubs (&rest body)
  "Run BODY with session trees fetched by hand.
`fetches' lists (SESSION-ID CALLBACK ON-ERROR) for each fetch started,
newest first, and `counted' lists the directories counted.  Each tree's
roots are the directories in its session info's `roots'."
  (declare (indent 0))
  `(let ((overview (get-buffer-create opencode-sessions--buffer-name))
         fetches counted)
     (unwind-protect
         (cl-letf (((symbol-function 'opencode-changes-fetch-tree)
                    (lambda (_conn id callback on-error)
                      (push (list id callback on-error) fetches)))
                   ((symbol-function 'opencode-changes-tree-steps)
                    (lambda (tree &optional _turn) (alist-get 'roots (caar tree))))
                   ((symbol-function 'opencode-changes-roots)
                    (lambda (directories)
                      (when (member "error" directories)
                        (error "Bad history"))
                      (mapcar (lambda (directory)
                                (opencode-changes-root-create :directory directory))
                              directories)))
                   ((symbol-function 'opencode-sessions--count)
                    (lambda (_work root)
                      (push (opencode-changes-root-directory root) counted))))
           ,@body)
       (kill-buffer overview))))

(defun opencode-sessions-test--finish-fetch (fetch &optional updated &rest roots)
  "Finish FETCH with a tree whose session was UPDATED and changed ROOTS."
  (funcall (nth 1 fetch)
           (list (list `((id . ,(car fetch)) (time . ((updated . ,(or updated 2000))))
                         (roots . ,roots))))))

(defun opencode-sessions-test--root-directories (id)
  "Return the directories of the worktrees session ID changed."
  (mapcar #'opencode-changes-root-directory
          (opencode-sessions--work-roots
           (gethash id (opencode-sessions-test--table 'changes)))))

(ert-deftest test-opencode-sessions/fetch-changes-finds-roots-and-counts ()
  "Changes are found from the session tree and each worktree is counted."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1"))
    (opencode-sessions-test--with-fetch-stubs
      (opencode-sessions--fetch-changes "s1")
      (should (equal (mapcar #'car fetches) '("s1")))
      (opencode-sessions-test--finish-fetch (car fetches) 2000 "/wt/a/" "/wt/b/")
      (should (equal (opencode-sessions-test--root-directories "s1") '("/wt/a/" "/wt/b/")))
      (should (equal (sort counted #'string<) '("/wt/a/" "/wt/b/"))))))

(ert-deftest test-opencode-sessions/fetch-changes-again-only-when-updated ()
  "An unchanged session is only counted again; an updated one is fetched again."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1" :updated 2000))
    (opencode-sessions-test--with-fetch-stubs
      (opencode-sessions--fetch-changes "s1")
      (opencode-sessions-test--finish-fetch (car fetches) 2000 "/wt/a/")
      (setq counted nil)
      (opencode-sessions--fetch-changes "s1")
      (should (= (length fetches) 1))
      (should (equal counted '("/wt/a/")))
      (opencode-sessions--update "s1" 'time '((updated . 3000)))
      (opencode-sessions--fetch-changes "s1")
      (should (= (length fetches) 2)))))

(ert-deftest test-opencode-sessions/fetch-changes-limits-concurrent-fetches ()
  "Only a few sessions are fetched at once; the rest wait their turn."
  (let ((ids '("s1" "s2" "s3" "s4" "s5" "s6")))
    (opencode-sessions-test--with-tracker
        (mapcar #'opencode-sessions-test--info ids)
      (opencode-sessions-test--with-fetch-stubs
        (mapc #'opencode-sessions--fetch-changes ids)
        (should (= (length fetches) opencode-sessions--max-fetches))
        (opencode-sessions-test--finish-fetch (car (last fetches)))
        (should (= (length fetches) (1+ opencode-sessions--max-fetches)))
        (funcall (nth 2 (car (last fetches 2))))
        (should (= (length fetches) (length ids)))))))

(ert-deftest test-opencode-sessions/no-fetches-without-the-overview ()
  "Changes are not looked up while only the mode line indicator needs sessions."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1"))
    (opencode-sessions-test--with-fetch-stubs
      (kill-buffer overview)
      (opencode-sessions--fetch-changes "s1" t)
      (should-not fetches))))

(ert-deftest test-opencode-sessions/failed-lookup-is-retried ()
  "A history that cannot be analyzed is looked up again next time."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1"))
    (opencode-sessions-test--with-fetch-stubs
      (opencode-sessions--fetch-changes "s1")
      (cl-letf (((symbol-function 'message) #'ignore))
        (opencode-sessions-test--finish-fetch (car fetches) 2000 "error"))
      (opencode-sessions--fetch-changes "s1")
      (should (= (length fetches) 2)))))

(ert-deftest test-opencode-sessions/relisted-session-is-fetched-once ()
  "A session dropped and listed again while queued is fetched only once."
  (let ((opencode-sessions--max-fetches 1))
    (opencode-sessions-test--with-tracker
        (list (opencode-sessions-test--info "s1") (opencode-sessions-test--info "s2"))
      (opencode-sessions-test--with-fetch-stubs
        (opencode-sessions--fetch-changes "s1")
        (opencode-sessions--fetch-changes "s2")
        (opencode-sessions--hide "s2")
        (opencode-sessions--add (opencode-sessions-test--info "s2"))
        (opencode-sessions--fetch-changes "s2")
        (opencode-sessions-test--finish-fetch (car fetches))
        (opencode-sessions-test--finish-fetch (car fetches))
        (should (equal (mapcar #'car fetches) '("s2" "s1")))))))

(ert-deftest test-opencode-sessions/forced-fetch-while-loading-runs-after ()
  "Forcing a fetch while one runs fetches again once it finishes."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1"))
    (opencode-sessions-test--with-fetch-stubs
      (opencode-sessions--fetch-changes "s1")
      (opencode-sessions--fetch-changes "s1" t)
      (should (= (length fetches) 1))
      (opencode-sessions-test--finish-fetch (car fetches) 2000 "/wt/a/")
      (should (= (length fetches) 2)))))

;;; Events

(ert-deftest test-opencode-sessions/events-ignored-without-tracker ()
  "Events are ignored while nothing shows sessions."
  (let ((opencode-sessions--tracker nil))
    (opencode-sessions-test--event "session.execution.started" '((sessionID . "s1")))))

(ert-deftest test-opencode-sessions/execution-started-marks-running ()
  "A started run marks the session running, learning unknown sessions."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1"))
    (let (fetched)
      (cl-letf (((symbol-function 'opencode-client-session-get)
                 (lambda (_conn id &rest _) (push id fetched))))
        (opencode-sessions-test--event "session.execution.started" '((sessionID . "s1")))
        (opencode-sessions-test--event "session.execution.started" '((sessionID . "child"))))
      (should (equal (gethash "s1" (opencode-sessions-test--table 'running)) "busy"))
      (should (equal fetched '("child"))))))

(ert-deftest test-opencode-sessions/learn-child-records-parent ()
  "Learning a child session records its parent and stops at a listed session."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1"))
    (let (fetched)
      (cl-letf (((symbol-function 'opencode-client-session-get)
                 (lambda (_conn id &rest args)
                   (push id fetched)
                   (funcall (plist-get args :success)
                            :data (opencode-sessions-test--info id :parent "s1")))))
        (opencode-sessions--learn "child"))
      (should (equal fetched '("child")))
      (should (equal (opencode-sessions--root "child") "s1")))))

(ert-deftest test-opencode-sessions/learn-unlisted-root-adds-it ()
  "Learning a top-level session that is not listed adds it to the list."
  (opencode-sessions-test--with-tracker nil
    (let (fetched)
      (cl-letf (((symbol-function 'opencode-client-session-get)
                 (lambda (_conn id &rest args)
                   (funcall (plist-get args :success) :data (opencode-sessions-test--info id))))
                ((symbol-function 'opencode-sessions--fetch-changes)
                 (lambda (id &optional _force) (push id fetched))))
        (opencode-sessions--learn "s9"))
      (should (equal fetched '("s9"))))
    (should (gethash "s9" (opencode-sessions-test--table 'sessions)))))

(ert-deftest test-opencode-sessions/execution-ended-refreshes-session ()
  "A finished run reloads the session's info and finds its changes again."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1"))
    (puthash "s1" "busy" (opencode-sessions-test--table 'running))
    (let (diffed)
      (cl-letf (((symbol-function 'opencode-client-session-get)
                 (lambda (_conn id &rest args)
                   (funcall (plist-get args :success)
                            :data (opencode-sessions-test--info id :idle 50 :outcome "succeeded"))))
                ((symbol-function 'opencode-sessions--fetch-changes)
                 (lambda (id &optional force) (push (list id force) diffed)))
                ((symbol-function 'opencode-sessions--view-selected) #'ignore))
        (opencode-sessions-test--event "session.execution.succeeded" '((sessionID . "s1"))))
      (should-not (gethash "s1" (opencode-sessions-test--table 'running)))
      (should (eq (opencode-sessions--state "s1") 'done))
      (should (equal diffed '(("s1" t)))))))

(ert-deftest test-opencode-sessions/prompt-events ()
  "Permission and form events add and remove pending prompts."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1"))
    (opencode-sessions-test--event "permission.asked" '((id . "per_1") (sessionID . "s1")))
    (opencode-sessions-test--event "form.created" '((form . ((id . "frm_1") (sessionID . "s1")))))
    (should (equal (hash-table-count (opencode-sessions-test--table 'prompts)) 2))
    (should (eq (opencode-sessions--state "s1") 'permission))
    (opencode-sessions-test--event "permission.replied" '((requestID . "per_1") (sessionID . "s1")))
    (should (eq (opencode-sessions--state "s1") 'question))
    (opencode-sessions-test--event "form.cancelled" '((id . "frm_1") (sessionID . "s1")))
    (should (zerop (hash-table-count (opencode-sessions-test--table 'prompts))))))

(ert-deftest test-opencode-sessions/lifecycle-events ()
  "Renames, usage, views and deletions update the listed session."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1" :idle 50))
    (let ((sessions (opencode-sessions-test--table 'sessions)))
      (opencode-sessions-test--event "session.renamed" '((sessionID . "s1") (title . "New")))
      (should (equal (alist-get 'title (gethash "s1" sessions)) "New"))
      (opencode-sessions-test--event "session.usage.updated" '((sessionID . "s1") (cost . 2.5)))
      (should (equal (alist-get 'cost (gethash "s1" sessions)) 2.5))
      (opencode-sessions-test--event "session.viewed" '((sessionID . "s1") (idle . 50)))
      (should-not (opencode-sessions--state "s1"))
      (opencode-sessions-test--event "session.deleted" '((sessionID . "s1")))
      (should-not (gethash "s1" sessions)))))

(ert-deftest test-opencode-sessions/created-events ()
  "New top-level sessions are fetched; new children are linked to their parent."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1"))
    (let (fetched)
      (cl-letf (((symbol-function 'opencode-client-session-get)
                 (lambda (_conn id &rest _) (push id fetched))))
        (opencode-sessions-test--event "session.created" '((sessionID . "s2")))
        (opencode-sessions-test--event "session.created" '((sessionID . "c1") (parentID . "s1"))))
      (should (equal fetched '("s2")))
      (should (equal (opencode-sessions--root "c1") "s1")))))

(ert-deftest test-opencode-sessions/moved-session-changes-project ()
  "A session moved into another project is grouped under that project."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1"))
    (puthash "p2" "two" (opencode-sessions-test--table 'projects))
    (opencode-sessions-test--event "session.moved"
                                   '((sessionID . "s1") (projectID . "p2")
                                     (location . ((directory . "/tmp/p2")))))
    (let ((info (gethash "s1" (opencode-sessions-test--table 'sessions))))
      (should (equal (alist-get 'projectID info) "p2"))
      (should (equal (opencode-sessions--directory info) "/tmp/p2")))))

(ert-deftest test-opencode-sessions/step-after-retry-is-running ()
  "A session that starts a step after a retry is running again."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1"))
    (opencode-sessions-test--event "session.retry.scheduled" '((sessionID . "s1")))
    (should (eq (opencode-sessions--state "s1") 'retrying))
    (opencode-sessions-test--event "session.step.started" '((sessionID . "s1")))
    (should (eq (opencode-sessions--state "s1") 'running))))

(ert-deftest test-opencode-sessions/forked-session-is-learned ()
  "A forked session is looked up so it appears in the list."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1"))
    (let (fetched)
      (cl-letf (((symbol-function 'opencode-client-session-get)
                 (lambda (_conn id &rest _) (push id fetched))))
        (opencode-sessions-test--event "session.forked" '((sessionID . "s2") (parentID . "s1"))))
      (should (equal fetched '("s2"))))))

;;; Loading

(ert-deftest test-opencode-sessions/reload-reconnects-and-loads ()
  "Reloading goes through the connection and loads the sessions again."
  (opencode-sessions-test--with-tracker nil
    (let (connected loaded)
      (cl-letf (((symbol-function 'opencode-run-server)
                 (lambda (_directory on-ready) (setq connected t) (funcall on-ready 'conn)))
                ((symbol-function 'opencode-sessions--load) (lambda () (setq loaded t)))
                ((symbol-function 'opencode-sessions--render) #'ignore))
        (opencode-sessions--reload))
      (should connected)
      (should loaded))))

(ert-deftest test-opencode-sessions/load ()
  "Loading lists sessions, projects, running sessions and their prompts."
  (opencode-sessions-test--with-tracker nil
    (let (diffed)
      (cl-letf (((symbol-function 'opencode-client-projects)
                 (lambda (_conn &rest args)
                   (funcall (plist-get args :success)
                            :data '(((id . "p1") (canonical . "/tmp/p1"))
                                    ((id . "p2") (canonical . "/tmp/p2") (name . "Two"))))))
                ((symbol-function 'opencode-client-sessions)
                 (lambda (_conn &rest args)
                   (should (plist-get args :roots))
                   (funcall (plist-get args :success)
                            :data (list (opencode-sessions-test--info "s1")
                                        (opencode-sessions-test--info "s2" :directory "/tmp/p2"
                                                                      :project "p2")
                                        (opencode-sessions-test--info "s3" :archived 1)))))
                ((symbol-function 'opencode-client-session-active)
                 (lambda (_conn &rest args)
                   (funcall (plist-get args :success) :data '((s1 . ((type . "running")))))))
                ((symbol-function 'opencode-client-session-permissions)
                 (lambda (_conn id &rest args)
                   (funcall (plist-get args :success)
                            :data `(((id . "per_1") (sessionID . ,id))))))
                ((symbol-function 'opencode-client-session-forms)
                 (lambda (_conn _id &rest args) (funcall (plist-get args :success) :data nil)))
                ((symbol-function 'opencode-sessions--fetch-changes)
                 (lambda (id &optional _force) (push id diffed)))
                ((symbol-function 'float-time) (lambda (&rest _) 3.0)))
        (opencode-sessions--load))
      (let ((sessions (opencode-sessions-test--table 'sessions))
            (projects (opencode-sessions-test--table 'projects)))
        (should (equal (sort (hash-table-keys sessions) #'string<) '("s1" "s2")))
        (should (equal (gethash "p1" projects) "/tmp/p1"))
        (should (equal (gethash "p2" projects) "Two"))
        (should (eq (opencode-sessions--state "s1") 'permission))
        (should (equal (sort diffed #'string<) '("s1" "s2")))))))

(defun opencode-sessions-test--pages (pages)
  "Return a stub for `opencode-client-sessions' serving PAGES in order.
Each page is a list of session infos.  Like the server, the stub returns
a cursor with every non-empty page.  It records the cursors it was asked
for in `opencode-sessions-test--cursors'."
  (lambda (_conn &rest args)
    (push (plist-get args :cursor) opencode-sessions-test--cursors)
    (let* ((index (if-let* ((cursor (plist-get args :cursor))) (string-to-number cursor) 0))
           (page (nth index pages)))
      (funcall (plist-get args :success)
               :data page :cursor (and page (number-to-string (1+ index)))))))

(defvar opencode-sessions-test--cursors nil
  "Cursors requested from the stub made by `opencode-sessions-test--pages'.")

(defmacro opencode-sessions-test--with-load-stubs (pages &rest body)
  "Run BODY with the server stubbed to list session PAGES and nothing else.
The clock reads one million seconds."
  (declare (indent 1))
  `(progn
     (setq opencode-sessions-test--cursors nil)
     (cl-letf (((symbol-function 'opencode-client-projects) #'ignore)
               ((symbol-function 'opencode-client-session-active) #'ignore)
               ((symbol-function 'opencode-sessions--fetch-changes) #'ignore)
               ((symbol-function 'opencode-client-sessions) (opencode-sessions-test--pages ,pages))
               ((symbol-function 'float-time) (lambda (&rest _) 1000000.0)))
       (let ((opencode-sessions--page-size 2))
         ,@body))))

(ert-deftest test-opencode-sessions/load-pages-back-to-cutoff ()
  "Loading pages through sessions until they are older than the window."
  (let ((opencode-sessions-days 7)
        (day 86400000)
        (now 1000000000))
    (opencode-sessions-test--with-tracker nil
      (opencode-sessions-test--with-load-stubs
          (list (list (opencode-sessions-test--info "a" :updated (- now day))
                      (opencode-sessions-test--info "b" :updated (- now (* 2 day))))
                (list (opencode-sessions-test--info "c" :updated (- now (* 6 day)))
                      (opencode-sessions-test--info "d" :updated (- now (* 8 day))))
                (list (opencode-sessions-test--info "e" :updated (- now (* 9 day)))))
        (opencode-sessions--load))
      (should (equal (sort (hash-table-keys (opencode-sessions-test--table 'sessions)) #'string<)
                     '("a" "b" "c")))
      (should (equal (reverse opencode-sessions-test--cursors) '(nil "1"))))))

(ert-deftest test-opencode-sessions/load-stops-at-cap ()
  "Loading stops once it reaches `opencode-sessions-max'."
  (let ((opencode-sessions-days 7)
        (opencode-sessions-max 3)
        (now 1000000000))
    (opencode-sessions-test--with-tracker nil
      (opencode-sessions-test--with-load-stubs
          (list (list (opencode-sessions-test--info "a" :updated now)
                      (opencode-sessions-test--info "b" :updated now))
                (list (opencode-sessions-test--info "c" :updated now)
                      (opencode-sessions-test--info "d" :updated now))
                (list (opencode-sessions-test--info "e" :updated now)))
        (opencode-sessions--load))
      (should (equal (sort (hash-table-keys (opencode-sessions-test--table 'sessions)) #'string<)
                     '("a" "b" "c")))
      (should (equal (length opencode-sessions-test--cursors) 2)))))

(ert-deftest test-opencode-sessions/load-stops-at-short-page ()
  "A page shorter than requested is the last one."
  (let ((now 1000000000))
    (opencode-sessions-test--with-tracker nil
      (opencode-sessions-test--with-load-stubs
          (list (list (opencode-sessions-test--info "a" :updated now)
                      (opencode-sessions-test--info "b" :updated now))
                (list (opencode-sessions-test--info "c" :updated now)))
        (opencode-sessions--load))
      (should (= (hash-table-count (opencode-sessions-test--table 'sessions)) 3))
      (should (equal (length opencode-sessions-test--cursors) 2)))))

(ert-deftest test-opencode-sessions/load-asks-server-to-exclude-archived ()
  "The first page asks the server to leave out archived sessions."
  (opencode-sessions-test--with-tracker nil
    (let (archived)
      (cl-letf (((symbol-function 'opencode-client-projects) #'ignore)
                ((symbol-function 'opencode-client-session-active) #'ignore)
                ((symbol-function 'opencode-client-sessions)
                 (lambda (_conn &rest args)
                   (setq archived (plist-get args :archived))
                   (funcall (plist-get args :success) :data nil :cursor nil))))
        (opencode-sessions--load))
      (should (eq archived :exclude)))))

(ert-deftest test-opencode-sessions/load-keeps-sessions-added-meanwhile ()
  "Sessions updated after the load started survive it; stale ones are dropped."
  (let ((now 1000000000))
    (opencode-sessions-test--with-tracker
        (list (opencode-sessions-test--info "stale" :updated (- now 5000))
              (opencode-sessions-test--info "new" :updated (+ now 10)))
      (opencode-sessions-test--with-load-stubs
          (list (list (opencode-sessions-test--info "a" :updated now)))
        (opencode-sessions--load))
      (should (equal (sort (hash-table-keys (opencode-sessions-test--table 'sessions)) #'string<)
                     '("a" "new"))))))

;;; Viewing

(ert-deftest test-opencode-sessions/mark-viewed-once ()
  "An unread session is reported as viewed once per idle transition."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1" :idle 50)
            (opencode-sessions-test--info "s2" :idle 50 :viewed 50))
    (let (views)
      (cl-letf (((symbol-function 'opencode-client-session-view)
                 (lambda (_conn id idle &rest _) (push (cons id idle) views))))
        (opencode-sessions--mark-viewed "s1")
        (opencode-sessions--mark-viewed "s1")
        (opencode-sessions--mark-viewed "s2"))
      (should (equal views '(("s1" . 50)))))))

(ert-deftest test-opencode-sessions/view-selected-uses-root ()
  "Selecting a subagent's buffer marks its top-level session viewed."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1" :idle 50))
    (puthash "child" "s1" (opencode-sessions-test--table 'parents))
    (let (viewed)
      (with-temp-buffer
        (setq-local opencode-session--session (opencode-session-create :id "child"))
        (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer)))
                  ((symbol-function 'opencode-sessions--mark-viewed)
                   (lambda (id) (push id viewed))))
          (opencode-sessions--view-selected)))
      (should (equal viewed '("s1"))))))

;;; Indicator

(ert-deftest test-opencode-sessions/indicator-counts ()
  "The indicator counts sessions waiting on you, running, and done."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1")
            (opencode-sessions-test--info "s2")
            (opencode-sessions-test--info "s3" :idle 5)
            (opencode-sessions-test--info "s4" :idle 5 :viewed 5))
    (opencode-sessions--add-prompt "per_1" "s1" 'permission)
    (puthash "s2" "busy" (opencode-sessions-test--table 'running))
    (should (equal (substring-no-properties (opencode-sessions--indicator-text))
                   " [OC: 1 waiting, 1 running, 1 done]"))))

(ert-deftest test-opencode-sessions/indicator-empty-when-quiet ()
  "The indicator is empty when no session needs attention or runs."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1" :idle 5 :viewed 5))
    (should (equal (opencode-sessions--indicator-text) ""))))

(ert-deftest test-opencode-sessions/indicator-mode-shows-in-mode-line ()
  "The indicator is added to the mode line as a plain element, not a conditional.
A mode line list starting with a symbol is a conditional, so an empty
`global-mode-string' must not become a list headed by the indicator."
  (let ((global-mode-string nil))
    (cl-letf (((symbol-function 'opencode-sessions--ensure-tracking) #'ignore)
              ((symbol-function 'opencode-sessions--stop-unless-needed) #'ignore))
      (unwind-protect
          (progn
            (opencode-sessions-indicator-mode 1)
            (should (stringp (car global-mode-string)))
            (should (memq 'opencode-sessions--indicator global-mode-string)))
        (opencode-sessions-indicator-mode -1)))
    (should-not (memq 'opencode-sessions--indicator global-mode-string))))

;;; Overview buffer

(ert-deftest test-opencode-sessions/render-buffer ()
  "The overview buffer shows a heading per project and a row per session."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1" :title "First"))
    (puthash "p1" "alpha" (opencode-sessions-test--table 'projects))
    (with-current-buffer (get-buffer-create opencode-sessions--buffer-name)
      (unwind-protect
          (let ((opencode-sessions-indicator-mode t))
            (opencode-sessions-mode)
            (opencode-sessions--render)
            (goto-char (point-min))
            (should (search-forward "alpha" nil t))
            (should (search-forward "First" nil t))
            (should (equal (tabulated-list-get-id) "s1")))
        (kill-buffer)))))

(defmacro opencode-sessions-test--in-mode (name mode infos &rest body)
  "Run BODY in a rendered overview buffer NAME in MODE listing session INFOS."
  (declare (indent 3))
  `(opencode-sessions-test--with-tracker ,infos
     (with-current-buffer (get-buffer-create ,name)
       (unwind-protect
           (progn
             (funcall ,mode)
             (opencode-sessions--render)
             (goto-char (point-min))
             ,@body)
         (let ((kill-buffer-hook nil))
           (kill-buffer))))))

(defmacro opencode-sessions-test--in-overview (infos &rest body)
  "Run BODY in a rendered overview buffer listing session INFOS."
  (declare (indent 1))
  `(opencode-sessions-test--in-mode opencode-sessions--buffer-name #'opencode-sessions-mode
       ,infos ,@body))

(defmacro opencode-sessions-test--in-sidebar (infos &rest body)
  "Run BODY in a rendered sidebar buffer listing session INFOS."
  (declare (indent 1))
  `(opencode-sessions-test--in-mode opencode-sessions--sidebar-buffer-name
       #'opencode-sessions-sidebar-mode ,infos ,@body))

(defun opencode-sessions-test--goto (id)
  "Move point to the row of session ID."
  (goto-char (point-min))
  (while (and (not (eobp)) (not (equal (tabulated-list-get-id) id)))
    (forward-line)))

(defun opencode-sessions-test--tag ()
  "Return the mark tag shown on the current row."
  (buffer-substring-no-properties (line-beginning-position) (1+ (line-beginning-position))))

(ert-deftest test-opencode-sessions/flag-tags-row-and-advances ()
  "Flagging a session tags its row and moves to the next session."
  (opencode-sessions-test--in-overview
      (list (opencode-sessions-test--info "s1" :updated 200)
            (opencode-sessions-test--info "s2" :updated 100))
    (opencode-sessions-test--goto "s1")
    (opencode-sessions-flag-delete)
    (should (equal (tabulated-list-get-id) "s2"))
    (opencode-sessions-test--goto "s1")
    (should (equal (opencode-sessions-test--tag) "D"))
    (opencode-sessions-test--goto "s2")
    (should (equal (opencode-sessions-test--tag) " "))))

(ert-deftest test-opencode-sessions/flags-survive-redraws ()
  "Flags stay visible when rows move, the list is sorted, or it is reverted."
  (opencode-sessions-test--in-overview
      (list (opencode-sessions-test--info "s1" :updated 200 :title "B")
            (opencode-sessions-test--info "s2" :updated 100 :title "A"))
    (opencode-sessions-test--goto "s1")
    (opencode-sessions-flag-delete)
    (opencode-sessions--update "s2" 'time '((updated . 900)))
    (opencode-sessions--render)
    (opencode-sessions-test--goto "s1")
    (should (equal (opencode-sessions-test--tag) "D"))
    (opencode-sessions-test--goto "s2")
    (should (equal (opencode-sessions-test--tag) " "))
    (opencode-sessions-test--goto "s1")
    (forward-char 3)
    (tabulated-list-sort 1)
    (opencode-sessions-test--goto "s1")
    (should (equal (opencode-sessions-test--tag) "D"))
    (cl-letf (((symbol-function 'opencode-sessions--reload) #'ignore))
      (revert-buffer))
    (opencode-sessions-test--goto "s1")
    (should (equal (opencode-sessions-test--tag) "D"))))

(ert-deftest test-opencode-sessions/unmark ()
  "Unmarking clears one row's flag; unmarking all clears every flag."
  (opencode-sessions-test--in-overview
      (list (opencode-sessions-test--info "s1" :updated 200)
            (opencode-sessions-test--info "s2" :updated 100))
    (opencode-sessions-test--goto "s1")
    (opencode-sessions-flag-delete)
    (opencode-sessions-flag-delete)
    (opencode-sessions-test--goto "s1")
    (opencode-sessions-unmark)
    (opencode-sessions-test--goto "s1")
    (should (equal (opencode-sessions-test--tag) " "))
    (opencode-sessions-unmark-all)
    (opencode-sessions-test--goto "s2")
    (should (equal (opencode-sessions-test--tag) " "))
    (should (zerop (hash-table-count opencode-sessions--flagged)))))

(ert-deftest test-opencode-sessions/unmark-backward ()
  "Unmarking backward clears the previous session's flag."
  (opencode-sessions-test--in-overview
      (list (opencode-sessions-test--info "s1" :updated 200)
            (opencode-sessions-test--info "s2" :updated 100))
    (opencode-sessions-test--goto "s1")
    (opencode-sessions-flag-delete)
    (opencode-sessions-unmark-backward)
    (should (equal (tabulated-list-get-id) "s1"))
    (should (equal (opencode-sessions-test--tag) " "))
    (should-not (gethash "s1" opencode-sessions--flagged))))

(ert-deftest test-opencode-sessions/execute-deletes-flagged ()
  "Executing deletes the flagged sessions after confirming."
  (opencode-sessions-test--in-overview
      (list (opencode-sessions-test--info "s1" :updated 300)
            (opencode-sessions-test--info "s2" :updated 200)
            (opencode-sessions-test--info "s3" :updated 100))
    (opencode-sessions-test--goto "s1")
    (opencode-sessions-flag-delete)
    (opencode-sessions-flag-delete)
    (let (deleted prompt (loads 0))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (text) (setq prompt text) t))
                ((symbol-function 'opencode-sessions--load) (lambda () (cl-incf loads)))
                ((symbol-function 'opencode-client-session-delete)
                 (lambda (_conn id &rest args)
                   (push id deleted)
                   (should (zerop loads))
                   (if (equal id "s2")
                       (cl-letf (((symbol-function 'message) #'ignore))
                         (funcall (plist-get args :error)))
                     (funcall (plist-get args :success))))))
        (opencode-sessions-execute))
      (should (string-match-p "\\`Delete 2 sessions (Session s[12], Session s[12])\\? \\'" prompt))
      (should (equal (sort deleted #'string<) '("s1" "s2")))
      (should-not (gethash "s1" (opencode-sessions-test--table 'sessions)))
      (should (gethash "s3" (opencode-sessions-test--table 'sessions)))
      (should (equal (hash-table-keys opencode-sessions--flagged) '("s2")))
      (should (= loads 1)))))

(ert-deftest test-opencode-sessions/flag-archive-tags-row ()
  "Flagging a session for archiving tags it with A; flagging again replaces the flag."
  (opencode-sessions-test--in-overview
      (list (opencode-sessions-test--info "s1"))
    (opencode-sessions-test--goto "s1")
    (opencode-sessions-flag-archive)
    (opencode-sessions-test--goto "s1")
    (should (equal (opencode-sessions-test--tag) "A"))
    (should (eq (gethash "s1" opencode-sessions--flagged) 'archive))
    (opencode-sessions-flag-delete)
    (opencode-sessions-test--goto "s1")
    (should (equal (opencode-sessions-test--tag) "D"))))

(ert-deftest test-opencode-sessions/execute-archives-and-deletes ()
  "Executing archives and deletes the flagged sessions after one confirmation."
  (opencode-sessions-test--in-overview
      (list (opencode-sessions-test--info "s1" :updated 300 :title "One")
            (opencode-sessions-test--info "s2" :updated 200 :title "Two")
            (opencode-sessions-test--info "s3" :updated 100))
    (opencode-sessions-test--goto "s1")
    (opencode-sessions-flag-archive)
    (opencode-sessions-flag-delete)
    (let (calls prompt (loads 0))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (text) (setq prompt text) t))
                ((symbol-function 'opencode-sessions--load) (lambda () (cl-incf loads)))
                ((symbol-function 'opencode-client-session-archive)
                 (lambda (_conn id archived &rest args)
                   (push (list 'archive id archived) calls)
                   (funcall (plist-get args :success))))
                ((symbol-function 'opencode-client-session-delete)
                 (lambda (_conn id &rest args)
                   (push (list 'delete id) calls)
                   (funcall (plist-get args :success)))))
        (opencode-sessions-execute))
      (should (equal prompt "Archive 1 session (One) and delete 1 session (Two)? "))
      (should (equal (sort calls (lambda (a b) (string< (cadr a) (cadr b))))
                     '((archive "s1" t) (delete "s2"))))
      (should-not (gethash "s1" (opencode-sessions-test--table 'sessions)))
      (should (eq (gethash "s1" (opencode-sessions-test--table 'parents)) :root))
      (should-not (gethash "s2" (opencode-sessions-test--table 'sessions)))
      (should (zerop (hash-table-count opencode-sessions--flagged)))
      (should (= loads 1)))))

(ert-deftest test-opencode-sessions/execute-archive-only-asks-y-or-n ()
  "Archiving alone is reversible, so it asks a short question."
  (opencode-sessions-test--in-overview
      (list (opencode-sessions-test--info "s1" :title "One"))
    (opencode-sessions-test--goto "s1")
    (opencode-sessions-flag-archive)
    (let (prompt)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (text) (setq prompt text) nil))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) (error "Should not ask"))))
        (opencode-sessions-execute))
      (should (equal prompt "Archive 1 session (One)? ")))))

(ert-deftest test-opencode-sessions/archive-events ()
  "Archived sessions leave the list; restored ones are looked up again."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1"))
    (opencode-sessions-test--event "session.archived" '((sessionID . "s1")))
    (should-not (gethash "s1" (opencode-sessions-test--table 'sessions)))
    (let (fetched)
      (cl-letf (((symbol-function 'opencode-client-session-get)
                 (lambda (_conn id &rest _) (push id fetched))))
        (opencode-sessions-test--event "session.unarchived" '((sessionID . "s1")))
        (opencode-sessions-test--event "session.unarchived" '((sessionID . "never-seen")))
        (puthash "child" "s1" (opencode-sessions-test--table 'parents))
        (opencode-sessions-test--event "session.unarchived" '((sessionID . "child"))))
      (should (equal fetched '("never-seen" "s1"))))))

(ert-deftest test-opencode-sessions/execute-declined-keeps-flags ()
  "Declining the confirmation changes nothing."
  (opencode-sessions-test--in-overview
      (list (opencode-sessions-test--info "s1"))
    (opencode-sessions-test--goto "s1")
    (opencode-sessions-flag-delete)
    (cl-letf (((symbol-function 'yes-or-no-p) #'ignore)
              ((symbol-function 'opencode-client-session-delete)
               (lambda (&rest _) (error "Should not delete"))))
      (opencode-sessions-execute))
    (should (gethash "s1" opencode-sessions--flagged))))

(ert-deftest test-opencode-sessions/execute-without-flags-errors ()
  "Executing with nothing marked signals a user error."
  (opencode-sessions-test--in-overview
      (list (opencode-sessions-test--info "s1"))
    (should-error (opencode-sessions-execute) :type 'user-error)))

(ert-deftest test-opencode-sessions/new-session-in-same-project ()
  "A new session starts in the directory of the session at point."
  (opencode-sessions-test--in-overview
      (list (opencode-sessions-test--info "s1" :directory "/tmp/p1"))
    (opencode-sessions-test--goto "s1")
    (let (directory)
      (cl-letf (((symbol-function 'opencode)
                 (lambda (dir) (interactive (list default-directory)) (setq directory dir))))
        (opencode-sessions-new))
      (should (equal directory "/tmp/p1/")))))

;;; Sidebar

(defun opencode-sessions-test--row-lines (id)
  "Return the text of the lines in the row of session ID."
  (opencode-sessions-test--goto id)
  (let (lines)
    (while (equal (tabulated-list-get-id) id)
      (push (buffer-substring-no-properties (line-beginning-position) (line-end-position))
            lines)
      (forward-line))
    (nreverse lines)))

(ert-deftest test-opencode-sessions/sidebar-rows-span-lines ()
  "A sidebar row shows the title, then the details, then the directory."
  (opencode-sessions-test--in-sidebar
      (list (opencode-sessions-test--info "s1" :title "Fix bug" :directory "/tmp/p1"
                                          :cost 1.234 :updated 1000 :idle 900)
            (opencode-sessions-test--info "s2" :title "Quiet" :directory "/tmp/p2"
                                          :updated 1000 :idle 5 :viewed 5))
    (opencode-sessions-test--set-changes "s1" '("/tmp/p1/" ("a.el") (12 . 3)))
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) 3601.0)))
      (opencode-sessions--render))
    (should-not header-line-format)
    (should (equal (opencode-sessions-test--row-lines "s1")
                   '("  Fix bug" "    done  +12 -3  $1.23  1h" "    /tmp/p1")))
    (should (equal (opencode-sessions-test--row-lines "s2")
                   '("  Quiet" "    1h" "    /tmp/p2")))))

(ert-deftest test-opencode-sessions/sidebar-flags-and-moves-by-row ()
  "Flagging from any line of a sidebar row tags its first line and moves to the next row."
  (opencode-sessions-test--in-sidebar
      (list (opencode-sessions-test--info "s1" :updated 300)
            (opencode-sessions-test--info "s2" :updated 200)
            (opencode-sessions-test--info "s3" :updated 100))
    (opencode-sessions-test--goto "s1")
    (forward-line 2)
    (opencode-sessions-flag-delete)
    (should (equal (tabulated-list-get-id) "s2"))
    (should (equal (opencode-sessions-test--tag) " "))
    (opencode-sessions-test--goto "s1")
    (should (equal (opencode-sessions-test--tag) "D"))
    (opencode-sessions--render)
    (opencode-sessions-test--goto "s1")
    (should (equal (opencode-sessions-test--tag) "D"))
    (opencode-sessions-test--goto "s3")
    (forward-line 1)
    (opencode-sessions-unmark-backward)
    (should (equal (tabulated-list-get-id) "s2"))
    (should (equal (opencode-sessions-test--tag) " "))
    (opencode-sessions-test--goto "s1")
    (forward-line 1)
    (opencode-sessions-next)
    (should (equal (tabulated-list-get-id) "s2"))
    (should (bolp))
    (should-not (equal (tabulated-list-get-id (pos-bol 0)) "s2"))
    (opencode-sessions-previous)
    (should (equal (tabulated-list-get-id) "s1"))
    (opencode-sessions-previous)
    (should (equal (tabulated-list-get-id) "s1"))))

(ert-deftest test-opencode-sessions/redraw-keeps-other-windows-in-place ()
  "Redrawing keeps an unselected window on its session and scroll position."
  (opencode-sessions-test--in-sidebar
      (cl-loop for i from 1 to 30
               collect (opencode-sessions-test--info (format "s%02d" i) :updated (- 1000 i)))
    (let ((window (split-window)))
      (unwind-protect
          (progn
            (set-window-buffer window (current-buffer))
            (opencode-sessions-test--goto "s20")
            (forward-line 1)
            (set-window-point window (point))
            (opencode-sessions-test--goto "s10")
            (set-window-start window (point))
            (goto-char (point-min))
            (opencode-sessions--update "s05" 'title "Renamed")
            (opencode-sessions--render)
            (should (equal (tabulated-list-get-id (window-point window)) "s20"))
            (should (equal (tabulated-list-get-id
                            (save-excursion (goto-char (window-point window)) (pos-bol 0)))
                           "s20"))
            (should (equal (tabulated-list-get-id (window-start window)) "s10")))
        (delete-window window)))))

(ert-deftest test-opencode-sessions/redraw-keeps-point-within-row ()
  "Redrawing keeps point on the same line and column of its session's row."
  (opencode-sessions-test--in-sidebar
      (list (opencode-sessions-test--info "s1" :updated 300)
            (opencode-sessions-test--info "s2" :updated 200))
    (opencode-sessions-test--goto "s2")
    (forward-line 2)
    (move-to-column 6)
    (opencode-sessions--update "s1" 'time '((updated . 100)))
    (opencode-sessions--render)
    (should (equal (tabulated-list-get-id) "s2"))
    (should (equal (tabulated-list-get-id (pos-bol 0)) "s2"))
    (should (equal (tabulated-list-get-id (pos-bol -1)) "s2"))
    (should (= (current-column) 6))))

(ert-deftest test-opencode-sessions/render-updates-every-overview ()
  "Both the overview buffer and the sidebar are redrawn."
  (opencode-sessions-test--in-sidebar
      (list (opencode-sessions-test--info "s1" :title "First"))
    (with-current-buffer (get-buffer-create opencode-sessions--buffer-name)
      (unwind-protect
          (progn
            (opencode-sessions-mode)
            (opencode-sessions--update "s1" 'title "Renamed")
            (opencode-sessions--render)
            (goto-char (point-min))
            (should (search-forward "Renamed" nil t)))
        (let ((kill-buffer-hook nil))
          (kill-buffer))))
    (goto-char (point-min))
    (should (search-forward "Renamed" nil t))))

(ert-deftest test-opencode-sessions/sidebar-alone-keeps-tracking ()
  "Changes are looked up and tracking continues while only the sidebar is open."
  (opencode-sessions-test--in-sidebar
      (list (opencode-sessions-test--info "s1"))
    (let (fetched)
      (cl-letf (((symbol-function 'opencode-changes-fetch-tree)
                 (lambda (_conn id &rest _) (push id fetched))))
        (opencode-sessions--fetch-changes "s1"))
      (should (equal fetched '("s1"))))
    (opencode-sessions--stop-unless-needed)
    (should opencode-sessions--tracker)
    (opencode-sessions--stop-unless-needed (current-buffer))
    (should-not opencode-sessions--tracker)))

(ert-deftest test-opencode-sessions/sidebar-toggles-and-opens-in-main-window ()
  "The sidebar shows in a side window, opens sessions elsewhere, and toggles off."
  (opencode-sessions-test--with-tracker
      (list (opencode-sessions-test--info "s1"))
    (let ((main (selected-window))
          opened-in)
      (delete-other-windows)
      (unwind-protect
          (cl-letf (((symbol-function 'opencode-sessions--ensure-tracking) #'ignore)
                    ((symbol-function 'opencode--ensure-session-buffer)
                     (lambda (&rest _) (setq opened-in (selected-window)))))
            (opencode-sessions-sidebar)
            (let ((sidebar (selected-window)))
              (should (eq (window-parameter sidebar 'window-side)
                          opencode-sessions-sidebar-side))
              (should (eq major-mode 'opencode-sessions-sidebar-mode))
              (opencode-sessions--render)
              (opencode-sessions-test--goto "s1")
              (opencode-sessions-open)
              (should (eq opened-in main))
              (should (eq (selected-window) main))
              (with-current-buffer opencode-sessions--sidebar-buffer-name
                (should (eq (key-binding "S") #'undefined)))
              (opencode-sessions-sidebar)
              (should-not (window-live-p sidebar))
              (should-not (get-buffer opencode-sessions--sidebar-buffer-name))))
        (when-let* ((buffer (get-buffer opencode-sessions--sidebar-buffer-name)))
          (let ((kill-buffer-hook nil))
            (kill-buffer buffer)))))))

(ert-deftest test-opencode-sessions/evil-normal-state-bindings ()
  "Under Evil, the overview's commands are reachable from normal state."
  (skip-unless (require 'evil nil t))
  (let ((evil-mode t))
    (with-temp-buffer
      (opencode-sessions-mode)
      (evil-local-mode 1)
      (evil-normal-state)
      (should (eq (key-binding "x") #'opencode-sessions-execute))
      (should (eq (key-binding "d") #'opencode-sessions-flag-delete))
      (should (eq (key-binding "a") #'opencode-sessions-flag-archive))
      (should (eq (key-binding "u") #'opencode-sessions-unmark))
      (should (eq (key-binding "go") #'opencode-sessions-open-other-window))
      (should (eq (key-binding (kbd "RET")) #'opencode-sessions-open))
      (should (string-prefix-p "evil-" (symbol-name (key-binding "k"))))
      (opencode-sessions-sidebar-mode)
      (evil-normal-state)
      (should (eq (key-binding "x") #'opencode-sessions-execute)))))

(provide 'emacs-opencode-sessions-test)

;;; emacs-opencode-sessions-test.el ends here
