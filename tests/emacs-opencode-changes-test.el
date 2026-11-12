;;; emacs-opencode-changes-test.el --- Tests for finding session changes  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-opencode-changes)

(defmacro opencode-changes-test--with-tree (names &rest body)
  "Bind NAMES to Git worktrees under a fresh temporary directory around BODY.
Each worktree is an empty directory holding a `.git' directory.  The
variable `tmp' holds the parent directory."
  (declare (indent 1))
  `(let* ((tmp (file-name-as-directory (file-truename (make-temp-file "oc-changes" t))))
          ,@(mapcar (lambda (name)
                      `(,name (file-name-as-directory (expand-file-name ,(symbol-name name) tmp))))
                    names))
     (unwind-protect
         (progn
           ,@(mapcar (lambda (name) `(make-directory (expand-file-name ".git" ,name) t)) names)
           ,@body)
       (delete-directory tmp t))))

(defun opencode-changes-test--info (id directory &optional parent)
  "Return session info for ID located in DIRECTORY with PARENT."
  `((id . ,id) (title . ,(concat "Session " id))
    (location . ((directory . ,(directory-file-name directory))))
    ,@(when parent `((parentID . ,parent)))))

(defun opencode-changes-test--step (id &rest props)
  "Return an assistant message ID built from PROPS.
PROPS accepts :start, :end, :files (t for an empty list), and :tools, a
list of (NAME INPUT METADATA)."
  (let ((files (plist-get props :files)))
    `((id . ,id) (type . "assistant") (time . ((created . ,(or (plist-get props :time) 0))))
      (snapshot . (,@(when (plist-get props :start) `((start . ,(plist-get props :start))))
                   ,@(when (plist-get props :end) `((end . ,(plist-get props :end))))
                   ,@(cond ((eq files t) '((files)))
                           (files `((files . ,files))))))
      (content . ,(mapcar (lambda (tool)
                            `((type . "tool") (name . ,(nth 0 tool))
                              (state . ((status . "completed") (input . ,(nth 1 tool))
                                        (metadata . ,(nth 2 tool))))))
                          (plist-get props :tools))))))

(defun opencode-changes-test--move (from to)
  "Return a location-switched message from directory FROM to TO."
  `((id . ,(concat "mv-" to)) (type . "location-switched")
    (location . ((directory . ,(directory-file-name to))))
    (previous . ((location . ((directory . ,(directory-file-name from))))))))

(defun opencode-changes-test--root-summary (roots)
  "Return ROOTS as a list of (DIRECTORY FILES SESSIONS)."
  (mapcar (lambda (root)
            (list (opencode-changes-root-directory root)
                  (opencode-changes-root-files root)
                  (opencode-changes-root-sessions root)))
          roots))

;;; Turns

(ert-deftest test-opencode-changes/turns-end-at-idle-markers ()
  "Prompts steered in before the session went idle share a turn."
  (let ((turns (opencode-changes--turns
                '(((id . "u1") (type . "user")) ((id . "a1") (type . "assistant"))
                  ((id . "u2") (type . "user")) ((id . "i1") (type . "idle"))
                  ((id . "u3") (type . "user")) ((id . "a2") (type . "assistant"))))))
    (should (equal (mapcar (lambda (id) (gethash id turns)) '("u1" "a1" "u2" "u3" "a2"))
                   '(0 0 0 1 1)))))

(ert-deftest test-opencode-changes/turns-without-idle-markers ()
  "Histories without idle markers start a turn at each prompt."
  (let ((turns (opencode-changes--turns
                '(((id . "u1") (type . "user")) ((id . "a1") (type . "assistant"))
                  ((id . "u2") (type . "user")) ((id . "a2") (type . "assistant"))))))
    (should (equal (mapcar (lambda (id) (gethash id turns)) '("u1" "a1" "u2" "a2"))
                   '(0 0 1 1)))))

;;; Steps

(ert-deftest test-opencode-changes/steps-follow-location-switches ()
  "Steps after a move run in the new directory; earlier ones in the first."
  (opencode-changes-test--with-tree (main work)
    (let* ((messages (list (opencode-changes-test--step "a1" :start "t0" :end "t0" :files t)
                           (opencode-changes-test--move main work)
                           (opencode-changes-test--step "a2" :start "t1" :end "t2" :files '("x.el"))))
           (steps (car (opencode-changes--session-steps
                        (opencode-changes-test--info "s1" work) messages))))
      (should (equal (mapcar #'opencode-changes-step-directory steps)
                     (list (directory-file-name main) (directory-file-name work)))))))

(ert-deftest test-opencode-changes/steps-record-tool-evidence ()
  "File tools name their files, shells their workdirs, and subagents their turn."
  (opencode-changes-test--with-tree (main)
    (let* ((parsed (opencode-changes--session-steps
                    (opencode-changes-test--info "s1" main)
                    (list '((id . "u1") (type . "user"))
                          (opencode-changes-test--step
                           "a1" :tools `(("edit" ((path . "a.el")) nil)
                                         ("patch" nil ((files . (((file . "/abs/b.el"))))))
                                         ("shell" ((command . "make") (workdir . "../other")) nil)
                                         ("subagent" nil ((sessionID . "child"))))))))
           (step (car (car parsed))))
      (should (opencode-changes-step-writes step))
      (should (equal (opencode-changes-step-paths step)
                     (list (expand-file-name "a.el" main) "/abs/b.el")))
      (should (equal (opencode-changes-step-workdirs step)
                     (list (expand-file-name "../other" main))))
      (should (equal (cdr parsed) '(("child" . 0)))))))

(ert-deftest test-opencode-changes/read-only-steps-do-not-write ()
  "Steps that only read cannot have made the changes their snapshot shows."
  (opencode-changes-test--with-tree (main)
    (let ((step (car (car (opencode-changes--session-steps
                           (opencode-changes-test--info "s1" main)
                           (list (opencode-changes-test--step
                                  "a1" :tools '(("read" ((path . "a.el")) nil)
                                                ("grep" ((pattern . "x")) nil)))))))))
      (should-not (opencode-changes-step-writes step)))))

;;; Roots

(ert-deftest test-opencode-changes/moved-session-changes-only-the-worktree ()
  "A session that moved to a worktree before editing leaves out the main one."
  (opencode-changes-test--with-tree (main work)
    (let* ((info (opencode-changes-test--info "s1" work))
           (messages (list (opencode-changes-test--step
                            "a1" :start "t0" :end "t0" :files t
                            :tools '(("shell" ((command . "git worktree add")) nil)))
                           (opencode-changes-test--move main work)
                           (opencode-changes-test--step
                            "a2" :start "t1" :end "t2" :files '("x.el")
                            :tools '(("edit" ((path . "x.el")) nil)))))
           (roots (opencode-changes-roots
                   (opencode-changes-tree-steps (list (cons info messages)) nil))))
      (should (equal (opencode-changes-test--root-summary roots)
                     `((,work ("x.el") ("s1")))))
      (should (equal (mapcar #'opencode-changes-step-start
                             (opencode-changes-root-steps (car roots)))
                     '("t1"))))))

(ert-deftest test-opencode-changes/unmoved-session-editing-a-worktree-by-path ()
  "Edits made by path in another worktree count, without the main worktree."
  (opencode-changes-test--with-tree (main work)
    (let* ((info (opencode-changes-test--info "s1" main))
           (messages (list (opencode-changes-test--step
                            "a1" :start "t0" :end "t0" :files t
                            :tools `(("write" ((path . ,(expand-file-name "new.el" work))) nil)))))
           (roots (opencode-changes-roots
                   (opencode-changes-tree-steps (list (cons info messages)) nil))))
      (should (equal (opencode-changes-test--root-summary roots)
                     `((,work ("new.el") ("s1"))))))))

(ert-deftest test-opencode-changes/changes-outside-the-session-are-ignored ()
  "Snapshot files from steps without writing tools came from someone else."
  (opencode-changes-test--with-tree (main)
    (let* ((info (opencode-changes-test--info "s1" main))
           (messages (list (opencode-changes-test--step "a1" :start "t0" :end "t1"
                                                      :files '("theirs.el"))
                           (opencode-changes-test--step "a2" :start "t1" :end "t2"
                                                      :files '("mine.el")
                                                      :tools '(("shell" ((command . "gen")) nil)))))
           (roots (opencode-changes-roots
                   (opencode-changes-tree-steps (list (cons info messages)) nil))))
      (should (equal (opencode-changes-test--root-summary roots)
                     `((,main ("mine.el") ("s1"))))))))

(ert-deftest test-opencode-changes/unrecorded-snapshot-keeps-the-directory ()
  "Without a recorded file list, the step's worktree stays a candidate."
  (opencode-changes-test--with-tree (main)
    (let* ((info (opencode-changes-test--info "s1" main))
           (messages (list (opencode-changes-test--step
                            "a1" :tools '(("shell" ((command . "gen")) nil)))))
           (roots (opencode-changes-roots
                   (opencode-changes-tree-steps (list (cons info messages)) nil))))
      (should (equal (opencode-changes-test--root-summary roots)
                     `((,main nil ("s1"))))))))

(ert-deftest test-opencode-changes/subagent-worktrees-stay-separate ()
  "Subagents working in their own worktrees each get their own root."
  (opencode-changes-test--with-tree (main wa wb)
    (let* ((tree (list (cons (opencode-changes-test--info "s1" main)
                             (list (opencode-changes-test--step
                                    "a1" :start "t0" :end "t0" :files t
                                    :tools '(("subagent" nil ((sessionID . "A")))
                                             ("subagent" nil ((sessionID . "B")))))))
                       (cons (opencode-changes-test--info "A" wa "s1")
                             (list (opencode-changes-test--move main wa)
                                   (opencode-changes-test--step
                                    "b1" :start "a0" :end "a1" :files '("a.el")
                                    :tools '(("edit" ((path . "a.el")) nil)))))
                       (cons (opencode-changes-test--info "B" wb "s1")
                             (list (opencode-changes-test--move main wb)
                                   (opencode-changes-test--step
                                    "c1" :start "b0" :end "b1" :files '("b.el")
                                    :tools '(("edit" ((path . "b.el")) nil)))))))
           (roots (opencode-changes-roots (opencode-changes-tree-steps tree nil))))
      (should (equal (opencode-changes-test--root-summary roots)
                     `((,wa ("a.el") ("A")) (,wb ("b.el") ("B"))))))))

(ert-deftest test-opencode-changes/nested-worktree-files-go-to-the-nested-root ()
  "Snapshot files inside a nested worktree belong to that worktree."
  (opencode-changes-test--with-tree (main)
    (let ((nested (file-name-as-directory (expand-file-name "wt/feature" main))))
      (make-directory (expand-file-name ".git" nested) t)
      (let* ((info (opencode-changes-test--info "s1" main))
             (messages (list (opencode-changes-test--step
                              "a1" :start "t0" :end "t1" :files '("wt/feature/x.el")
                              :tools '(("shell" ((command . "gen")) nil)))))
             (roots (opencode-changes-roots
                     (opencode-changes-tree-steps (list (cons info messages)) nil))))
        (should (equal (opencode-changes-test--root-summary roots)
                       `((,nested ("x.el") ("s1")))))))))

(ert-deftest test-opencode-changes/turn-includes-subagents-started-then ()
  "A turn's changes include the subagents it started, but not later turns."
  (opencode-changes-test--with-tree (main wa)
    (let* ((tree (list (cons (opencode-changes-test--info "s1" main)
                             (list '((id . "u1") (type . "user"))
                                   (opencode-changes-test--step
                                    "a1" :time 11 :start "t0" :end "t0" :files t
                                    :tools '(("subagent" nil ((sessionID . "A")))))
                                   '((id . "i1") (type . "idle"))
                                   '((id . "u2") (type . "user") (time . ((created . 20))))
                                   (opencode-changes-test--step
                                    "a2" :time 21 :start "t0" :end "t1" :files '("late.el")
                                    :tools '(("edit" ((path . "late.el")) nil)))))
                       (cons (opencode-changes-test--info "A" wa "s1")
                             (list (opencode-changes-test--move main wa)
                                   (opencode-changes-test--step
                                    "b1" :time 12 :start "a0" :end "a1" :files '("a.el")
                                    :tools '(("edit" ((path . "a.el")) nil))))))))
      (should (equal (opencode-changes-test--root-summary
                      (opencode-changes-roots (opencode-changes-tree-steps tree "u1")))
                     `((,wa ("a.el") ("A")))))
      (should (equal (opencode-changes-test--root-summary
                      (opencode-changes-roots (opencode-changes-tree-steps tree t)))
                     `((,main ("late.el") ("s1"))))))))

(ert-deftest test-opencode-changes/turn-leaves-out-other-turns-of-a-resumed-subagent ()
  "A subagent resumed in a later turn contributes only that turn's work."
  (opencode-changes-test--with-tree (main wa)
    (let* ((tree (list (cons (opencode-changes-test--info "s1" main)
                             (list `((id . "u1") (type . "user") (time . ((created . 10))))
                                   (opencode-changes-test--step
                                    "a1" :time 11 :start "t0" :end "t0" :files t
                                    :tools '(("subagent" nil ((sessionID . "A")))))
                                   '((id . "i1") (type . "idle") (time . ((created . 19))))
                                   `((id . "u2") (type . "user") (time . ((created . 20))))
                                   (opencode-changes-test--step
                                    "a2" :time 21 :start "t0" :end "t0" :files t
                                    :tools '(("subagent" nil ((sessionID . "A")))))))
                       (cons (opencode-changes-test--info "A" wa "s1")
                             (list (opencode-changes-test--step
                                    "b1" :time 12 :start "a0" :end "a1" :files '("first.el")
                                    :tools '(("edit" ((path . "first.el")) nil)))
                                   (opencode-changes-test--step
                                    "b2" :time 22 :start "a1" :end "a2" :files '("second.el")
                                    :tools '(("edit" ((path . "second.el")) nil))))))))
      (should (equal (opencode-changes-test--root-summary
                      (opencode-changes-roots (opencode-changes-tree-steps tree "u1")))
                     `((,wa ("first.el") ("A")))))
      (should (equal (opencode-changes-test--root-summary
                      (opencode-changes-roots (opencode-changes-tree-steps tree "u2")))
                     `((,wa ("second.el") ("A"))))))))

(ert-deftest test-opencode-changes/waiting-on-a-subagent-is-not-writing ()
  "A step that only starts a subagent does not claim the subagent's changes."
  (opencode-changes-test--with-tree (main)
    (let* ((tree (list (cons (opencode-changes-test--info "s1" main)
                             (list (opencode-changes-test--step
                                    "a1" :start "t0" :end "t1" :files '("x.el")
                                    :tools '(("subagent" nil ((sessionID . "A")))))))
                       (cons (opencode-changes-test--info "A" main "s1")
                             (list (opencode-changes-test--step
                                    "b1" :start "t0" :end "t1" :files '("x.el")
                                    :tools '(("edit" ((path . "x.el")) nil))))))))
      (should (equal (opencode-changes-test--root-summary
                      (opencode-changes-roots (opencode-changes-tree-steps tree nil)))
                     `((,main ("x.el") ("A"))))))))

(ert-deftest test-opencode-changes/removed-worktree-keeps-its-files ()
  "A worktree removed since the session ran still lists its changes."
  (opencode-changes-test--with-tree (main)
    (let* ((gone (expand-file-name "gone/" tmp))
           (info (opencode-changes-test--info "s1" gone))
           (messages (list (opencode-changes-test--move main gone)
                           (opencode-changes-test--step
                            "a1" :start "t0" :end "t1" :files '("x.el")
                            :tools '(("edit" ((path . "x.el")) nil)))))
           (roots (opencode-changes-roots
                   (opencode-changes-tree-steps (list (cons info messages)) nil))))
      (should (equal (opencode-changes-test--root-summary roots)
                     `((,gone ("x.el") ("s1"))))))))

(ert-deftest test-opencode-changes/snapshot-range-spans-the-root-steps ()
  "A root's snapshot range runs from its first step to its last."
  (let ((root (opencode-changes-root-create
               :steps (list (opencode-changes-step-create :time 2 :start "t1" :end "t2")
                            (opencode-changes-step-create :time 1 :start "t0" :end "t1")))))
    (should (equal (opencode-changes-snapshot-range root) '("t0" . "t2")))))

(provide 'emacs-opencode-changes-test)

;;; emacs-opencode-changes-test.el ends here
