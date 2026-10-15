;;; emacs-opencode-magit-test.el --- Tests for the Magit integration  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-opencode-magit)

(defmacro opencode-magit-test--with-tree (names &rest body)
  "Bind NAMES to Git worktrees under a fresh temporary directory around BODY.
Each worktree is an empty directory holding a `.git' directory.  The
variable `tmp' holds the parent directory."
  (declare (indent 1))
  `(let* ((tmp (file-name-as-directory (file-truename (make-temp-file "oc-magit" t))))
          ,@(mapcar (lambda (name)
                      `(,name (file-name-as-directory (expand-file-name ,(symbol-name name) tmp))))
                    names))
     (unwind-protect
         (progn
           ,@(mapcar (lambda (name) `(make-directory (expand-file-name ".git" ,name) t)) names)
           ,@body)
       (delete-directory tmp t))))

(defun opencode-magit-test--info (id directory &optional parent)
  "Return session info for ID located in DIRECTORY with PARENT."
  `((id . ,id) (title . ,(concat "Session " id))
    (location . ((directory . ,(directory-file-name directory))))
    ,@(when parent `((parentID . ,parent)))))

(defun opencode-magit-test--step (id &rest props)
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

(defun opencode-magit-test--move (from to)
  "Return a location-switched message from directory FROM to TO."
  `((id . ,(concat "mv-" to)) (type . "location-switched")
    (location . ((directory . ,(directory-file-name to))))
    (previous . ((location . ((directory . ,(directory-file-name from))))))))

(defun opencode-magit-test--root-summary (roots)
  "Return ROOTS as a list of (DIRECTORY FILES SESSIONS)."
  (mapcar (lambda (root)
            (list (opencode-magit-root-directory root)
                  (opencode-magit-root-files root)
                  (opencode-magit-root-sessions root)))
          roots))

;;; Turns

(ert-deftest test-opencode-magit/turns-end-at-idle-markers ()
  "Prompts steered in before the session went idle share a turn."
  (let ((turns (opencode-magit--turns
                '(((id . "u1") (type . "user")) ((id . "a1") (type . "assistant"))
                  ((id . "u2") (type . "user")) ((id . "i1") (type . "idle"))
                  ((id . "u3") (type . "user")) ((id . "a2") (type . "assistant"))))))
    (should (equal (mapcar (lambda (id) (gethash id turns)) '("u1" "a1" "u2" "u3" "a2"))
                   '(0 0 0 1 1)))))

(ert-deftest test-opencode-magit/turns-without-idle-markers ()
  "Histories without idle markers start a turn at each prompt."
  (let ((turns (opencode-magit--turns
                '(((id . "u1") (type . "user")) ((id . "a1") (type . "assistant"))
                  ((id . "u2") (type . "user")) ((id . "a2") (type . "assistant"))))))
    (should (equal (mapcar (lambda (id) (gethash id turns)) '("u1" "a1" "u2" "a2"))
                   '(0 0 1 1)))))

;;; Steps

(ert-deftest test-opencode-magit/steps-follow-location-switches ()
  "Steps after a move run in the new directory; earlier ones in the first."
  (opencode-magit-test--with-tree (main work)
    (let* ((messages (list (opencode-magit-test--step "a1" :start "t0" :end "t0" :files t)
                           (opencode-magit-test--move main work)
                           (opencode-magit-test--step "a2" :start "t1" :end "t2" :files '("x.el"))))
           (steps (car (opencode-magit--session-steps
                        (opencode-magit-test--info "s1" work) messages))))
      (should (equal (mapcar #'opencode-magit--step-directory steps)
                     (list (directory-file-name main) (directory-file-name work)))))))

(ert-deftest test-opencode-magit/steps-record-tool-evidence ()
  "File tools name their files, shells their workdirs, and subagents their turn."
  (opencode-magit-test--with-tree (main)
    (let* ((parsed (opencode-magit--session-steps
                    (opencode-magit-test--info "s1" main)
                    (list '((id . "u1") (type . "user"))
                          (opencode-magit-test--step
                           "a1" :tools `(("edit" ((path . "a.el")) nil)
                                         ("patch" nil ((files . (((file . "/abs/b.el"))))))
                                         ("shell" ((command . "make") (workdir . "../other")) nil)
                                         ("subagent" nil ((sessionID . "child"))))))))
           (step (car (car parsed))))
      (should (opencode-magit--step-writes step))
      (should (equal (opencode-magit--step-paths step)
                     (list (expand-file-name "a.el" main) "/abs/b.el")))
      (should (equal (opencode-magit--step-workdirs step)
                     (list (expand-file-name "../other" main))))
      (should (equal (cdr parsed) '(("child" . 0)))))))

(ert-deftest test-opencode-magit/read-only-steps-do-not-write ()
  "Steps that only read cannot have made the changes their snapshot shows."
  (opencode-magit-test--with-tree (main)
    (let ((step (car (car (opencode-magit--session-steps
                           (opencode-magit-test--info "s1" main)
                           (list (opencode-magit-test--step
                                  "a1" :tools '(("read" ((path . "a.el")) nil)
                                                ("grep" ((pattern . "x")) nil)))))))))
      (should-not (opencode-magit--step-writes step)))))

;;; Roots

(ert-deftest test-opencode-magit/moved-session-changes-only-the-worktree ()
  "A session that moved to a worktree before editing leaves out the main one."
  (opencode-magit-test--with-tree (main work)
    (let* ((info (opencode-magit-test--info "s1" work))
           (messages (list (opencode-magit-test--step
                            "a1" :start "t0" :end "t0" :files t
                            :tools '(("shell" ((command . "git worktree add")) nil)))
                           (opencode-magit-test--move main work)
                           (opencode-magit-test--step
                            "a2" :start "t1" :end "t2" :files '("x.el")
                            :tools '(("edit" ((path . "x.el")) nil)))))
           (roots (opencode-magit--roots
                   (opencode-magit--tree-steps (list (cons info messages)) nil))))
      (should (equal (opencode-magit-test--root-summary roots)
                     `((,work ("x.el") ("s1")))))
      (should (equal (mapcar #'opencode-magit--step-start
                             (opencode-magit-root-steps (car roots)))
                     '("t1"))))))

(ert-deftest test-opencode-magit/unmoved-session-editing-a-worktree-by-path ()
  "Edits made by path in another worktree count, without the main worktree."
  (opencode-magit-test--with-tree (main work)
    (let* ((info (opencode-magit-test--info "s1" main))
           (messages (list (opencode-magit-test--step
                            "a1" :start "t0" :end "t0" :files t
                            :tools `(("write" ((path . ,(expand-file-name "new.el" work))) nil)))))
           (roots (opencode-magit--roots
                   (opencode-magit--tree-steps (list (cons info messages)) nil))))
      (should (equal (opencode-magit-test--root-summary roots)
                     `((,work ("new.el") ("s1"))))))))

(ert-deftest test-opencode-magit/changes-outside-the-session-are-ignored ()
  "Snapshot files from steps without writing tools came from someone else."
  (opencode-magit-test--with-tree (main)
    (let* ((info (opencode-magit-test--info "s1" main))
           (messages (list (opencode-magit-test--step "a1" :start "t0" :end "t1"
                                                      :files '("theirs.el"))
                           (opencode-magit-test--step "a2" :start "t1" :end "t2"
                                                      :files '("mine.el")
                                                      :tools '(("shell" ((command . "gen")) nil)))))
           (roots (opencode-magit--roots
                   (opencode-magit--tree-steps (list (cons info messages)) nil))))
      (should (equal (opencode-magit-test--root-summary roots)
                     `((,main ("mine.el") ("s1"))))))))

(ert-deftest test-opencode-magit/unrecorded-snapshot-keeps-the-directory ()
  "Without a recorded file list, the step's worktree stays a candidate."
  (opencode-magit-test--with-tree (main)
    (let* ((info (opencode-magit-test--info "s1" main))
           (messages (list (opencode-magit-test--step
                            "a1" :tools '(("shell" ((command . "gen")) nil)))))
           (roots (opencode-magit--roots
                   (opencode-magit--tree-steps (list (cons info messages)) nil))))
      (should (equal (opencode-magit-test--root-summary roots)
                     `((,main nil ("s1"))))))))

(ert-deftest test-opencode-magit/subagent-worktrees-stay-separate ()
  "Subagents working in their own worktrees each get their own root."
  (opencode-magit-test--with-tree (main wa wb)
    (let* ((tree (list (cons (opencode-magit-test--info "s1" main)
                             (list (opencode-magit-test--step
                                    "a1" :start "t0" :end "t0" :files t
                                    :tools '(("subagent" nil ((sessionID . "A")))
                                             ("subagent" nil ((sessionID . "B")))))))
                       (cons (opencode-magit-test--info "A" wa "s1")
                             (list (opencode-magit-test--move main wa)
                                   (opencode-magit-test--step
                                    "b1" :start "a0" :end "a1" :files '("a.el")
                                    :tools '(("edit" ((path . "a.el")) nil)))))
                       (cons (opencode-magit-test--info "B" wb "s1")
                             (list (opencode-magit-test--move main wb)
                                   (opencode-magit-test--step
                                    "c1" :start "b0" :end "b1" :files '("b.el")
                                    :tools '(("edit" ((path . "b.el")) nil)))))))
           (roots (opencode-magit--roots (opencode-magit--tree-steps tree nil))))
      (should (equal (opencode-magit-test--root-summary roots)
                     `((,wa ("a.el") ("A")) (,wb ("b.el") ("B"))))))))

(ert-deftest test-opencode-magit/nested-worktree-files-go-to-the-nested-root ()
  "Snapshot files inside a nested worktree belong to that worktree."
  (opencode-magit-test--with-tree (main)
    (let ((nested (file-name-as-directory (expand-file-name "wt/feature" main))))
      (make-directory (expand-file-name ".git" nested) t)
      (let* ((info (opencode-magit-test--info "s1" main))
             (messages (list (opencode-magit-test--step
                              "a1" :start "t0" :end "t1" :files '("wt/feature/x.el")
                              :tools '(("shell" ((command . "gen")) nil)))))
             (roots (opencode-magit--roots
                     (opencode-magit--tree-steps (list (cons info messages)) nil))))
        (should (equal (opencode-magit-test--root-summary roots)
                       `((,nested ("x.el") ("s1")))))))))

(ert-deftest test-opencode-magit/turn-includes-subagents-started-then ()
  "A turn's changes include the subagents it started, but not later turns."
  (opencode-magit-test--with-tree (main wa)
    (let* ((tree (list (cons (opencode-magit-test--info "s1" main)
                             (list '((id . "u1") (type . "user"))
                                   (opencode-magit-test--step
                                    "a1" :time 11 :start "t0" :end "t0" :files t
                                    :tools '(("subagent" nil ((sessionID . "A")))))
                                   '((id . "i1") (type . "idle"))
                                   '((id . "u2") (type . "user") (time . ((created . 20))))
                                   (opencode-magit-test--step
                                    "a2" :time 21 :start "t0" :end "t1" :files '("late.el")
                                    :tools '(("edit" ((path . "late.el")) nil)))))
                       (cons (opencode-magit-test--info "A" wa "s1")
                             (list (opencode-magit-test--move main wa)
                                   (opencode-magit-test--step
                                    "b1" :time 12 :start "a0" :end "a1" :files '("a.el")
                                    :tools '(("edit" ((path . "a.el")) nil))))))))
      (should (equal (opencode-magit-test--root-summary
                      (opencode-magit--roots (opencode-magit--tree-steps tree "u1")))
                     `((,wa ("a.el") ("A")))))
      (should (equal (opencode-magit-test--root-summary
                      (opencode-magit--roots (opencode-magit--tree-steps tree t)))
                     `((,main ("late.el") ("s1"))))))))

(ert-deftest test-opencode-magit/turn-leaves-out-other-turns-of-a-resumed-subagent ()
  "A subagent resumed in a later turn contributes only that turn's work."
  (opencode-magit-test--with-tree (main wa)
    (let* ((tree (list (cons (opencode-magit-test--info "s1" main)
                             (list `((id . "u1") (type . "user") (time . ((created . 10))))
                                   (opencode-magit-test--step
                                    "a1" :time 11 :start "t0" :end "t0" :files t
                                    :tools '(("subagent" nil ((sessionID . "A")))))
                                   '((id . "i1") (type . "idle") (time . ((created . 19))))
                                   `((id . "u2") (type . "user") (time . ((created . 20))))
                                   (opencode-magit-test--step
                                    "a2" :time 21 :start "t0" :end "t0" :files t
                                    :tools '(("subagent" nil ((sessionID . "A")))))))
                       (cons (opencode-magit-test--info "A" wa "s1")
                             (list (opencode-magit-test--step
                                    "b1" :time 12 :start "a0" :end "a1" :files '("first.el")
                                    :tools '(("edit" ((path . "first.el")) nil)))
                                   (opencode-magit-test--step
                                    "b2" :time 22 :start "a1" :end "a2" :files '("second.el")
                                    :tools '(("edit" ((path . "second.el")) nil))))))))
      (should (equal (opencode-magit-test--root-summary
                      (opencode-magit--roots (opencode-magit--tree-steps tree "u1")))
                     `((,wa ("first.el") ("A")))))
      (should (equal (opencode-magit-test--root-summary
                      (opencode-magit--roots (opencode-magit--tree-steps tree "u2")))
                     `((,wa ("second.el") ("A"))))))))

(ert-deftest test-opencode-magit/waiting-on-a-subagent-is-not-writing ()
  "A step that only starts a subagent does not claim the subagent's changes."
  (opencode-magit-test--with-tree (main)
    (let* ((tree (list (cons (opencode-magit-test--info "s1" main)
                             (list (opencode-magit-test--step
                                    "a1" :start "t0" :end "t1" :files '("x.el")
                                    :tools '(("subagent" nil ((sessionID . "A")))))))
                       (cons (opencode-magit-test--info "A" main "s1")
                             (list (opencode-magit-test--step
                                    "b1" :start "t0" :end "t1" :files '("x.el")
                                    :tools '(("edit" ((path . "x.el")) nil))))))))
      (should (equal (opencode-magit-test--root-summary
                      (opencode-magit--roots (opencode-magit--tree-steps tree nil)))
                     `((,main ("x.el") ("A"))))))))

(ert-deftest test-opencode-magit/removed-worktree-keeps-its-files ()
  "A worktree removed since the session ran still lists its changes."
  (opencode-magit-test--with-tree (main)
    (let* ((gone (expand-file-name "gone/" tmp))
           (info (opencode-magit-test--info "s1" gone))
           (messages (list (opencode-magit-test--move main gone)
                           (opencode-magit-test--step
                            "a1" :start "t0" :end "t1" :files '("x.el")
                            :tools '(("edit" ((path . "x.el")) nil)))))
           (roots (opencode-magit--roots
                   (opencode-magit--tree-steps (list (cons info messages)) nil))))
      (should (equal (opencode-magit-test--root-summary roots)
                     `((,gone ("x.el") ("s1"))))))))

(ert-deftest test-opencode-magit/snapshot-range-spans-the-root-steps ()
  "A root's snapshot range runs from its first step to its last."
  (let ((root (opencode-magit-root-create
               :steps (list (opencode-magit--step-create :time 2 :start "t1" :end "t2")
                            (opencode-magit--step-create :time 1 :start "t0" :end "t1")))))
    (should (equal (opencode-magit--snapshot-range root) '("t0" . "t2")))))

;;; Git

(defun opencode-magit-test--git (directory &rest args)
  "Run git with ARGS in DIRECTORY and return its trimmed output."
  (with-temp-buffer
    (let ((default-directory directory))
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (error "git %s failed: %s" args (buffer-string))))
    (string-trim (buffer-string))))

(ert-deftest test-opencode-magit/finds-the-snapshot-repository ()
  "The snapshot repository is the one for the worktree that has the tree."
  (let* ((data (make-temp-file "oc-data" t))
         (worktree (file-truename (make-temp-file "oc-wt" t)))
         (hash (secure-hash 'sha1 worktree))
         (empty (expand-file-name (concat "opencode/snapshot/other/" hash) data))
         (repo (expand-file-name (concat "opencode/snapshot/project/" hash) data)))
    (unwind-protect
        (let ((process-environment (cons (concat "XDG_DATA_HOME=" data) process-environment)))
          (make-directory empty t)
          (make-directory repo t)
          (opencode-magit-test--git repo "init" "--quiet" "--bare")
          (let ((tree (opencode-magit-test--git repo "mktree")))
            (should (equal (opencode-magit--snapshot-git-dir worktree tree) repo))
            (should-not (opencode-magit--snapshot-git-dir worktree "0123456789abcdef"))))
      (delete-directory data t)
      (delete-directory worktree t))))

(ert-deftest test-opencode-magit/base-follows-magit ()
  "The base is the upstream, else the main branch's upstream, else the main branch.
An upstream that is the branch's own remote copy is not a base."
  (cl-letf (((symbol-function 'magit-main-branch) (lambda () "main"))
            ((symbol-function 'magit-list-remotes) (lambda () '("origin")))
            ((symbol-function 'magit-list-local-branch-names) #'ignore))
    (cl-letf (((symbol-function 'magit-get-current-branch) (lambda () "topic"))
              ((symbol-function 'magit-get-upstream-branch)
               (lambda (&optional branch) (if branch "origin/main" "origin/feature"))))
      (should (equal (opencode-magit--base) "origin/feature")))
    (cl-letf (((symbol-function 'magit-get-upstream-branch)
               (lambda (&optional branch) (and branch "origin/main"))))
      (should (equal (opencode-magit--base) "origin/main")))
    (cl-letf (((symbol-function 'magit-get-upstream-branch) #'ignore))
      (should (equal (opencode-magit--base) "main")))
    (cl-letf (((symbol-function 'magit-get-current-branch) (lambda () "feature"))
              ((symbol-function 'magit-get-upstream-branch)
               (lambda (&optional branch) (if branch "origin/main" "origin/feature"))))
      (should (equal (opencode-magit--base) "origin/main")))))

(defun opencode-magit-test--tree (git-dir directory files)
  "Write a tree of FILES into GIT-DIR and return its ID.
FILES is an alist of (NAME . CONTENT).  DIRECTORY is scratch space."
  (let ((file (expand-file-name "blob" directory))
        entries)
    (dolist (entry files)
      (write-region (cdr entry) nil file)
      (push (format "100644 blob %s\t%s\n"
                    (opencode-magit-test--git directory (concat "--git-dir=" git-dir)
                                              "hash-object" "-w" file)
                    (car entry))
            entries))
    (with-temp-buffer
      (insert (apply #'concat (nreverse entries)))
      (let ((default-directory directory))
        (call-process-region (point-min) (point-max) "git" t t nil
                             (concat "--git-dir=" git-dir) "mktree"))
      (string-trim (buffer-string)))))

(defmacro opencode-magit-test--with-snapshots (&rest body)
  "Run BODY with a Git worktree and a separate snapshot repository.
`worktree' holds a.txt containing \"one\" and an untracked notes.txt;
`snapshots' is the snapshot repository and `tree' builds trees in it."
  (declare (indent 0))
  `(let* ((tmp (file-name-as-directory (file-truename (make-temp-file "oc-snap" t))))
          (worktree (expand-file-name "wt/" tmp))
          (snapshots (expand-file-name "snapshots" tmp))
          (magit-display-buffer-function #'display-buffer)
          (buffers nil))
     (cl-flet ((tree (&rest files) (opencode-magit-test--tree snapshots tmp files))
               (write (file content) (write-region content nil (expand-file-name file worktree))))
       (unwind-protect
           (progn
             (make-directory worktree)
             (opencode-magit-test--git worktree "init" "--quiet")
             (write "a.txt" "one\n")
             (write "b.txt" "bee\n")
             (opencode-magit-test--git worktree "add" "a.txt" "b.txt")
             (opencode-magit-test--git worktree "-c" "user.name=t" "-c" "user.email=t"
                                       "commit" "--quiet" "-m" "init")
             (write "notes.txt" "untracked\n")
             (opencode-magit-test--git tmp "init" "--quiet" "--bare" snapshots)
             (cl-letf (((symbol-function 'opencode-magit--snapshot-git-dir)
                        (lambda (_directory _tree) snapshots)))
               ,@body))
         (dolist (buffer buffers) (when (buffer-live-p buffer) (kill-buffer buffer)))
         (delete-directory tmp t)))))

(defun opencode-magit-test--root (worktree from to &optional files)
  "Return a root in WORKTREE whose session changed FILES between FROM and TO."
  (opencode-magit-root-create
   :directory worktree :files (or files '("a.txt"))
   :steps (list (opencode-magit--step-create :time 1 :start from :end to))))

(ert-deftest test-opencode-magit/session-diff-follows-the-working-tree ()
  "The session diff compares the starting snapshot with the working tree."
  (opencode-magit-test--with-snapshots
    (let* ((start (tree '("a.txt" . "one\n") '("b.txt" . "bee\n")
                        '("notes.txt" . "untracked\n")))
           (root (opencode-magit-test--root worktree start start)))
      (write "a.txt" "two\n")
      (let ((buffer (opencode-magit--session-diff root)))
        (push buffer buffers)
        (with-current-buffer buffer
          (should (derived-mode-p 'magit-diff-mode))
          (should (string-match-p "-one\n\\+two" (buffer-string)))
          (should-not (string-match-p "notes.txt" (buffer-string)))
          (should-not magit-buffer-diff-files)
          (write "a.txt" "three\n")
          (magit-refresh-buffer)
          (should (string-match-p "\\+three" (buffer-string))))))))

(ert-deftest test-opencode-magit/session-diff-reuses-its-revision ()
  "Opening the session diff again names the same revision, so reviews resume."
  (opencode-magit-test--with-snapshots
    (let* ((start (tree '("a.txt" . "one\n") '("b.txt" . "bee\n")))
           (root (opencode-magit-test--root worktree start start)))
      (write "a.txt" "two\n")
      (let ((first (with-current-buffer (opencode-magit--session-diff root)
                     (push (current-buffer) buffers)
                     magit-buffer-diff-range)))
        (kill-buffer (car buffers))
        (with-current-buffer (opencode-magit--session-diff root)
          (push (current-buffer) buffers)
          (should (equal magit-buffer-diff-range first)))))))

(ert-deftest test-opencode-magit/session-diff-filters-others-changes ()
  "When other files changed too, the session diff shows only the session's."
  (opencode-magit-test--with-snapshots
    (let* ((start (tree '("a.txt" . "one\n") '("b.txt" . "bee\n")))
           (root (opencode-magit-test--root worktree start start)))
      (write "a.txt" "two\n")
      (write "b.txt" "someone else\n")
      (with-current-buffer (opencode-magit--session-diff root)
        (push (current-buffer) buffers)
        (should (equal magit-buffer-diff-files '("a.txt")))))))

(ert-deftest test-opencode-magit/turn-diff-compares-two-snapshots ()
  "A turn diff compares the snapshots taken around the turn."
  (opencode-magit-test--with-snapshots
    (let* ((from (tree '("a.txt" . "one\n") '("b.txt" . "bee\n")))
           (to (tree '("a.txt" . "two\n") '("b.txt" . "bee\n")))
           (root (opencode-magit-test--root worktree from to)))
      (write "a.txt" "later\n")
      (with-current-buffer (opencode-magit--turn-diff root)
        (push (current-buffer) buffers)
        (should (derived-mode-p 'magit-diff-mode))
        (should (string-match-p "-one\n\\+two" (buffer-string)))
        (should-not magit-buffer-diff-files)))))

(ert-deftest test-opencode-magit/session-diff-shows-deleted-files ()
  "Files the session removed from Git still show as deleted."
  (opencode-magit-test--with-snapshots
    (let* ((start (tree '("a.txt" . "one\n") '("b.txt" . "bee\n")))
           (root (opencode-magit-test--root worktree start start '("b.txt"))))
      (opencode-magit-test--git worktree "rm" "--quiet" "b.txt")
      (opencode-magit-test--git worktree "-c" "user.name=t" "-c" "user.email=t"
                                "commit" "--quiet" "-m" "remove b")
      (with-current-buffer (opencode-magit--session-diff root)
        (push (current-buffer) buffers)
        (should (string-match-p "deleted +b.txt" (buffer-string)))))))

(ert-deftest test-opencode-magit/snapshot-commits-ignore-signing ()
  "Snapshot commits are never signed, so the same tree gives the same commit."
  (opencode-magit-test--with-snapshots
    (let ((start (tree '("a.txt" . "one\n") '("b.txt" . "bee\n"))))
      (opencode-magit-test--git worktree "config" "commit.gpgSign" "true")
      (opencode-magit-test--git worktree "config" "gpg.program" "false")
      (let ((default-directory worktree))
        (should (equal (opencode-magit--snapshot-commit snapshots start)
                       (opencode-magit--snapshot-commit snapshots start)))))))

(ert-deftest test-opencode-magit/turn-diff-filters-others-changes ()
  "A turn diff shows only the session's files when others changed too."
  (opencode-magit-test--with-snapshots
    (let* ((from (tree '("a.txt" . "one\n") '("b.txt" . "bee\n")))
           (to (tree '("a.txt" . "two\n") '("b.txt" . "someone else\n")))
           (root (opencode-magit-test--root worktree from to)))
      (with-current-buffer (opencode-magit--turn-diff root)
        (push (current-buffer) buffers)
        (should (equal magit-buffer-diff-files '("a.txt")))))))

(provide 'emacs-opencode-magit-test)

;;; emacs-opencode-magit-test.el ends here
