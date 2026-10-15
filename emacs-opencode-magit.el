;;; emacs-opencode-magit.el --- Magit views of OpenCode session changes  -*- lexical-binding: t; -*-

;; Optional: load with (require 'emacs-opencode-magit).  Requires Magit.

(require 'cl-lib)
(require 'subr-x)
(require 'magit)
(require 'emacs-opencode-client)
(require 'emacs-opencode-session)
(require 'emacs-opencode-session-vars)
(require 'emacs-opencode-message)
(require 'emacs-opencode-session-mode)

(declare-function opencode--ensure-session-buffer "emacs-opencode"
                  (session connection &optional on-ready))

(defvar-local opencode-magit--overview nil
  "Plist describing the session an overview buffer shows.
Keys are :connection, :session-id, :title, :roots, and :tree.")

(defconst opencode-magit--read-only-tools
  '("read" "glob" "grep" "list" "webfetch" "websearch" "skill" "question"
    "todowrite" "todoread" "subagent" "task")
  "Tools that cannot change files themselves.
Changes recorded during a step that called only these tools were made by
another session, a subagent, or the user.  Subagents record their own
changes.")

(defconst opencode-magit--file-tools '("edit" "write" "patch" "apply_patch")
  "Tools whose input and metadata name the files they change.")

(defconst opencode-magit--children-limit 1000
  "Maximum number of subagent sessions fetched for each session.")

;;; Session analysis

(cl-defstruct (opencode-magit--step (:constructor opencode-magit--step-create))
  "An assistant step, reduced to where it ran and what it changed.
START and END are snapshot trees.  FILES lists the changed files relative
to the worktree, and FILES-KNOWN is nil when OpenCode recorded no list.
WRITES is non-nil when the step called a tool that can change files.
PATHS are files named by file tools and WORKDIRS are other directories
shell commands ran in."
  session-id turn time directory start end files files-known writes paths workdirs)

(cl-defstruct (opencode-magit-root (:constructor opencode-magit-root-create))
  "A Git worktree changed by a session tree.
FILES are relative to DIRECTORY, SESSIONS are the IDs of the sessions that
changed it, and STEPS are their steps that ran there."
  directory files sessions steps)

(defun opencode-magit--type (message)
  "Return the type of server MESSAGE."
  (alist-get 'type message))

(defun opencode-magit--turns (messages)
  "Return a hash table mapping each of MESSAGES' IDs to a turn number.
A turn ends when the session goes idle, so prompts steered into a busy
session share its turn.  Histories without idle markers start a turn at
each prompt instead."
  (let ((turns (make-hash-table :test #'equal))
        (legacy (not (cl-find "idle" messages :key #'opencode-magit--type :test #'equal)))
        (turn 0)
        prompted
        ended)
    (dolist (message messages turns)
      (let ((type (opencode-magit--type message)))
        (when (or ended (and legacy prompted (equal type "user")))
          (cl-incf turn)
          (setq ended nil))
        (when (equal type "user")
          (setq prompted t))
        (puthash (alist-get 'id message) turn turns)
        (when (equal type "idle")
          (setq ended t))))))

(defun opencode-magit--initial-directory (info messages)
  "Return the directory session INFO started in, given its MESSAGES."
  (or (when-let* ((switch (cl-find "location-switched" messages
                                   :key #'opencode-magit--type :test #'equal)))
        (alist-get 'directory (alist-get 'location (alist-get 'previous switch))))
      (alist-get 'directory (alist-get 'location info))))

(defun opencode-magit--expand (path directory)
  "Expand PATH against DIRECTORY, or return nil when PATH is not a name."
  (when (and (stringp path) (not (string-empty-p path)))
    (expand-file-name path (file-name-as-directory directory))))

(defun opencode-magit--session-steps (info messages)
  "Return the steps of session INFO, given its MESSAGES.
Returns (STEPS . SPAWNED), where SPAWNED maps the IDs of subagent
sessions started by the steps to the turn that started them."
  (let ((session-id (alist-get 'id info))
        (directory (opencode-magit--initial-directory info messages))
        (turns (opencode-magit--turns messages))
        steps
        spawned)
    (dolist (message messages)
      (pcase (opencode-magit--type message)
        ("location-switched"
         (setq directory (or (alist-get 'directory (alist-get 'location message))
                             directory)))
        ("assistant"
         (let ((turn (gethash (alist-get 'id message) turns))
               (snapshot (alist-get 'snapshot message))
               writes paths workdirs)
           (dolist (item (alist-get 'content message))
             (when (equal (alist-get 'type item) "tool")
               (let* ((name (alist-get 'name item))
                      (state (alist-get 'state item))
                      (input (and (listp (alist-get 'input state)) (alist-get 'input state)))
                      (metadata (alist-get 'metadata state)))
                 (unless (member name opencode-magit--read-only-tools)
                   (setq writes t))
                 (when (member name opencode-magit--file-tools)
                   (dolist (path (cons (or (alist-get 'path input) (alist-get 'filePath input))
                                       (mapcar (lambda (file) (alist-get 'file file))
                                               (alist-get 'files metadata))))
                     (when-let* ((path (opencode-magit--expand path directory)))
                       (cl-pushnew path paths :test #'equal))))
                 (when-let* ((workdir (opencode-magit--expand (alist-get 'workdir input)
                                                              directory))
                             ((not (file-equal-p workdir directory))))
                   (cl-pushnew workdir workdirs :test #'equal))
                 (when-let* (((member name '("subagent" "task")))
                             (child (alist-get 'sessionID metadata)))
                   (push (cons child turn) spawned)))))
           (push (opencode-magit--step-create
                  :session-id session-id
                  :turn turn
                  :time (alist-get 'created (alist-get 'time message))
                  :directory directory
                  :start (alist-get 'start snapshot)
                  :end (alist-get 'end snapshot)
                  :files (alist-get 'files snapshot)
                  :files-known (and (assq 'files snapshot) t)
                  :writes writes
                  :paths (nreverse paths)
                  :workdirs (nreverse workdirs))
                 steps)))))
    (cons (nreverse steps) (nreverse spawned))))

(defun opencode-magit--tree-steps (tree &optional turn)
  "Return the steps of the sessions in TREE.
TREE is a list of (INFO . MESSAGES) whose first entry is the session the
user asked about; the rest are its subagents.  When TURN is non-nil, keep
only that session's steps from one turn, together with the steps that
subagents it started or resumed during that turn ran before the next
turn.  TURN is the ID of a message in the turn, or t for the latest turn."
  (let ((parsed (mapcar (lambda (entry)
                          (cons (alist-get 'id (car entry))
                                (opencode-magit--session-steps (car entry) (cdr entry))))
                        tree)))
    (if (not turn)
        (apply #'append (mapcar #'cadr parsed))
      (let* ((root-id (caar parsed))
             (messages (cdar tree))
             (turns (opencode-magit--turns messages))
             (index (or (and (stringp turn) (gethash turn turns))
                        (apply #'max 0 (hash-table-values turns))))
             (turn-start (lambda (index)
                           (cl-loop for message in messages
                                    for time = (alist-get 'created (alist-get 'time message))
                                    when (and time (equal (gethash (alist-get 'id message) turns)
                                                          index))
                                    minimize time)))
             (start (funcall turn-start index))
             (end (funcall turn-start (1+ index)))
             (in-turn (lambda (step)
                        (let ((time (opencode-magit--step-time step)))
                          (or (null time)
                              (and (or (null start) (>= time start))
                                   (or (null end) (< time end)))))))
             (children (make-hash-table :test #'equal))
             (included (delete-dups
                        (mapcar #'car (cl-remove-if-not (lambda (spawn) (equal (cdr spawn) index))
                                                        (cddar parsed)))))
             (queue (copy-sequence included)))
        (dolist (entry tree)
          (when-let* ((parent (alist-get 'parentID (car entry))))
            (push (alist-get 'id (car entry)) (gethash parent children))))
        (while queue
          (dolist (child (gethash (pop queue) children))
            (unless (member child included)
              (push child included)
              (push child queue))))
        (apply #'append
               (cl-remove-if-not (lambda (step) (equal (opencode-magit--step-turn step) index))
                                 (cadr (assoc root-id parsed)))
               (mapcar (lambda (id) (cl-remove-if-not in-turn (cadr (assoc id parsed))))
                       included))))))

(defun opencode-magit--toplevel (directory)
  "Return the Git worktree containing DIRECTORY, or nil."
  (when-let* ((top (locate-dominating-file directory ".git")))
    (file-name-as-directory (file-truename top))))

(defun opencode-magit--roots (steps)
  "Return the worktrees STEPS changed as a sorted list of roots.
A step's snapshot shows every change made in its worktree while it ran,
so it only counts when the step called a tool that can change files.
File tools and shell working directories show changes in other
worktrees, which snapshots do not cover."
  (let ((roots (make-hash-table :test #'equal))
        (toplevels (make-hash-table :test #'equal)))
    (cl-labels
        ((toplevel (directory)
           (let ((directory (file-name-as-directory directory)))
             (with-memoization (gethash directory toplevels)
               (if (file-directory-p directory)
                   (opencode-magit--toplevel directory)
                 directory))))
         (root (directory session-id)
           (let ((root (with-memoization (gethash directory roots)
                         (opencode-magit-root-create :directory directory))))
             (cl-pushnew session-id (opencode-magit-root-sessions root) :test #'equal)
             root))
         (add-file (path session-id)
           (let ((parent (file-name-directory path)))
             (when-let* (((file-directory-p parent))
                         (top (toplevel parent)))
               (cl-pushnew (file-relative-name
                            (expand-file-name (file-name-nondirectory path)
                                              (file-truename parent))
                            top)
                           (opencode-magit-root-files (root top session-id))
                           :test #'equal)))))
      (dolist (step steps)
        (let ((session-id (opencode-magit--step-session-id step))
              (top (toplevel (opencode-magit--step-directory step))))
          (when (opencode-magit--step-writes step)
            (when top
              (let ((files (opencode-magit--step-files step)))
                (cond
                 ((not (opencode-magit--step-files-known step))
                  (root top session-id))
                 ((file-directory-p top)
                  (dolist (file files)
                    (add-file (expand-file-name file top) session-id)))
                 (t
                  ;; A removed worktree can no longer be searched for nested ones.
                  (dolist (file files)
                    (cl-pushnew file (opencode-magit-root-files (root top session-id))
                                :test #'equal))))))
            (dolist (path (opencode-magit--step-paths step))
              (add-file path session-id))
            (dolist (workdir (opencode-magit--step-workdirs step))
              (when-let* (((file-directory-p workdir))
                          (top (toplevel workdir)))
                (root top session-id))))))
      (dolist (step steps)
        (when-let* (((opencode-magit--step-start step))
                    ((opencode-magit--step-end step))
                    (root (gethash (toplevel (opencode-magit--step-directory step)) roots)))
          (push step (opencode-magit-root-steps root))))
      (sort (mapcar (lambda (root)
                      (setf (opencode-magit-root-files root)
                            (sort (opencode-magit-root-files root) #'string<))
                      (setf (opencode-magit-root-sessions root)
                            (nreverse (opencode-magit-root-sessions root)))
                      (setf (opencode-magit-root-steps root)
                            (nreverse (opencode-magit-root-steps root)))
                      root)
                    (hash-table-values roots))
            (lambda (a b) (string< (opencode-magit-root-directory a)
                                   (opencode-magit-root-directory b)))))))

(defun opencode-magit--snapshot-range (root)
  "Return (FROM . TO), the snapshots spanning ROOT's steps, or nil."
  (let ((steps (sort (copy-sequence (opencode-magit-root-steps root))
                     (lambda (a b) (< (or (opencode-magit--step-time a) 0)
                                      (or (opencode-magit--step-time b) 0))))))
    (when steps
      (cons (opencode-magit--step-start (car steps))
            (opencode-magit--step-end (car (last steps)))))))

;;; Fetching

(defun opencode-magit--fetch-tree (connection session-id callback)
  "Fetch SESSION-ID and its subagents through CONNECTION.
Call CALLBACK with a list of (INFO . MESSAGES), SESSION-ID's entry first."
  (let ((pending 1)
        entries
        failed)
    (cl-labels
        ((settle ()
           (when (and (zerop (cl-decf pending)) (not failed))
             (funcall callback (reverse entries))))
         (fail (&rest _)
           (unless failed
             (setq failed t)
             (message "OpenCode: failed to load the session's changes")))
         (visit (info)
           (let ((id (alist-get 'id info))
                 (entry (list info)))
             (push entry entries)
             (cl-incf pending 2)
             (opencode-client-session-messages
              connection id
              :success (lambda (&rest args)
                         (setcdr entry (plist-get args :data))
                         (settle))
              :error #'fail)
             (opencode-client-sessions
              connection :parent id :limit opencode-magit--children-limit
              :success (lambda (&rest args)
                         (mapc #'visit (plist-get args :data))
                         (settle))
              :error #'fail))))
      (opencode-client-session-get
       connection session-id
       :success (lambda (&rest args)
                  (visit (plist-get args :data))
                  (settle))
       :error #'fail))))

(defun opencode-magit--context ()
  "Return the session the current buffer shows as a plist.
The plist has :connection, :session-id, and :title."
  (cond
   ((derived-mode-p 'opencode-magit-overview-mode)
    (list :connection (plist-get opencode-magit--overview :connection)
          :session-id (plist-get opencode-magit--overview :session-id)
          :title (plist-get opencode-magit--overview :title)))
   ((and (derived-mode-p 'opencode-session-mode) opencode-session--session)
    (unless opencode-session--connection
      (user-error "OpenCode session is not connected"))
    (let ((session opencode-session--session))
      (list :connection opencode-session--connection
            :session-id (opencode-session-id session)
            :title (or (opencode-session-title session) (opencode-session-id session)))))
   (t (user-error "Not in an OpenCode session buffer"))))

(defun opencode-magit--collect (context turn callback)
  "Find the worktrees changed by CONTEXT's session and its subagents.
CONTEXT is a plist from `opencode-magit--context'.  TURN restricts the
changes as in `opencode-magit--tree-steps'.  CALLBACK is called with the
roots and the fetched session tree."
  (let ((buffer (current-buffer)))
    (message "OpenCode: finding changed worktrees...")
    (opencode-magit--fetch-tree
     (plist-get context :connection) (plist-get context :session-id)
     (lambda (tree)
       ;; Request callbacks run in a process sentinel, which must not prompt.
       (run-at-time
        0 nil
        (lambda ()
          (message nil)
          (with-current-buffer (if (buffer-live-p buffer) buffer (current-buffer))
            (condition-case err
                (funcall callback
                         (opencode-magit--roots (opencode-magit--tree-steps tree turn))
                         tree)
              (error (message "OpenCode: %s" (error-message-string err)))))))))))

(defun opencode-magit--titles (tree)
  "Return an alist mapping the session IDs in TREE to their titles."
  (mapcar (lambda (entry)
            (cons (alist-get 'id (car entry))
                  (or (alist-get 'title (car entry)) (alist-get 'id (car entry)))))
          tree))

(defun opencode-magit--read-root (roots titles prompt)
  "Read one of ROOTS with PROMPT, or return the only one.
TITLES maps session IDs to titles."
  (if (cdr roots)
      (let ((choices (mapcar (lambda (root)
                               (cons (format "%s  (%s)"
                                             (abbreviate-file-name
                                              (opencode-magit-root-directory root))
                                             (mapconcat (lambda (id) (alist-get id titles nil nil #'equal))
                                                        (opencode-magit-root-sessions root)
                                                        ", "))
                                     root))
                             roots)))
        (cdr (assoc (completing-read prompt choices nil t) choices)))
    (car roots)))

(defun opencode-magit--with-root (turn prompt function)
  "Call FUNCTION with a root changed by the current session and its title.
TURN restricts the changes as in `opencode-magit--tree-steps'.  When the
session changed several worktrees, read one with PROMPT."
  (let ((context (opencode-magit--context)))
    (opencode-magit--collect
     context turn
     (lambda (roots tree)
       (if roots
           (funcall function
                    (opencode-magit--read-root roots (opencode-magit--titles tree) prompt)
                    (plist-get context :title))
         (message "OpenCode: found no changes from this session"))))))

(defun opencode-magit--turn-at-point ()
  "Return the ID of the transcript message at point, or t for the latest turn."
  (or (and (derived-mode-p 'opencode-session-mode)
           (when-let* ((message (opencode-session--message-at-point)))
             (opencode-message-id message)))
      t))

;;; Magit views

(defun opencode-magit--existing-directory (root)
  "Return ROOT's directory, or signal an error when it was removed."
  (let ((directory (opencode-magit-root-directory root)))
    (unless (file-directory-p directory)
      (user-error "Worktree %s no longer exists" (abbreviate-file-name directory)))
    directory))

(defun opencode-magit--base ()
  "Return the revision Magit treats as the base of the current branch.
That is the upstream, else the main branch's upstream, else the main
branch.  An upstream that is the branch's own copy on a remote is not a
base, because it only differs by what has not been pushed."
  (let ((main (magit-main-branch))
        (upstream (magit-get-upstream-branch)))
    (or (and upstream
             (not (equal (cdr (magit-split-branch-name upstream))
                         (magit-get-current-branch)))
             upstream)
        (and main (magit-get-upstream-branch main))
        main)))

(defun opencode-magit--diff (root &optional session-files)
  "Show ROOT's changes since its base in a Magit diff buffer.
This includes uncommitted changes.  With SESSION-FILES, show only the
files the session changed."
  (let* ((default-directory (opencode-magit--existing-directory root))
         (base (or (opencode-magit--base)
                   (user-error "Cannot find a base branch in %s"
                               (abbreviate-file-name default-directory))))
         (merge-base (or (magit-git-string "merge-base" base "HEAD")
                         (user-error "%s has no history in common with HEAD" base))))
    (magit-diff-working-tree merge-base
                             (car (magit-diff-arguments 'magit-diff-mode))
                             (and session-files (opencode-magit-root-files root)))))

(defun opencode-magit--data-directory ()
  "Return the directory where OpenCode keeps its data."
  (expand-file-name "opencode" (or (getenv "XDG_DATA_HOME")
                                   (expand-file-name "~/.local/share"))))

(defun opencode-magit--snapshot-git-dir (directory tree)
  "Return OpenCode's snapshot repository for worktree DIRECTORY holding TREE.
OpenCode keeps one repository per project and worktree, named after the
SHA-1 of the worktree's path."
  (let ((hash (secure-hash 'sha1 (directory-file-name (file-truename directory)))))
    (cl-find-if (lambda (git-dir)
                  (zerop (call-process magit-git-executable nil nil nil
                                       (concat "--git-dir=" git-dir) "cat-file" "-e" tree)))
                (file-expand-wildcards
                 (expand-file-name (concat "snapshot/*/" hash)
                                   (opencode-magit--data-directory))))))

(defconst opencode-magit--snapshot-commit-environment
  '("GIT_AUTHOR_NAME=OpenCode" "GIT_AUTHOR_EMAIL=opencode@localhost"
    "GIT_AUTHOR_DATE=@0 +0000" "GIT_COMMITTER_NAME=OpenCode"
    "GIT_COMMITTER_EMAIL=opencode@localhost" "GIT_COMMITTER_DATE=@0 +0000")
  "Identity and date of snapshot commits.
Fixing them gives a tree the same commit every time, so a review of a
snapshot diff resumes instead of starting over.")

(defun opencode-magit--git (input environment &rest args)
  "Run Git with ARGS in `default-directory' and return its trimmed output.
INPUT is sent on standard input when non-nil, and ENVIRONMENT is a list
of extra variables."
  (let ((process-environment (append environment process-environment))
        (stderr (make-temp-file "opencode-git")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (if input
                            (progn
                              (insert input)
                              (apply #'call-process-region (point-min) (point-max)
                                     magit-git-executable t (list t stderr) nil args))
                          (apply #'process-file magit-git-executable nil (list t stderr) nil
                                 args))))
            (unless (zerop status)
              (user-error "git %s failed: %s" (car args)
                          (with-temp-buffer
                            (insert-file-contents stderr)
                            (string-trim (buffer-string)))))
            (string-trim (buffer-string))))
      (delete-file stderr))))

(defun opencode-magit--has-object-p (object)
  "Return non-nil when the current repository has OBJECT."
  (zerop (process-file magit-git-executable nil nil nil "cat-file" "-e" object)))

(defun opencode-magit--import-tree (git-dir tree &optional known)
  "Copy TREE and its contents from GIT-DIR into the current repository.
KNOWN is a tree already copied, whose contents are skipped."
  (unless (opencode-magit--has-object-p tree)
    (let* ((git (shell-quote-argument magit-git-executable))
           (source (concat git " --git-dir=" (shell-quote-argument git-dir))))
      (call-process-shell-command
       (format "%s rev-list --objects %s | %s pack-objects --stdout -q | %s unpack-objects -q"
               source
               (concat (shell-quote-argument tree)
                       (and known (concat " --not " (shell-quote-argument known))))
               source git))
      (unless (opencode-magit--has-object-p tree)
        (user-error "Cannot copy OpenCode snapshot %s" tree)))))

(defun opencode-magit--tracked-tree (tree)
  "Return TREE without the files that are on disk but untracked.
Git compares only tracked files with the working tree, so those files
would look deleted.  Files that are gone from disk stay, so their
deletion shows."
  (let ((index (make-temp-file "opencode-index")))
    (unwind-protect
        (let* ((scratch (list (concat "GIT_INDEX_FILE=" index)))
               (tracked (make-hash-table :test #'equal))
               untracked)
          (dolist (file (split-string (opencode-magit--git nil nil "ls-files" "-z") "\0" t))
            (puthash file t tracked))
          (opencode-magit--git nil scratch "read-tree" tree)
          (dolist (file (split-string (opencode-magit--git nil scratch "ls-files" "-z") "\0" t))
            (unless (or (gethash file tracked) (not (file-exists-p file)))
              (push file untracked)))
          (when untracked
            (opencode-magit--git (concat (string-join untracked "\0") "\0") scratch
                                 "update-index" "--force-remove" "-z" "--stdin"))
          (opencode-magit--git nil scratch "write-tree"))
      (delete-file index))))

(defun opencode-magit--snapshot-commit (git-dir tree &optional tracked-only known)
  "Return a commit in the current repository holding snapshot TREE from GIT-DIR.
With TRACKED-ONLY, leave out files that are on disk but untracked.  KNOWN
is a tree already copied from GIT-DIR."
  (opencode-magit--import-tree git-dir tree known)
  (opencode-magit--git nil opencode-magit--snapshot-commit-environment
                       "commit-tree" "--no-gpg-sign"
                       (if tracked-only (opencode-magit--tracked-tree tree) tree)
                       "-m" "OpenCode snapshot"))

(defun opencode-magit--filter-files (root &rest revs)
  "Return ROOT's files when others changed between REVS, else nil.
REVS are one revision, compared with the working tree, or two.  Without a
filter the diff keeps the same arguments as the session goes on."
  (let ((changed (split-string (apply #'opencode-magit--git nil nil
                                      "diff" "--name-only" "-z" (append revs '("--")))
                               "\0" t)))
    (unless (cl-subsetp changed (opencode-magit-root-files root) :test #'equal)
      (opencode-magit-root-files root))))

(defun opencode-magit--snapshots (root)
  "Return (GIT-DIR FROM . TO) for the snapshots spanning ROOT's steps."
  (pcase-let* ((directory (opencode-magit--existing-directory root))
               (`(,from . ,to)
                (or (opencode-magit--snapshot-range root)
                    (user-error "OpenCode recorded no snapshots in %s"
                                (abbreviate-file-name directory)))))
    (unless (opencode-magit-root-files root)
      (user-error "OpenCode recorded no changed files in %s"
                  (abbreviate-file-name directory)))
    (cons (or (opencode-magit--snapshot-git-dir directory from)
              (user-error "Cannot find OpenCode's snapshots of %s"
                          (abbreviate-file-name directory)))
          (cons from to))))

(defun opencode-magit--session-diff (root)
  "Show the changes in ROOT since the session started there.
Return the Magit diff buffer."
  (pcase-let* ((`(,git-dir ,from . ,_) (opencode-magit--snapshots root))
               (default-directory (opencode-magit-root-directory root))
               (start (opencode-magit--snapshot-commit git-dir from t)))
    (magit-diff-working-tree start (car (magit-diff-arguments 'magit-diff-mode))
                             (opencode-magit--filter-files root start))))

(defun opencode-magit--turn-diff (root)
  "Show the changes OpenCode recorded in ROOT during a turn.
Return the Magit diff buffer."
  (pcase-let* ((`(,git-dir ,from . ,to) (opencode-magit--snapshots root))
               (default-directory (opencode-magit-root-directory root))
               (start (opencode-magit--snapshot-commit git-dir from))
               (end (opencode-magit--snapshot-commit git-dir to nil from)))
    (magit-diff-range (concat start ".." end) (car (magit-diff-arguments 'magit-diff-mode))
                      (opencode-magit--filter-files root start end))))

;;; Commands

;;;###autoload
(defun opencode-magit-status ()
  "Show `magit-status' for a worktree the current session changed."
  (interactive)
  (opencode-magit--with-root
   nil "Magit status for worktree: "
   (lambda (root _title)
     (magit-status-setup-buffer (opencode-magit--existing-directory root)))))

;;;###autoload
(defun opencode-magit-diff (&optional session-files)
  "Show a worktree's changes since its base branch, uncommitted ones included.
The worktree is one the current session changed, and the base is the
branch Magit considers upstream.  With a prefix argument SESSION-FILES,
show only the files the session changed."
  (interactive "P")
  (opencode-magit--with-root
   nil "Diff worktree: "
   (lambda (root _title) (opencode-magit--diff root session-files))))

;;;###autoload
(defun opencode-magit-session-diff ()
  "Show what changed in a worktree since the current session started there.
The diff runs to the working tree, so it shows later changes too.  When
files the session did not change differ as well, only the session's
files are shown."
  (interactive)
  (opencode-magit--with-root
   nil "Session changes in worktree: "
   (lambda (root _title) (opencode-magit--session-diff root))))

;;;###autoload
(defun opencode-magit-turn-diff ()
  "Show what one turn of the current session changed in a worktree.
The turn is the one at point, or the latest one.  Its changes include
those of the subagents it started."
  (interactive)
  (let ((turn (opencode-magit--turn-at-point)))
    (opencode-magit--with-root
     turn "Turn changes in worktree: "
     (lambda (root _title) (opencode-magit--turn-diff root)))))

;;; Overview

(defvar-keymap opencode-magit-overview-mode-map
  :doc "Keymap for `opencode-magit-overview-mode'."
  :parent magit-section-mode-map
  "RET" #'opencode-magit-overview-status
  "d" #'opencode-magit-overview-diff
  "s" #'opencode-magit-overview-session-diff
  "o" #'opencode-magit-overview-open-session
  "g" #'opencode-magit-overview-refresh)

(define-derived-mode opencode-magit-overview-mode magit-section-mode "OpenCode Changes"
  "Mode listing the worktrees an OpenCode session and its subagents changed.

\\{opencode-magit-overview-mode-map}"
  :group 'emacs-opencode)

(defun opencode-magit--describe-worktree (directory)
  "Return a short description of the branch and state of DIRECTORY."
  (if (not (file-directory-p directory))
      "removed"
    (let ((default-directory directory))
      (concat (or (magit-get-current-branch)
                  (format "detached at %s" (magit-rev-abbrev "HEAD")))
              (when (magit-anything-modified-p) ", uncommitted changes")))))

(defun opencode-magit--render-overview ()
  "Render the overview buffer from `opencode-magit--overview'."
  (let ((inhibit-read-only t)
        (roots (plist-get opencode-magit--overview :roots))
        (titles (opencode-magit--titles (plist-get opencode-magit--overview :tree))))
    (erase-buffer)
    (magit-insert-section (opencode-magit-overview)
      (magit-insert-heading
        (format "Worktrees changed by %s" (plist-get opencode-magit--overview :title)))
      (unless roots
        (insert "No changes found.\n"))
      (dolist (root roots)
        (let ((directory (opencode-magit-root-directory root)))
          (magit-insert-section (opencode-magit-root root)
            (magit-insert-heading
              (propertize (abbreviate-file-name directory) 'font-lock-face 'magit-section-heading)
              "  "
              (propertize (opencode-magit--describe-worktree directory)
                          'font-lock-face 'magit-branch-local)
              "  "
              (mapconcat (lambda (id) (alist-get id titles nil nil #'equal))
                         (opencode-magit-root-sessions root)
                         ", "))
            (dolist (file (opencode-magit-root-files root))
              (magit-insert-section (opencode-magit-file file)
                (insert "  " file "\n")))))))
    (goto-char (point-min))))

;;;###autoload
(defun opencode-magit ()
  "List the worktrees the current session and its subagents changed."
  (interactive)
  (let ((context (opencode-magit--context)))
    (opencode-magit--collect
     context nil
     (lambda (roots tree)
       (let ((buffer (get-buffer-create
                      (format "*OpenCode changes: %s*" (plist-get context :title)))))
         (with-current-buffer buffer
           (unless (derived-mode-p 'opencode-magit-overview-mode)
             (opencode-magit-overview-mode))
           (setq opencode-magit--overview (append context (list :roots roots :tree tree)))
           (opencode-magit--render-overview))
         (pop-to-buffer buffer))))))

(defun opencode-magit--root-at-point ()
  "Return the root of the overview section at point."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) 'opencode-magit-root)))
      (setq section (oref section parent)))
    (if section
        (oref section value)
      (user-error "No worktree at point"))))

(defun opencode-magit-overview-refresh ()
  "Reload the worktrees shown in the overview."
  (interactive)
  (opencode-magit))

(defun opencode-magit-overview-status ()
  "Show `magit-status' for the worktree at point."
  (interactive)
  (magit-status-setup-buffer
   (opencode-magit--existing-directory (opencode-magit--root-at-point))))

(defun opencode-magit-overview-diff (&optional session-files)
  "Show the changes in the worktree at point since its base branch.
With a prefix argument SESSION-FILES, show only the files the session
changed."
  (interactive "P")
  (opencode-magit--diff (opencode-magit--root-at-point) session-files))

(defun opencode-magit-overview-session-diff ()
  "Show what the sessions changed in the worktree at point."
  (interactive)
  (opencode-magit--session-diff (opencode-magit--root-at-point)))

(defun opencode-magit-overview-open-session ()
  "Open a session that changed the worktree at point."
  (interactive)
  (let* ((tree (plist-get opencode-magit--overview :tree))
         (titles (opencode-magit--titles tree))
         (ids (opencode-magit-root-sessions (opencode-magit--root-at-point)))
         (id (if (cdr ids)
                 (let ((choices (mapcar (lambda (id) (cons (alist-get id titles nil nil #'equal) id))
                                        ids)))
                   (cdr (assoc (completing-read "Open session: " choices nil t) choices)))
               (car ids)))
         (info (car (cl-find id tree :key (lambda (entry) (alist-get 'id (car entry)))
                             :test #'equal))))
    (opencode--ensure-session-buffer (opencode-session--session-from-info info)
                                     (plist-get opencode-magit--overview :connection))))

;;; Key bindings

(defvar-keymap opencode-magit-map
  :doc "Keymap for Magit views of an OpenCode session's changes."
  "o" #'opencode-magit
  "s" #'opencode-magit-status
  "d" #'opencode-magit-diff
  "a" #'opencode-magit-session-diff
  "t" #'opencode-magit-turn-diff)

(keymap-set opencode-session-mode-map "C-c C-d" opencode-magit-map)

(provide 'emacs-opencode-magit)

;;; emacs-opencode-magit.el ends here
