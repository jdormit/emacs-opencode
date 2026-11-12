;;; emacs-opencode-changes.el --- Find the worktrees OpenCode sessions changed  -*- lexical-binding: t; -*-

;; Reads a session's messages and its subagents' to find the Git worktrees
;; and files they changed, from snapshots and file tool calls.

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-opencode-client)

(defconst opencode-changes--read-only-tools
  '("read" "glob" "grep" "list" "webfetch" "websearch" "skill" "question"
    "todowrite" "todoread" "subagent" "task")
  "Tools that cannot change files themselves.
Changes recorded during a step that called only these tools were made by
another session, a subagent, or the user.  Subagents record their own
changes.")

(defconst opencode-changes--file-tools '("edit" "write" "patch" "apply_patch")
  "Tools whose input and metadata name the files they change.")

(defconst opencode-changes--children-limit 1000
  "Maximum number of subagent sessions fetched for each session.")

;;; Session analysis

(cl-defstruct (opencode-changes-step (:constructor opencode-changes-step-create))
  "An assistant step, reduced to where it ran and what it changed.
START and END are snapshot trees.  FILES lists the changed files relative
to the worktree, and FILES-KNOWN is nil when OpenCode recorded no list.
WRITES is non-nil when the step called a tool that can change files.
PATHS are files named by file tools and WORKDIRS are other directories
shell commands ran in."
  session-id turn time directory start end files files-known writes paths workdirs)

(cl-defstruct (opencode-changes-root (:constructor opencode-changes-root-create))
  "A Git worktree changed by a session tree.
FILES are relative to DIRECTORY, SESSIONS are the IDs of the sessions that
changed it, and STEPS are their steps that ran there."
  directory files sessions steps)

(defun opencode-changes--type (message)
  "Return the type of server MESSAGE."
  (alist-get 'type message))

(defun opencode-changes--turns (messages)
  "Return a hash table mapping each of MESSAGES' IDs to a turn number.
A turn ends when the session goes idle, so prompts steered into a busy
session share its turn.  Histories without idle markers start a turn at
each prompt instead."
  (let ((turns (make-hash-table :test #'equal))
        (legacy (not (cl-find "idle" messages :key #'opencode-changes--type :test #'equal)))
        (turn 0)
        prompted
        ended)
    (dolist (message messages turns)
      (let ((type (opencode-changes--type message)))
        (when (or ended (and legacy prompted (equal type "user")))
          (cl-incf turn)
          (setq ended nil))
        (when (equal type "user")
          (setq prompted t))
        (puthash (alist-get 'id message) turn turns)
        (when (equal type "idle")
          (setq ended t))))))

(defun opencode-changes--initial-directory (info messages)
  "Return the directory session INFO started in, given its MESSAGES."
  (or (when-let* ((switch (cl-find "location-switched" messages
                                   :key #'opencode-changes--type :test #'equal)))
        (alist-get 'directory (alist-get 'location (alist-get 'previous switch))))
      (alist-get 'directory (alist-get 'location info))))

(defun opencode-changes--expand (path directory)
  "Expand PATH against DIRECTORY, or return nil when PATH is not a name."
  (when (and (stringp path) (not (string-empty-p path)))
    (expand-file-name path (file-name-as-directory directory))))

(defun opencode-changes--session-steps (info messages)
  "Return the steps of session INFO, given its MESSAGES.
Returns (STEPS . SPAWNED), where SPAWNED maps the IDs of subagent
sessions started by the steps to the turn that started them."
  (let ((session-id (alist-get 'id info))
        (directory (opencode-changes--initial-directory info messages))
        (turns (opencode-changes--turns messages))
        steps
        spawned)
    (dolist (message messages)
      (pcase (opencode-changes--type message)
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
                 (unless (member name opencode-changes--read-only-tools)
                   (setq writes t))
                 (when (member name opencode-changes--file-tools)
                   (dolist (path (cons (or (alist-get 'path input) (alist-get 'filePath input))
                                       (mapcar (lambda (file) (alist-get 'file file))
                                               (alist-get 'files metadata))))
                     (when-let* ((path (opencode-changes--expand path directory)))
                       (cl-pushnew path paths :test #'equal))))
                 (when-let* ((workdir (opencode-changes--expand (alist-get 'workdir input)
                                                              directory))
                             ((not (file-equal-p workdir directory))))
                   (cl-pushnew workdir workdirs :test #'equal))
                 (when-let* (((member name '("subagent" "task")))
                             (child (alist-get 'sessionID metadata)))
                   (push (cons child turn) spawned)))))
           (push (opencode-changes-step-create
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

(defun opencode-changes-tree-steps (tree &optional turn)
  "Return the steps of the sessions in TREE.
TREE is a list of (INFO . MESSAGES) whose first entry is the session the
user asked about; the rest are its subagents.  When TURN is non-nil, keep
only that session's steps from one turn, together with the steps that
subagents it started or resumed during that turn ran before the next
turn.  TURN is the ID of a message in the turn, or t for the latest turn."
  (let ((parsed (mapcar (lambda (entry)
                          (cons (alist-get 'id (car entry))
                                (opencode-changes--session-steps (car entry) (cdr entry))))
                        tree)))
    (if (not turn)
        (apply #'append (mapcar #'cadr parsed))
      (let* ((root-id (caar parsed))
             (messages (cdar tree))
             (turns (opencode-changes--turns messages))
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
                        (let ((time (opencode-changes-step-time step)))
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
               (cl-remove-if-not (lambda (step) (equal (opencode-changes-step-turn step) index))
                                 (cadr (assoc root-id parsed)))
               (mapcar (lambda (id) (cl-remove-if-not in-turn (cadr (assoc id parsed))))
                       included))))))

(defun opencode-changes--toplevel (directory)
  "Return the Git worktree containing DIRECTORY, or nil."
  (when-let* ((top (locate-dominating-file directory ".git")))
    (file-name-as-directory (file-truename top))))

(defun opencode-changes-roots (steps)
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
                   (opencode-changes--toplevel directory)
                 directory))))
         (root (directory session-id)
           (let ((root (with-memoization (gethash directory roots)
                         (opencode-changes-root-create :directory directory))))
             (cl-pushnew session-id (opencode-changes-root-sessions root) :test #'equal)
             root))
         (add-file (path session-id)
           (let ((parent (file-name-directory path)))
             (when-let* (((file-directory-p parent))
                         (top (toplevel parent)))
               (cl-pushnew (file-relative-name
                            (expand-file-name (file-name-nondirectory path)
                                              (file-truename parent))
                            top)
                           (opencode-changes-root-files (root top session-id))
                           :test #'equal)))))
      (dolist (step steps)
        (let ((session-id (opencode-changes-step-session-id step))
              (top (toplevel (opencode-changes-step-directory step))))
          (when (opencode-changes-step-writes step)
            (when top
              (let ((files (opencode-changes-step-files step)))
                (cond
                 ((not (opencode-changes-step-files-known step))
                  (root top session-id))
                 ((file-directory-p top)
                  (dolist (file files)
                    (add-file (expand-file-name file top) session-id)))
                 (t
                  ;; A removed worktree can no longer be searched for nested ones.
                  (dolist (file files)
                    (cl-pushnew file (opencode-changes-root-files (root top session-id))
                                :test #'equal))))))
            (dolist (path (opencode-changes-step-paths step))
              (add-file path session-id))
            (dolist (workdir (opencode-changes-step-workdirs step))
              (when-let* (((file-directory-p workdir))
                          (top (toplevel workdir)))
                (root top session-id))))))
      (dolist (step steps)
        (when-let* (((opencode-changes-step-start step))
                    ((opencode-changes-step-end step))
                    (root (gethash (toplevel (opencode-changes-step-directory step)) roots)))
          (push step (opencode-changes-root-steps root))))
      (sort (mapcar (lambda (root)
                      (setf (opencode-changes-root-files root)
                            (sort (opencode-changes-root-files root) #'string<))
                      (setf (opencode-changes-root-sessions root)
                            (nreverse (opencode-changes-root-sessions root)))
                      (setf (opencode-changes-root-steps root)
                            (nreverse (opencode-changes-root-steps root)))
                      root)
                    (hash-table-values roots))
            (lambda (a b) (string< (opencode-changes-root-directory a)
                                   (opencode-changes-root-directory b)))))))

(defun opencode-changes-snapshot-range (root)
  "Return (FROM . TO), the snapshots spanning ROOT's steps, or nil."
  (let ((steps (sort (copy-sequence (opencode-changes-root-steps root))
                     (lambda (a b) (< (or (opencode-changes-step-time a) 0)
                                      (or (opencode-changes-step-time b) 0))))))
    (when steps
      (cons (opencode-changes-step-start (car steps))
            (opencode-changes-step-end (car (last steps)))))))

;;; Fetching

(defun opencode-changes-fetch-tree (connection session-id callback on-error)
  "Fetch SESSION-ID and its subagents through CONNECTION.
Call CALLBACK with a list of (INFO . MESSAGES), SESSION-ID's entry first,
or ON-ERROR with no arguments when a request fails."
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
             (funcall on-error)))
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
              connection :parent id :limit opencode-changes--children-limit
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

(provide 'emacs-opencode-changes)

;;; emacs-opencode-changes.el ends here
