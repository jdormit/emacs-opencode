;;; emacs-opencode-sessions.el --- Overview of OpenCode sessions  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'emacs-opencode-changes)
(require 'emacs-opencode-client)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-session)
(require 'emacs-opencode-session-vars)
(require 'emacs-opencode-session-mode)
(require 'emacs-opencode-sse)

(declare-function opencode-run-server "emacs-opencode" (directory &optional on-ready))
(declare-function opencode--ensure-session-buffer "emacs-opencode"
                  (session connection &optional on-ready))
(declare-function opencode "emacs-opencode" (directory))

(defcustom opencode-sessions-days 7
  "Number of days of top-level sessions the overview lists.
A session is listed when it was updated within that many days.  Running
sessions are listed even when they are older."
  :type 'integer
  :group 'emacs-opencode)

(defcustom opencode-sessions-max 1000
  "Maximum number of top-level sessions the overview lists.
This caps how much the overview loads when many sessions were updated
within `opencode-sessions-days'."
  :type 'natnum
  :group 'emacs-opencode)

(defcustom opencode-sessions-sidebar-width 40
  "Width of the session sidebar, in columns."
  :type 'natnum
  :group 'emacs-opencode)

(defcustom opencode-sessions-sidebar-side 'left
  "Side of the frame the session sidebar shows on."
  :type '(choice (const left) (const right))
  :group 'emacs-opencode)

(defcustom opencode-sessions-archive-buries-buffer nil
  "Non-nil means archiving a session from the overview buries its buffer.
The buffer moves to the end of the buffer list and is replaced in the
windows showing it."
  :type 'boolean
  :group 'emacs-opencode)

(defconst opencode-sessions--page-size 200
  "Number of sessions requested per page while loading the overview.")

(defconst opencode-sessions--buffer-name "*OpenCode: Sessions*"
  "Name of the session overview buffer.")

(defconst opencode-sessions--sidebar-buffer-name "*OpenCode: Sessions Sidebar*"
  "Name of the session sidebar buffer.")

(defconst opencode-sessions--max-fetches 4
  "Maximum number of sessions whose changes are looked up at once.")

(cl-defstruct (opencode-sessions--tracker (:constructor opencode-sessions--tracker-create))
  "What the overview knows about the server's sessions.
SESSIONS maps the ID of each listed top-level session to its info.
PARENTS maps other session IDs to their parent's ID, to `:root' for
top-level sessions that are not listed, or to `:pending' while being
looked up.  RUNNING maps running session IDs to \"busy\" or \"retry\".
PROMPTS maps pending request IDs to (SESSION-ID . KIND), where KIND is
`permission' or `question'.  CHANGES maps listed session IDs to what
they changed, an `opencode-sessions--work'.  PROJECTS maps project IDs to
display names.  VIEWED maps session IDs to the idle time last reported as
viewed.  QUEUE lists (SESSION-ID . WORK) for the sessions waiting to have
their changes looked up, and FETCHING counts the lookups running."
  connection
  (sessions (make-hash-table :test 'equal))
  (parents (make-hash-table :test 'equal))
  (running (make-hash-table :test 'equal))
  (prompts (make-hash-table :test 'equal))
  (changes (make-hash-table :test 'equal))
  (projects (make-hash-table :test 'equal))
  (viewed (make-hash-table :test 'equal))
  (queue nil)
  (fetching 0))

(cl-defstruct (opencode-sessions--work (:constructor opencode-sessions--work-create))
  "What a listed session and its subagents changed.
ROOTS are the worktrees they changed, found when the session was last
UPDATED.  STATS maps worktree directories to (ADDED . DELETED) line
counts, from the counts started at GENERATION.  LOADING is non-nil while
ROOTS are looked up, and AGAIN when they must be looked up again once
that finishes."
  updated roots stats (generation 0) loading again)

(defvar opencode-sessions--tracker nil
  "The session tracker, or nil when neither the overview nor the indicator is on.")

(defvar opencode-sessions--refresh-timer nil
  "Timer that redraws the overview and indicator after changes.")

(defvar opencode-sessions--indicator ""
  "Mode line text of `opencode-sessions-indicator-mode'.")
(put 'opencode-sessions--indicator 'risky-local-variable t)

(defvar opencode-sessions-indicator-mode)

(defvar-local opencode-sessions--flagged nil
  "Hash table mapping flagged session IDs to `delete' or `archive'.")

(defvar-local opencode-sessions--folded nil
  "Hash table of the project IDs whose sessions are hidden.")

;;; Session data

(defun opencode-sessions--table (slot)
  "Return the tracker's table in SLOT."
  (cl-struct-slot-value 'opencode-sessions--tracker slot opencode-sessions--tracker))

(defun opencode-sessions--directory (info)
  "Return the directory of session INFO."
  (alist-get 'directory (alist-get 'location info)))

(defun opencode-sessions--hide (session-id)
  "Stop listing the top-level session SESSION-ID, remembering it is top-level."
  (remhash session-id (opencode-sessions--table 'sessions))
  (remhash session-id (opencode-sessions--table 'changes))
  (puthash session-id :root (opencode-sessions--table 'parents)))

(defun opencode-sessions--add (info)
  "List the top-level session INFO, unless it is archived."
  (let ((id (alist-get 'id info)))
    (if (alist-get 'archived (alist-get 'time info))
        (opencode-sessions--hide id)
      (remhash id (opencode-sessions--table 'parents))
      (puthash id info (opencode-sessions--table 'sessions)))))

(defun opencode-sessions--update (session-id key value)
  "Set KEY to VALUE in the info of the listed session SESSION-ID."
  (let ((sessions (opencode-sessions--table 'sessions)))
    (when-let* ((info (gethash session-id sessions)))
      (setq info (copy-alist info))
      (setf (alist-get key info) value)
      (puthash session-id info sessions))))

(defun opencode-sessions--root (session-id)
  "Return the ID of the listed session that SESSION-ID belongs to, or nil."
  (let ((sessions (opencode-sessions--table 'sessions))
        (parents (opencode-sessions--table 'parents))
        (seen nil)
        (root nil))
    (while (and (stringp session-id) (not root) (not (member session-id seen)))
      (push session-id seen)
      (if (gethash session-id sessions)
          (setq root session-id)
        (setq session-id (gethash session-id parents))))
    root))

(defun opencode-sessions--learn (session-id)
  "Look up SESSION-ID when the tracker does not know where it belongs.
A top-level session is added to the list.  For a subagent session, its
parent is learned in turn."
  (let ((tracker opencode-sessions--tracker))
    (when (and tracker
               session-id
               (not (gethash session-id (opencode-sessions--table 'sessions)))
               (not (gethash session-id (opencode-sessions--table 'parents))))
      (puthash session-id :pending (opencode-sessions--table 'parents))
      (opencode-client-session-get
       (opencode-sessions--tracker-connection tracker)
       session-id
       :success (lambda (&rest args)
                  (when (eq tracker opencode-sessions--tracker)
                    (let* ((info (plist-get args :data))
                           (parent (alist-get 'parentID info)))
                      (if parent
                          (progn
                            (puthash session-id parent (opencode-sessions--table 'parents))
                            (opencode-sessions--learn parent))
                        (opencode-sessions--add info)
                        (opencode-sessions--fetch-changes session-id)))
                    (opencode-sessions--changed)))
       :error (lambda (&rest _args)
                (when (eq tracker opencode-sessions--tracker)
                  (remhash session-id (opencode-sessions--table 'parents))))))))

(defun opencode-sessions--refresh-info (session-id)
  "Reload the info of the listed session SESSION-ID."
  (let ((tracker opencode-sessions--tracker))
    (opencode-client-session-get
     (opencode-sessions--tracker-connection tracker)
     session-id
     :success (lambda (&rest args)
                (when (and (eq tracker opencode-sessions--tracker)
                           (gethash session-id (opencode-sessions--table 'sessions)))
                  (opencode-sessions--add (plist-get args :data))
                  (opencode-sessions--view-selected)
                  (opencode-sessions--changed)))
     :error #'ignore)))

(defun opencode-sessions--add-prompt (request-id session-id kind)
  "Record pending REQUEST-ID of SESSION-ID.
KIND is `permission' or `question'."
  (puthash request-id (cons session-id kind) (opencode-sessions--table 'prompts)))

(defun opencode-sessions--prompt-kinds (session-id)
  "Return the kinds of the prompts pending in the listed session SESSION-ID."
  (let (kinds)
    (maphash (lambda (_request-id prompt)
               (when (equal (opencode-sessions--root (car prompt)) session-id)
                 (push (cdr prompt) kinds)))
             (opencode-sessions--table 'prompts))
    kinds))

;;; Session state

(defconst opencode-sessions--states
  '((permission "permission" error)
    (question "question" warning)
    (failed "failed" error)
    (done "done" success)
    (stopped "stopped" success)
    (retrying "retrying" warning)
    (running "running" font-lock-keyword-face))
  "Label and face of each session state, most urgent first.")

(defun opencode-sessions--unread-p (info)
  "Return non-nil when session INFO went idle after it was last viewed."
  (let* ((time (alist-get 'time info))
         (idle (alist-get 'idle time))
         (viewed (alist-get 'viewed time)))
    (and (numberp idle)
         (or (not (numberp viewed)) (> idle viewed)))))

(defun opencode-sessions--state (session-id)
  "Return the state of the listed session SESSION-ID, counting its subagents.
The state is a key of `opencode-sessions--states', or nil when the
session is idle and its last run was viewed."
  (let ((info (gethash session-id (opencode-sessions--table 'sessions)))
        (kinds (opencode-sessions--prompt-kinds session-id))
        (running nil))
    (maphash (lambda (id status)
               (when (equal (opencode-sessions--root id) session-id)
                 (push status running)))
             (opencode-sessions--table 'running))
    (cond
     ((memq 'permission kinds) 'permission)
     ((memq 'question kinds) 'question)
     ((member "retry" running) 'retrying)
     (running 'running)
     ((opencode-sessions--unread-p info)
      (pcase (alist-get 'outcome info)
        ("failed" 'failed)
        ("interrupted" 'stopped)
        (_ 'done))))))

(defun opencode-sessions--rank (state)
  "Return the sort rank of STATE; lower ranks are more urgent."
  (or (cl-position state opencode-sessions--states :key #'car)
      (length opencode-sessions--states)))

(defun opencode-sessions--updated (info)
  "Return the time session INFO was last updated."
  (or (alist-get 'updated (alist-get 'time info)) 0))

(defun opencode-sessions--before-p (a b)
  "Return non-nil when session info A sorts before B.
More urgent sessions come first, then more recently updated ones."
  (let ((rank-a (opencode-sessions--rank (opencode-sessions--state (alist-get 'id a))))
        (rank-b (opencode-sessions--rank (opencode-sessions--state (alist-get 'id b)))))
    (if (= rank-a rank-b)
        (> (opencode-sessions--updated a) (opencode-sessions--updated b))
      (< rank-a rank-b))))

;;; Formatting

(defun opencode-sessions--format-state (state)
  "Return the label of STATE."
  (if-let* ((entry (alist-get state opencode-sessions--states)))
      (propertize (car entry) 'face (cadr entry))
    ""))

(defun opencode-sessions--format-age (time)
  "Return how long ago TIME, in epoch milliseconds, was."
  (if (not (numberp time))
      ""
    (let ((seconds (- (float-time) (/ time 1000.0))))
      (cond
       ((< seconds 60) "now")
       ((< seconds 3600) (format "%dm" (/ seconds 60)))
       ((< seconds 86400) (format "%dh" (/ seconds 3600)))
       (t (format "%dd" (/ seconds 86400)))))))

(defun opencode-sessions--format-cost (cost)
  "Return COST in dollars, or an empty string when there is none."
  (if (and (numberp cost) (> cost 0)) (format "$%.2f" cost) ""))

(defun opencode-sessions--format-diff (diff)
  "Return the line counts in DIFF, a cons (ADDED . DELETED)."
  (if (and (consp diff) (or (> (car diff) 0) (> (cdr diff) 0)))
      (concat (propertize (format "+%d" (car diff)) 'face 'success)
              " "
              (propertize (format "-%d" (cdr diff)) 'face 'error))
    ""))

(defun opencode-sessions--roots (session-id)
  "Return the worktrees the listed session SESSION-ID changed, as far as known."
  (when-let* ((work (gethash session-id (opencode-sessions--table 'changes))))
    (opencode-sessions--work-roots work)))

(defun opencode-sessions--diff (session-id)
  "Return the (ADDED . DELETED) lines counted so far for SESSION-ID's worktrees."
  (when-let* ((work (gethash session-id (opencode-sessions--table 'changes))))
    (let ((stats (opencode-sessions--work-stats work))
          diff)
      (dolist (root (opencode-sessions--work-roots work) diff)
        (when-let* ((counts (alist-get (opencode-changes-root-directory root) stats
                                       nil nil #'equal)))
          (setq diff (cons (+ (car counts) (or (car diff) 0))
                           (+ (cdr counts) (or (cdr diff) 0)))))))))

(defun opencode-sessions--format-directory (info)
  "Return where session INFO worked: the worktrees it changed, else its directory.
When it changed several, the one with the most changed lines comes first."
  (let* ((work (gethash (alist-get 'id info) (opencode-sessions--table 'changes)))
         (stats (and work (opencode-sessions--work-stats work)))
         (size (lambda (root)
                 (let ((counts (alist-get (opencode-changes-root-directory root) stats
                                          nil nil #'equal)))
                   (+ (or (car counts) 0) (or (cdr counts) 0)))))
         (directories (mapcar (lambda (root)
                                (abbreviate-file-name
                                 (directory-file-name (opencode-changes-root-directory root))))
                              (sort (copy-sequence (opencode-sessions--roots (alist-get 'id info)))
                                    (lambda (a b) (> (funcall size a) (funcall size b)))))))
    (cond
     ((cdr directories)
      (propertize (format "%s +%d" (car directories) (length (cdr directories)))
                  'help-echo (string-join directories "\n")))
     (directories (car directories))
     ((opencode-sessions--directory info)
      (abbreviate-file-name (opencode-sessions--directory info)))
     (t ""))))

(defun opencode-sessions--entry (info)
  "Return the tabulated list entry for session INFO."
  (let ((id (alist-get 'id info)))
    (list id
          (vector (opencode-sessions--format-state (opencode-sessions--state id))
                  (or (alist-get 'title info) "Untitled session")
                  (opencode-sessions--format-directory info)
                  (opencode-sessions--format-diff (opencode-sessions--diff id))
                  (opencode-sessions--format-cost (alist-get 'cost info))
                  (opencode-sessions--format-age (alist-get 'updated (alist-get 'time info)))))))

(defun opencode-sessions--project-name (project-id)
  "Return the display name of PROJECT-ID."
  (cond
   ((gethash project-id (opencode-sessions--table 'projects)))
   ((equal project-id "global") "Outside projects")
   (t (or project-id "Unknown project"))))

(defun opencode-sessions--heading (project-id infos folded)
  "Return the heading of PROJECT-ID's group of sessions INFOS.
When FOLDED is non-nil, it says how many sessions are hidden."
  (propertize (concat (propertize (opencode-sessions--project-name project-id) 'face 'bold)
                      (when folded
                        (propertize (format " (%d hidden)" (length infos)) 'face 'shadow)))
              'opencode-sessions-project project-id))

(defun opencode-sessions--groups (&optional folded)
  "Return the listed sessions as tabulated list groups, one per project.
Groups with more urgent sessions come first.  Projects in the hash table
FOLDED show only their heading."
  (let ((by-project (make-hash-table :test 'equal))
        (groups nil))
    (maphash (lambda (_id info)
               (push info (gethash (alist-get 'projectID info) by-project)))
             (opencode-sessions--table 'sessions))
    (maphash (lambda (project infos)
               (push (cons project (sort infos #'opencode-sessions--before-p)) groups))
             by-project)
    (mapcar (pcase-lambda (`(,project . ,infos))
              (let ((hidden (and folded (gethash project folded))))
                (cons (opencode-sessions--heading project infos hidden)
                      (unless hidden (mapcar #'opencode-sessions--entry infos)))))
            (sort groups (lambda (a b) (opencode-sessions--before-p (cadr a) (cadr b)))))))

;;; Changes

(defconst opencode-sessions--diff-script
  "branch=$(git symbolic-ref --short -q HEAD)
upstream=$(git rev-parse --abbrev-ref '@{upstream}' 2>/dev/null)
if [ -n \"$upstream\" ] && [ \"${upstream#*/}\" != \"$branch\" ]; then
  base=$upstream
else
  base=$(git rev-parse --abbrev-ref origin/HEAD 2>/dev/null)
fi
from=$(git merge-base HEAD \"$base\" 2>/dev/null) || from=HEAD
[ $# -eq 0 ] && exec git diff --shortstat \"$from\" 2>/dev/null
scratch=$(mktemp -d) || exit 1
trap 'rm -rf \"$scratch\"' EXIT
git ls-files -z --others --exclude-standard -- \"$@\" >\"$scratch/new\" || exit 1
if [ -s \"$scratch/new\" ]; then
  index=$(git rev-parse --git-path index) || exit 1
  if [ -f \"$index\" ]; then cp \"$index\" \"$scratch/index\" || exit 1; fi
  export GIT_INDEX_FILE=\"$scratch/index\"
  git -c core.splitIndex=false add --intent-to-add \\
    --pathspec-from-file=\"$scratch/new\" --pathspec-file-nul >/dev/null 2>&1
fi
git diff --shortstat \"$from\" -- \"$@\" 2>/dev/null"
  "Shell script printing a worktree's changes since its base branch.
The base is the upstream branch, unless that is the branch's own copy on
a remote, in which case it is the remote's default branch.  Uncommitted
changes count too.  The arguments are the files to count, or none for
every tracked file.  Untracked files among them are added to a copy of
the index, so they count while Git's own index stays untouched.")

(defun opencode-sessions--parse-shortstat (output)
  "Return the (ADDED . DELETED) line counts in git diff --shortstat OUTPUT."
  (cl-flet ((lines (kind)
              (if (string-match (concat "\\([0-9]+\\) " kind) output)
                  (string-to-number (match-string 1 output))
                0)))
    (cons (lines "insertion") (lines "deletion"))))

(defun opencode-sessions--count (work root)
  "Count the changes in worktree ROOT since its base branch into WORK's stats.
Only the files the session changed count, or every file when OpenCode
recorded none.  The count is dropped when a newer one has started."
  (let ((tracker opencode-sessions--tracker)
        (generation (opencode-sessions--work-generation work))
        (directory (opencode-changes-root-directory root)))
    (when (file-directory-p directory)
      (let ((default-directory (file-name-as-directory directory))
            (process-environment (append '("GIT_LITERAL_PATHSPECS=1" "GIT_OPTIONAL_LOCKS=0")
                                         process-environment))
            (buffer (generate-new-buffer " *opencode-sessions-diff*")))
        (condition-case nil
            (make-process
             :name "opencode-sessions-diff"
             :buffer buffer
             :noquery t
             :connection-type 'pipe
             :command (append (list "sh" "-c" opencode-sessions--diff-script "sh")
                              (opencode-changes-root-files root))
             :sentinel
             (lambda (process _event)
               (when (memq (process-status process) '(exit signal))
                 (unwind-protect
                     (when (and (eq tracker opencode-sessions--tracker)
                                (= generation (opencode-sessions--work-generation work))
                                (zerop (process-exit-status process)))
                       (setf (alist-get directory (opencode-sessions--work-stats work)
                                        nil nil #'equal)
                             (opencode-sessions--parse-shortstat
                              (with-current-buffer buffer (buffer-string))))
                       (opencode-sessions--changed))
                   (kill-buffer buffer)))))
          (error (kill-buffer buffer)))))))

(defun opencode-sessions--count-all (work)
  "Count the changes in each worktree in WORK."
  (cl-incf (opencode-sessions--work-generation work))
  (dolist (root (opencode-sessions--work-roots work))
    (opencode-sessions--count work root)))

(defun opencode-sessions--find-roots (session-id work)
  "Look up the worktrees SESSION-ID and its subagents changed into WORK."
  (let ((tracker opencode-sessions--tracker))
    (cl-flet ((finish ()
                (when (eq tracker opencode-sessions--tracker)
                  (cl-decf (opencode-sessions--tracker-fetching tracker))
                  (setf (opencode-sessions--work-loading work) nil)
                  (when (opencode-sessions--work-again work)
                    (setf (opencode-sessions--work-again work) nil)
                    (opencode-sessions--fetch-changes session-id t))
                  (opencode-sessions--start-fetches))))
      (opencode-changes-fetch-tree
       (opencode-sessions--tracker-connection tracker) session-id
       (lambda (tree)
         (unwind-protect
             (when (eq tracker opencode-sessions--tracker)
               (condition-case err
                   (let ((roots (opencode-changes-roots (opencode-changes-tree-steps tree))))
                     (setf (opencode-sessions--work-roots work) roots)
                     (setf (opencode-sessions--work-updated work)
                           (opencode-sessions--updated (caar tree)))
                     (opencode-sessions--count-all work)
                     (opencode-sessions--changed))
                 (error (message "OpenCode: cannot find a session's changes: %s"
                                 (error-message-string err)))))
           (finish)))
       (lambda () (finish))))))

(defun opencode-sessions--start-fetches ()
  "Start looking up queued sessions' changes, up to the concurrency limit."
  (let ((tracker opencode-sessions--tracker))
    (while (and (opencode-sessions--tracker-queue tracker)
                (< (opencode-sessions--tracker-fetching tracker) opencode-sessions--max-fetches))
      (pcase-let ((`(,id . ,work) (pop (opencode-sessions--tracker-queue tracker))))
        (when (eq work (gethash id (opencode-sessions--table 'changes)))
          (cl-incf (opencode-sessions--tracker-fetching tracker))
          (opencode-sessions--find-roots id work))))))

(defun opencode-sessions--fetch-changes (session-id &optional force)
  "Find the worktrees the listed session SESSION-ID changed and count the changes.
The worktrees are looked up from the session's messages when it was
updated since they were last found, or when FORCE is non-nil.
Otherwise only their changes are counted again.  Nothing is looked up
while no overview buffer is open, since only they show changes."
  (when-let* (((opencode-sessions--buffers))
              (info (gethash session-id (opencode-sessions--table 'sessions))))
    (let ((work (with-memoization (gethash session-id (opencode-sessions--table 'changes))
                  (opencode-sessions--work-create)))
          (tracker opencode-sessions--tracker))
      (cond
       ((opencode-sessions--work-loading work)
        (when force
          (setf (opencode-sessions--work-again work) t)))
       ((and (not force)
             (opencode-sessions--work-updated work)
             (>= (opencode-sessions--work-updated work) (opencode-sessions--updated info)))
        (opencode-sessions--count-all work))
       (t
        (setf (opencode-sessions--work-loading work) t)
        (setf (opencode-sessions--tracker-queue tracker)
              (append (opencode-sessions--tracker-queue tracker)
                      (list (cons session-id work))))
        (opencode-sessions--start-fetches))))))

;;; Loading

(defun opencode-sessions--fetch-projects ()
  "Load the display names of the server's projects."
  (let ((tracker opencode-sessions--tracker))
    (opencode-client-projects
     (opencode-sessions--tracker-connection tracker)
     :success (lambda (&rest args)
                (when (eq tracker opencode-sessions--tracker)
                  (dolist (project (plist-get args :data))
                    (puthash (alist-get 'id project)
                             (or (alist-get 'name project)
                                 (abbreviate-file-name (alist-get 'canonical project)))
                             (opencode-sessions--table 'projects)))
                  (opencode-sessions--changed)))
     :error #'ignore)))

(defun opencode-sessions--fetch-prompts (session-id)
  "Load the pending permissions and forms of SESSION-ID."
  (let ((tracker opencode-sessions--tracker)
        (connection (opencode-sessions--tracker-connection opencode-sessions--tracker)))
    (cl-flet ((store (kind)
                (lambda (&rest args)
                  (when (eq tracker opencode-sessions--tracker)
                    (dolist (request (plist-get args :data))
                      (opencode-sessions--add-prompt (alist-get 'id request) session-id kind))
                    (opencode-sessions--changed)))))
      (opencode-client-session-permissions connection session-id
                                           :success (store 'permission) :error #'ignore)
      (opencode-client-session-forms connection session-id
                                     :success (store 'question) :error #'ignore))))

(defun opencode-sessions--fetch-running ()
  "Load the running sessions and their pending prompts."
  (let ((tracker opencode-sessions--tracker))
    (opencode-client-session-active
     (opencode-sessions--tracker-connection tracker)
     :success (lambda (&rest args)
                (when (eq tracker opencode-sessions--tracker)
                  (clrhash (opencode-sessions--table 'running))
                  (clrhash (opencode-sessions--table 'prompts))
                  (dolist (entry (plist-get args :data))
                    (let ((session-id (symbol-name (car entry))))
                      (puthash session-id "busy" (opencode-sessions--table 'running))
                      (opencode-sessions--learn session-id)
                      (opencode-sessions--fetch-prompts session-id)))
                  (opencode-sessions--changed)))
     :error #'ignore)))

(defun opencode-sessions--fetch-recent (callback)
  "Call CALLBACK with the unarchived top-level sessions updated within the window.
Pages through sessions, most recently updated first, until they are older
than `opencode-sessions-days' or `opencode-sessions-max' are loaded.  When
a page fails to load, CALLBACK is not called."
  (let* ((tracker opencode-sessions--tracker)
         (cutoff (- (* 1000 (float-time)) (* opencode-sessions-days 86400000)))
         (max (max 1 opencode-sessions-max))
         (page-size (min opencode-sessions--page-size max))
         (infos nil))
    (cl-labels
        ((recent-p (info) (>= (opencode-sessions--updated info) cutoff))
         (fetch (cursor)
           (opencode-client-sessions
            (opencode-sessions--tracker-connection tracker)
            :roots t
            :archived :exclude
            :limit page-size
            :cursor cursor
            :success (lambda (&rest args)
                       (when (eq tracker opencode-sessions--tracker)
                         (let ((page (plist-get args :data))
                               (next (plist-get args :cursor)))
                           (setq infos (nconc infos (cl-remove-if-not #'recent-p page)))
                           (if (and next
                                    (= (length page) page-size)
                                    (cl-every #'recent-p page)
                                    (< (length infos) max))
                               (fetch next)
                             (funcall callback (take max infos))))))
            :error (lambda (&rest _args)
                     (message "OpenCode: failed to list sessions")))))
      (fetch nil))))

(defun opencode-sessions--load ()
  "Load the sessions, projects, running sessions and changes from scratch.
Sessions that events added while the load ran are kept."
  (let ((tracker opencode-sessions--tracker)
        (started (* 1000 (float-time))))
    (opencode-sessions--fetch-projects)
    (opencode-sessions--fetch-recent
     (lambda (infos)
       (when (eq tracker opencode-sessions--tracker)
         (let ((sessions (opencode-sessions--table 'sessions))
               (loaded (make-hash-table :test 'equal)))
           (dolist (info infos)
             (puthash (alist-get 'id info) t loaded)
             (opencode-sessions--add info))
           (dolist (id (hash-table-keys sessions))
             (unless (or (gethash id loaded)
                         (>= (opencode-sessions--updated (gethash id sessions)) started))
               (remhash id sessions)))
           (dolist (id (hash-table-keys (opencode-sessions--table 'changes)))
             (unless (gethash id sessions)
               (remhash id (opencode-sessions--table 'changes))))
           (mapc #'opencode-sessions--fetch-changes (hash-table-keys sessions)))
         (opencode-sessions--fetch-running)
         (opencode-sessions--changed))))))

;;; Events

(defun opencode-sessions--handle-event (event data meta)
  "Update the tracker from server EVENT with DATA and META."
  (when opencode-sessions--tracker
    (let* ((payload (alist-get 'data data))
           (session-id (alist-get 'sessionID payload)))
      (pcase event
        ("server.connected"
         (when (plist-get meta :reconnect)
           (opencode-sessions--load)))
        ("session.created"
         (if-let* ((parent (alist-get 'parentID payload)))
             (puthash session-id parent (opencode-sessions--table 'parents))
           (opencode-sessions--learn session-id)))
        ("session.forked"
         (opencode-sessions--learn session-id))
        ("session.archived"
         (when (gethash session-id (opencode-sessions--table 'sessions))
           (opencode-sessions--hide session-id)))
        ("session.unarchived"
         (unless (stringp (gethash session-id (opencode-sessions--table 'parents)))
           (remhash session-id (opencode-sessions--table 'parents))
           (opencode-sessions--learn session-id)))
        ("session.deleted"
         (remhash session-id (opencode-sessions--table 'sessions))
         (remhash session-id (opencode-sessions--table 'parents))
         (remhash session-id (opencode-sessions--table 'running))
         (remhash session-id (opencode-sessions--table 'changes)))
        ("session.renamed"
         (opencode-sessions--update session-id 'title (alist-get 'title payload)))
        ("session.moved"
         (opencode-sessions--update session-id 'location (alist-get 'location payload))
         (when-let* ((project-id (alist-get 'projectID payload)))
           (opencode-sessions--update session-id 'projectID project-id)
           (unless (gethash project-id (opencode-sessions--table 'projects))
             (opencode-sessions--fetch-projects))))
        ("session.usage.updated"
         (opencode-sessions--update session-id 'cost (alist-get 'cost payload)))
        ("session.execution.started"
         (puthash session-id "busy" (opencode-sessions--table 'running))
         (opencode-sessions--learn session-id))
        ("session.retry.scheduled"
         (puthash session-id "retry" (opencode-sessions--table 'running)))
        ("session.step.started"
         (when (equal (gethash session-id (opencode-sessions--table 'running)) "retry")
           (puthash session-id "busy" (opencode-sessions--table 'running))))
        ((or "session.execution.succeeded" "session.execution.failed"
             "session.execution.interrupted")
         (remhash session-id (opencode-sessions--table 'running))
         (when-let* ((root (opencode-sessions--root session-id)))
           (opencode-sessions--refresh-info root)
           (opencode-sessions--fetch-changes root t)))
        ("session.viewed"
         (when-let* ((info (gethash session-id (opencode-sessions--table 'sessions))))
           (let ((time (copy-alist (alist-get 'time info)))
                 (idle (alist-get 'idle payload)))
             (setf (alist-get 'viewed time) (max idle (or (alist-get 'viewed time) 0)))
             (opencode-sessions--update session-id 'time time))))
        ("permission.asked"
         (opencode-sessions--add-prompt (alist-get 'id payload) session-id 'permission)
         (opencode-sessions--learn session-id))
        ("permission.replied"
         (remhash (alist-get 'requestID payload) (opencode-sessions--table 'prompts)))
        ("form.created"
         (let ((form (alist-get 'form payload)))
           (opencode-sessions--add-prompt (alist-get 'id form) (alist-get 'sessionID form)
                                          'question)
           (opencode-sessions--learn (alist-get 'sessionID form))))
        ((or "form.replied" "form.cancelled")
         (remhash (alist-get 'id payload) (opencode-sessions--table 'prompts))))
      (opencode-sessions--changed))))

(dolist (event '("server.connected" "session.created" "session.forked" "session.deleted"
                 "session.archived" "session.unarchived"
                 "session.renamed" "session.moved" "session.usage.updated"
                 "session.execution.started" "session.retry.scheduled"
                 "session.step.started" "session.execution.succeeded"
                 "session.execution.failed" "session.execution.interrupted"
                 "session.viewed" "permission.asked" "permission.replied" "form.created"
                 "form.replied" "form.cancelled"))
  (opencode-sse-register-handler event #'opencode-sessions--handle-event))

;;; Viewed state

(defun opencode-sessions--mark-viewed (session-id)
  "Tell the server the user saw the latest run of SESSION-ID, if it is unread."
  (let* ((info (gethash session-id (opencode-sessions--table 'sessions)))
         (idle (alist-get 'idle (alist-get 'time info)))
         (viewed (opencode-sessions--table 'viewed)))
    (when (and (opencode-sessions--unread-p info)
               (not (equal (gethash session-id viewed) idle)))
      (puthash session-id idle viewed)
      (opencode-client-session-view
       (opencode-sessions--tracker-connection opencode-sessions--tracker)
       session-id idle
       :success #'ignore
       :error (lambda (&rest _args) (remhash session-id viewed))))))

(defun opencode-sessions--view-selected (&rest _)
  "Mark the session shown in the selected window as viewed."
  (when-let* ((opencode-sessions--tracker)
              (session (buffer-local-value 'opencode-session--session
                                           (window-buffer (selected-window))))
              (root (opencode-sessions--root (opencode-session-id session))))
    (opencode-sessions--mark-viewed root)))

;;; Tracking

(defun opencode-sessions--start (connection)
  "Start tracking the sessions of CONNECTION's server."
  (setq opencode-sessions--tracker (opencode-sessions--tracker-create :connection connection))
  (add-hook 'window-selection-change-functions #'opencode-sessions--view-selected)
  (add-hook 'window-buffer-change-functions #'opencode-sessions--view-selected)
  (opencode-sessions--load))

(defun opencode-sessions--ensure-tracking (&optional reload)
  "Connect to OpenCode and track its sessions.
When already tracking and RELOAD is non-nil, load the sessions again."
  (opencode-run-server
   default-directory
   (lambda (connection)
     (cond
      ((not opencode-sessions--tracker) (opencode-sessions--start connection))
      (reload (opencode-sessions--load))
      (t (mapc #'opencode-sessions--fetch-changes
               (hash-table-keys (opencode-sessions--table 'sessions)))))
     (opencode-sessions--render))))

(defun opencode-sessions--stop-unless-needed (&optional closing-buffer)
  "Stop tracking sessions when neither the overview nor the indicator needs it.
CLOSING-BUFFER is an overview buffer being killed."
  (unless (or opencode-sessions-indicator-mode
              (remq closing-buffer (opencode-sessions--buffers)))
    (setq opencode-sessions--tracker nil)
    (setq opencode-sessions--indicator "")
    (remove-hook 'window-selection-change-functions #'opencode-sessions--view-selected)
    (remove-hook 'window-buffer-change-functions #'opencode-sessions--view-selected)))

;;; Rendering

(defun opencode-sessions--indicator-text ()
  "Return the mode line text counting sessions that need attention."
  (let ((waiting 0) (running 0) (done 0))
    (dolist (id (hash-table-keys (opencode-sessions--table 'sessions)))
      (pcase (opencode-sessions--state id)
        ((or 'permission 'question) (cl-incf waiting))
        ((or 'running 'retrying) (cl-incf running))
        ((or 'done 'failed 'stopped) (cl-incf done))))
    (let ((segments (delq nil (list (and (> waiting 0)
                                         (propertize (format "%d waiting" waiting) 'face 'error))
                                    (and (> running 0) (format "%d running" running))
                                    (and (> done 0)
                                         (propertize (format "%d done" done) 'face 'success))))))
      (if (null segments)
          ""
        (propertize (format " [OC: %s]" (string-join segments ", "))
                    'help-echo "OpenCode sessions\nmouse-1: list sessions"
                    'mouse-face 'mode-line-highlight
                    'local-map (make-mode-line-mouse-map 'mouse-1 #'opencode-sessions))))))

(defun opencode-sessions--buffers ()
  "Return the open overview buffers: the full overview and the sidebar."
  (delq nil (mapcar #'get-buffer (list opencode-sessions--buffer-name
                                       opencode-sessions--sidebar-buffer-name))))

(defun opencode-sessions--render ()
  "Redraw the overview buffers and the mode line indicator."
  (when opencode-sessions--tracker
    (when opencode-sessions-indicator-mode
      (setq opencode-sessions--indicator (opencode-sessions--indicator-text))
      (force-mode-line-update t))
    (dolist (buffer (opencode-sessions--buffers))
      (with-current-buffer buffer
        (opencode-sessions--render-buffer)))))

(defun opencode-sessions--render-buffer ()
  "Redraw the current overview buffer."
  ;; Each buffer gets its own lists, since sorting reorders them in place.
  (let ((groups (opencode-sessions--groups opencode-sessions--folded)))
    (when (boundp 'tabulated-list-groups)
      (setq tabulated-list-groups groups))
    (setq tabulated-list-entries (apply #'append (mapcar #'cdr groups)))
    (opencode-sessions--forget-unlisted-flags)
    (opencode-sessions--print)))

(defun opencode-sessions--line-position (line)
  "Return the position of the start of LINE in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun opencode-sessions--row-position (id)
  "Return the position of the row of session ID, or nil."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp)) (not (equal (tabulated-list-get-id) id)))
      (forward-line))
    (unless (eobp) (point))))

(defun opencode-sessions--place (pos)
  "Return where POS is, as (ID OFFSET COLUMN LINE).
ID is the session on POS's line, OFFSET how many lines into its row the
line is, and LINE the line number, for lines without a session."
  (save-excursion
    (goto-char pos)
    (let ((line (line-number-at-pos))
          (column (current-column))
          (id (tabulated-list-get-id)))
      (opencode-sessions--row-start)
      (list id (- line (line-number-at-pos)) column line))))

(defun opencode-sessions--place-position (place)
  "Return the position of PLACE, from `opencode-sessions--place'."
  (pcase-let ((`(,id ,offset ,column ,line) place))
    (save-excursion
      (if-let* ((start (and id (opencode-sessions--row-position id))))
          (progn
            (goto-char start)
            (forward-line offset)
            (while (and (> (point) start) (not (equal (tabulated-list-get-id) id)))
              (forward-line -1)))
        (goto-char (opencode-sessions--line-position line)))
      (move-to-column column)
      (point))))

(defun opencode-sessions--print ()
  "Reprint the current overview buffer, keeping point and each window in place.
Reprinting erases the buffer, which would otherwise send point to the
top, and with it every window showing the buffer."
  (let ((point (opencode-sessions--place (point)))
        (windows (mapcar (lambda (window)
                           (list window
                                 (line-number-at-pos (window-start window))
                                 (opencode-sessions--place (window-point window))))
                         (get-buffer-window-list nil nil t))))
    (tabulated-list-print)
    (goto-char (opencode-sessions--place-position point))
    (pcase-dolist (`(,window ,start ,place) windows)
      (set-window-start window (opencode-sessions--line-position start) t)
      (set-window-point window (opencode-sessions--place-position place)))))

(defun opencode-sessions--changed ()
  "Redraw the overview and the indicator soon, coalescing bursts of changes."
  (unless opencode-sessions--refresh-timer
    (setq opencode-sessions--refresh-timer
          (run-with-timer 0.1 nil
                          (lambda ()
                            (setq opencode-sessions--refresh-timer nil)
                            (opencode-sessions--render))))))

;;; Overview buffer

(defun opencode-sessions--entry-info (entry)
  "Return the session info of tabulated list ENTRY."
  (gethash (car entry) (opencode-sessions--table 'sessions)))

(defun opencode-sessions--state-less-p (a b)
  "Return non-nil when entry A is more urgent than entry B."
  (< (opencode-sessions--rank (opencode-sessions--state (car a)))
     (opencode-sessions--rank (opencode-sessions--state (car b)))))

(defun opencode-sessions--cost-less-p (a b)
  "Return non-nil when entry A cost less than entry B."
  (< (or (alist-get 'cost (opencode-sessions--entry-info a)) 0)
     (or (alist-get 'cost (opencode-sessions--entry-info b)) 0)))

(defun opencode-sessions--updated-less-p (a b)
  "Return non-nil when entry A was updated more recently than entry B."
  (> (opencode-sessions--updated (opencode-sessions--entry-info a))
     (opencode-sessions--updated (opencode-sessions--entry-info b))))

(defvar-keymap opencode-sessions-mode-map
  :doc "Keymap for `opencode-sessions-mode'."
  :parent tabulated-list-mode-map
  "RET" #'opencode-sessions-open
  "TAB" #'opencode-sessions-toggle-group
  "o" #'opencode-sessions-open-other-window
  "C-o" #'opencode-sessions-display
  "n" #'opencode-sessions-next
  "p" #'opencode-sessions-previous
  "R" #'opencode-sessions-rename
  "c" #'opencode-sessions-new
  "d" #'opencode-sessions-flag-delete
  "a" #'opencode-sessions-flag-archive
  "u" #'opencode-sessions-unmark
  "DEL" #'opencode-sessions-unmark-backward
  "U" #'opencode-sessions-unmark-all
  "x" #'opencode-sessions-execute)

(defun opencode-sessions--setup-evil ()
  "Bind the overview's commands in Evil's normal state.
Keys that Evil uses for motions get other bindings: `go' opens in
another window, `gO' shows without selecting, and `gr' reloads."
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal opencode-sessions-mode-map
      (kbd "RET") #'opencode-sessions-open
      (kbd "TAB") #'opencode-sessions-toggle-group
      "go" #'opencode-sessions-open-other-window
      "gO" #'opencode-sessions-display
      "R" #'opencode-sessions-rename
      "c" #'opencode-sessions-new
      "d" #'opencode-sessions-flag-delete
      "a" #'opencode-sessions-flag-archive
      "u" #'opencode-sessions-unmark
      (kbd "DEL") #'opencode-sessions-unmark-backward
      "U" #'opencode-sessions-unmark-all
      "x" #'opencode-sessions-execute
      "gr" #'revert-buffer
      "q" #'quit-window)))

(define-derived-mode opencode-sessions-mode tabulated-list-mode "OpenCode Sessions"
  "Mode listing OpenCode sessions across all projects.
Sessions are grouped by project.  Within each project, sessions waiting
on you come first, then finished runs you have not looked at, then
running sessions.  Fold a project to hide its sessions.

As in Dired, flag sessions to delete or archive, then act on them all at
once.

\\{opencode-sessions-mode-map}"
  :group 'emacs-opencode
  (setq tabulated-list-format
        [("Status" 10 opencode-sessions--state-less-p)
         ("Title" 40 t)
         ("Directory" 32 t)
         ("Changes" 12 nil)
         ("Cost" 7 opencode-sessions--cost-less-p :right-align t)
         ("Updated" 8 opencode-sessions--updated-less-p :right-align t)])
  (setq tabulated-list-padding 1)
  (setq opencode-sessions--flagged (make-hash-table :test 'equal))
  (setq opencode-sessions--folded (make-hash-table :test 'equal))
  (setq tabulated-list-printer #'opencode-sessions--print-entry)
  (when (bound-and-true-p evil-mode)
    (opencode-sessions--setup-evil))
  (add-hook 'tabulated-list-revert-hook #'opencode-sessions--reload nil t)
  (add-hook 'kill-buffer-hook
            (lambda () (opencode-sessions--stop-unless-needed (current-buffer)))
            nil t)
  (tabulated-list-init-header))

(defvar-keymap opencode-sessions-sidebar-mode-map
  :doc "Keymap for `opencode-sessions-sidebar-mode'.
Sidebar rows have no columns, so the column commands are unbound."
  "S" #'undefined
  "{" #'undefined
  "}" #'undefined)

(define-derived-mode opencode-sessions-sidebar-mode opencode-sessions-mode "OpenCode Sidebar"
  "Mode listing OpenCode sessions in a narrow side window.
It works like `opencode-sessions-mode', but each session takes three
lines: its title, then its status, changes, cost and age, then where it
worked.  Sessions open in the most recently used other window.

\\{opencode-sessions-sidebar-mode-map}"
  :group 'emacs-opencode
  (setq tabulated-list-printer #'opencode-sessions--print-sidebar-entry)
  (setq header-line-format nil))

(defun opencode-sessions--reload ()
  "Reconnect to OpenCode if needed and load the sessions again."
  (opencode-sessions--ensure-tracking t))

(defun opencode-sessions--buffer (name mode)
  "Return the overview buffer called NAME, in MODE."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (eq major-mode mode)
        (funcall mode)))
    buffer))

;;;###autoload
(defun opencode-sessions ()
  "List OpenCode sessions across all projects and what each needs from you."
  (interactive)
  (pop-to-buffer-same-window
   (opencode-sessions--buffer opencode-sessions--buffer-name #'opencode-sessions-mode))
  (opencode-sessions--ensure-tracking))

;;;###autoload
(defun opencode-sessions-sidebar ()
  "Show the OpenCode session list in a sidebar, or hide it."
  (interactive)
  (if-let* ((buffer (get-buffer opencode-sessions--sidebar-buffer-name))
            ((get-buffer-window buffer)))
      (kill-buffer buffer)
    (select-window
     (display-buffer-in-side-window
      (opencode-sessions--buffer opencode-sessions--sidebar-buffer-name
                                 #'opencode-sessions-sidebar-mode)
      `((side . ,opencode-sessions-sidebar-side)
        (slot . 0)
        (window-width . ,opencode-sessions-sidebar-width)
        (preserve-size . (t . nil))
        (dedicated . t)
        (window-parameters . ((no-delete-other-windows . t))))))
    (opencode-sessions--ensure-tracking)))

(defun opencode-sessions--select-main-window ()
  "Select the most recently used other window when in a side window.
This way, sessions opened from the sidebar replace a regular window."
  (when-let* (((window-parameter nil 'window-side))
              (window (get-mru-window nil nil t)))
    (select-window window)))

(defun opencode-sessions--info-at-point ()
  "Return the info of the session on the current line."
  (or (when-let* ((id (tabulated-list-get-id)))
        (gethash id (opencode-sessions--table 'sessions)))
      (user-error "No session on this line")))

(defun opencode-sessions--connection ()
  "Return the connection of the tracked server."
  (or (and opencode-sessions--tracker
           (opencode-sessions--tracker-connection opencode-sessions--tracker))
      (user-error "Not connected to OpenCode")))

(defun opencode-sessions--open (action &optional main-window)
  "Open the session at point, displaying its buffer with ACTION.
When MAIN-WINDOW is non-nil, leave a side window first."
  (let ((info (opencode-sessions--info-at-point))
        (display-buffer-overriding-action action))
    (when main-window
      (opencode-sessions--select-main-window))
    (opencode--ensure-session-buffer (opencode-session--session-from-info info)
                                     (opencode-sessions--connection))))

(defun opencode-sessions-open ()
  "Open the session at point in this window.
From the sidebar, open it in the most recently used other window."
  (interactive)
  (opencode-sessions--open '(display-buffer-same-window) t))

(defun opencode-sessions-open-other-window ()
  "Open the session at point in another window."
  (interactive)
  (opencode-sessions--open '((display-buffer-use-some-window display-buffer-pop-up-window)
                             (inhibit-same-window . t))))

(defun opencode-sessions-display ()
  "Show the session at point in another window without selecting it."
  (interactive)
  (save-selected-window
    (opencode-sessions-open-other-window)))

(defun opencode-sessions-rename (title)
  "Rename the session at point to TITLE."
  (interactive
   (list (read-string "Rename session to: "
                      (alist-get 'title (opencode-sessions--info-at-point)))))
  (opencode-client-session-rename
   (opencode-sessions--connection)
   (alist-get 'id (opencode-sessions--info-at-point))
   title
   :success #'ignore
   :error (lambda (&rest _args) (message "OpenCode: failed to rename session"))))

(defun opencode-sessions--row-start ()
  "Move to the first line of the session row at point."
  (forward-line 0)
  (when-let* ((id (tabulated-list-get-id)))
    (while (and (not (bobp)) (equal id (tabulated-list-get-id (pos-bol 0))))
      (forward-line -1))))

(defun opencode-sessions--next-session ()
  "Move to the first line of the next session row."
  (let ((id (tabulated-list-get-id)))
    (forward-line)
    (while (and (not (eobp)) (member (tabulated-list-get-id) (list nil id)))
      (forward-line))))

(defun opencode-sessions--previous-session ()
  "Move to the first line of the previous session row.
Return nil and stay on the current row when there is none."
  (opencode-sessions--row-start)
  (let ((start (point)))
    (forward-line -1)
    (while (and (not (bobp)) (not (tabulated-list-get-id)))
      (forward-line -1))
    (if (tabulated-list-get-id)
        (progn (opencode-sessions--row-start) t)
      (goto-char start)
      nil)))

(defun opencode-sessions--project-at-point ()
  "Return the ID of the project whose heading or session is on this line."
  (if-let* ((id (tabulated-list-get-id)))
      (alist-get 'projectID (gethash id (opencode-sessions--table 'sessions)))
    (get-text-property (pos-bol) 'opencode-sessions-project)))

(defun opencode-sessions-toggle-group ()
  "Hide or show the sessions of the project at point.
Point moves to the project's heading."
  (interactive)
  (unless (boundp 'tabulated-list-groups)
    (user-error "Folding projects needs Emacs 30 or later"))
  (let ((project (or (opencode-sessions--project-at-point)
                     (user-error "No project on this line"))))
    (if (gethash project opencode-sessions--folded)
        (remhash project opencode-sessions--folded)
      (puthash project t opencode-sessions--folded))
    (opencode-sessions--render-buffer)
    (goto-char (point-min))
    (when-let* ((match (text-property-search-forward 'opencode-sessions-project project t)))
      (goto-char (prop-match-beginning match)))))

(defun opencode-sessions-next ()
  "Move to the next session."
  (interactive)
  (opencode-sessions--next-session))

(defun opencode-sessions-previous ()
  "Move to the previous session."
  (interactive)
  (opencode-sessions--previous-session))

(defconst opencode-sessions--flag-tags
  `((delete . ,(propertize "D" 'face 'error))
    (archive . ,(propertize "A" 'face 'warning)))
  "Tag shown on the rows of sessions flagged for each action.")

(defun opencode-sessions--flag (action)
  "Flag the session at point for ACTION, `delete' or `archive'.
When ACTION is nil, unflag it."
  (let ((id (alist-get 'id (opencode-sessions--info-at-point))))
    (if action
        (puthash id action opencode-sessions--flagged)
      (remhash id opencode-sessions--flagged))
    (save-excursion
      (opencode-sessions--row-start)
      (tabulated-list-put-tag (alist-get action opencode-sessions--flag-tags " ")))))

(defun opencode-sessions--tag-if-flagged (id start)
  "Tag the row of session ID, which starts at START, when it is flagged."
  (when-let* ((action (gethash id opencode-sessions--flagged)))
    (save-excursion
      (goto-char start)
      (tabulated-list-put-tag (alist-get action opencode-sessions--flag-tags)))))

(defun opencode-sessions--print-entry (id columns)
  "Insert the row for session ID with COLUMNS, tagged when it is flagged."
  (let ((start (point)))
    (tabulated-list-print-entry id columns)
    (opencode-sessions--tag-if-flagged id start)))

(defun opencode-sessions--print-sidebar-entry (id columns)
  "Insert the sidebar row for session ID with COLUMNS, tagged when it is flagged.
The title goes on the first line, then the status, changes, cost and age,
then the directory."
  (pcase-let ((`[,state ,title ,directory ,diff ,cost ,age] columns)
              (start (point))
              (indent (make-string (+ tabulated-list-padding 3) ?\s)))
    (insert (make-string tabulated-list-padding ?\s) " " title "\n")
    (dolist (line (list (string-join (seq-remove #'string-empty-p (list state diff cost age))
                                     "  ")
                        (propertize directory 'face 'shadow)))
      (unless (string-empty-p line)
        (insert indent line "\n")))
    (add-text-properties start (point) `(tabulated-list-id ,id tabulated-list-entry ,columns))
    (opencode-sessions--tag-if-flagged id start)))

(defun opencode-sessions--forget-unlisted-flags ()
  "Unflag sessions that are no longer listed."
  (let ((sessions (opencode-sessions--table 'sessions)))
    (dolist (id (hash-table-keys opencode-sessions--flagged))
      (unless (gethash id sessions)
        (remhash id opencode-sessions--flagged)))))

(defun opencode-sessions-flag-delete ()
  "Flag the session at point for deletion, with its subagent sessions."
  (interactive)
  (opencode-sessions--flag 'delete)
  (opencode-sessions--next-session))

(defun opencode-sessions-flag-archive ()
  "Flag the session at point for archiving."
  (interactive)
  (opencode-sessions--flag 'archive)
  (opencode-sessions--next-session))

(defun opencode-sessions-unmark ()
  "Remove the flag from the session at point."
  (interactive)
  (opencode-sessions--flag nil)
  (opencode-sessions--next-session))

(defun opencode-sessions-unmark-backward ()
  "Move to the previous session and remove its flag."
  (interactive)
  (if (opencode-sessions--previous-session)
      (opencode-sessions--flag nil)
    (user-error "No previous session")))

(defun opencode-sessions-unmark-all ()
  "Remove every flag."
  (interactive)
  (clrhash opencode-sessions--flagged)
  (tabulated-list-print t))

(defun opencode-sessions-new ()
  "Start a new session in the project of the session at point."
  (interactive)
  (let ((default-directory
         (file-name-as-directory
          (opencode-sessions--directory (opencode-sessions--info-at-point)))))
    (opencode-sessions--select-main-window)
    (call-interactively #'opencode)))

(defun opencode-sessions--title (id)
  "Return the title of the listed session ID."
  (or (alist-get 'title (gethash id (opencode-sessions--table 'sessions))) id))

(defun opencode-sessions--describe (verb ids)
  "Describe applying VERB to the sessions IDS, naming them."
  (format "%s %d session%s (%s)" verb (length ids) (if (cdr ids) "s" "")
          (mapconcat #'opencode-sessions--title ids ", ")))

(defun opencode-sessions--bury-session-buffer (session-id)
  "Bury the buffer of SESSION-ID, if any, and replace it in its windows."
  (when-let* ((buffer (opencode-session--buffer-for-session session-id))
              ((buffer-live-p buffer)))
    (dolist (window (get-buffer-window-list buffer nil t))
      (switch-to-prev-buffer window 'bury))
    (bury-buffer buffer)))

(defun opencode-sessions--execute-prompt (archives deletes)
  "Return the question confirming archiving ARCHIVES and deleting DELETES."
  (concat (string-join
           (delq nil (list (and archives (opencode-sessions--describe "Archive" archives))
                           (and deletes (opencode-sessions--describe
                                         (if archives "delete" "Delete") deletes))))
           " and ")
          "? "))

(defun opencode-sessions-execute ()
  "Archive and delete the flagged sessions, after confirming.
Deleting also deletes a session's subagent sessions.  Archiving buries
the session's buffer when `opencode-sessions-archive-buries-buffer' is
non-nil.  The list reloads once every request finishes."
  (interactive)
  (let ((connection (opencode-sessions--connection))
        (tracker opencode-sessions--tracker)
        (flagged opencode-sessions--flagged)
        archives deletes)
    (maphash (lambda (id action)
               (if (eq action 'archive) (push id archives) (push id deletes)))
             flagged)
    (unless (or archives deletes)
      (user-error "No sessions are flagged"))
    (when (funcall (if deletes #'yes-or-no-p #'y-or-n-p)
                   (opencode-sessions--execute-prompt archives deletes))
      (let ((pending (+ (length archives) (length deletes))))
        (cl-flet ((request (id verb send)
                    (let ((title (opencode-sessions--title id)))
                      (cl-flet ((finish ()
                                  (when (and (zerop (cl-decf pending))
                                             (eq tracker opencode-sessions--tracker))
                                    (opencode-sessions--load))))
                        (funcall
                         send
                         :success (lambda (&rest _args)
                                    (remhash id flagged)
                                    (when (and (equal verb "archive")
                                               opencode-sessions-archive-buries-buffer)
                                      (opencode-sessions--bury-session-buffer id))
                                    (when (eq tracker opencode-sessions--tracker)
                                      (if (equal verb "archive")
                                          (opencode-sessions--hide id)
                                        (remhash id (opencode-sessions--table 'sessions)))
                                      (opencode-sessions--changed))
                                    (finish))
                         :error (lambda (&rest _args)
                                  (message "OpenCode: failed to %s \"%s\"" verb title)
                                  (finish)))))))
          (dolist (id archives)
            (request id "archive"
                     (lambda (&rest callbacks)
                       (apply #'opencode-client-session-archive connection id t callbacks))))
          (dolist (id deletes)
            (request id "delete"
                     (lambda (&rest callbacks)
                       (apply #'opencode-client-session-delete connection id callbacks)))))))))

;;; Mode line indicator

;;;###autoload
(define-minor-mode opencode-sessions-indicator-mode
  "Show in the mode line how many OpenCode sessions need attention.
The indicator counts sessions waiting on a permission or question,
running sessions, and finished runs you have not looked at.  Click it
to list the sessions.  Turning it on connects to OpenCode."
  :global t
  :group 'emacs-opencode
  (if opencode-sessions-indicator-mode
      (progn
        ;; A list starting with a symbol is a mode line conditional, so
        ;; the list must start with a string.
        (unless (consp global-mode-string)
          (setq global-mode-string (list (or global-mode-string ""))))
        (add-to-list 'global-mode-string 'opencode-sessions--indicator t)
        (opencode-sessions--ensure-tracking))
    (setq global-mode-string (delq 'opencode-sessions--indicator global-mode-string))
    (opencode-sessions--stop-unless-needed)))

(provide 'emacs-opencode-sessions)

;;; emacs-opencode-sessions.el ends here
