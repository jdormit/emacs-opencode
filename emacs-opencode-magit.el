;;; emacs-opencode-magit.el --- Magit views of OpenCode session changes  -*- lexical-binding: t; -*-

;; Optional: load with (require 'emacs-opencode-magit).  Requires Magit.

(require 'cl-lib)
(require 'subr-x)
(require 'magit)
(require 'emacs-opencode-changes)
(require 'emacs-opencode-client)
(require 'emacs-opencode-session)
(require 'emacs-opencode-session-vars)
(require 'emacs-opencode-message)
(require 'emacs-opencode-session-mode)
(require 'emacs-opencode-sessions)

(declare-function opencode--ensure-session-buffer "emacs-opencode"
                  (session connection &optional on-ready))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))

(defvar-local opencode-magit--overview nil
  "Plist describing the session an overview buffer shows.
Keys are :connection, :session-id, :title, :roots, and :tree.")

;;; Session changes

(defun opencode-magit--context ()
  "Return the session the current buffer shows as a plist.
The plist has :connection, :session-id, and :title."
  (cond
   ((derived-mode-p 'opencode-magit-overview-mode)
    (list :connection (plist-get opencode-magit--overview :connection)
          :session-id (plist-get opencode-magit--overview :session-id)
          :title (plist-get opencode-magit--overview :title)))
   ((derived-mode-p 'opencode-sessions-mode)
    (let ((info (opencode-sessions--info-at-point)))
      (list :connection (opencode-sessions--connection)
            :session-id (alist-get 'id info)
            :title (or (alist-get 'title info) (alist-get 'id info)))))
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
changes as in `opencode-changes-tree-steps'.  CALLBACK is called with the
roots and the fetched session tree."
  (let ((buffer (current-buffer)))
    (message "OpenCode: finding changed worktrees...")
    (opencode-changes-fetch-tree
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
                         (opencode-changes-roots (opencode-changes-tree-steps tree turn))
                         tree)
              (error (message "OpenCode: %s" (error-message-string err))))))))
     (lambda () (message "OpenCode: failed to load the session's changes")))))

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
                                              (opencode-changes-root-directory root))
                                             (mapconcat (lambda (id) (alist-get id titles nil nil #'equal))
                                                        (opencode-changes-root-sessions root)
                                                        ", "))
                                     root))
                             roots)))
        (cdr (assoc (completing-read prompt choices nil t) choices)))
    (car roots)))

(defun opencode-magit--with-root (turn prompt function)
  "Call FUNCTION with a root changed by the current session and its title.
TURN restricts the changes as in `opencode-changes-tree-steps'.  When the
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
  (let ((directory (opencode-changes-root-directory root)))
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
                             (and session-files (opencode-changes-root-files root)))))

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
    (unless (cl-subsetp changed (opencode-changes-root-files root) :test #'equal)
      (opencode-changes-root-files root))))

(defun opencode-magit--snapshots (root)
  "Return (GIT-DIR FROM . TO) for the snapshots spanning ROOT's steps."
  (pcase-let* ((directory (opencode-magit--existing-directory root))
               (`(,from . ,to)
                (or (opencode-changes-snapshot-range root)
                    (user-error "OpenCode recorded no snapshots in %s"
                                (abbreviate-file-name directory)))))
    (unless (opencode-changes-root-files root)
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
               (default-directory (opencode-changes-root-directory root))
               (start (opencode-magit--snapshot-commit git-dir from t)))
    (magit-diff-working-tree start (car (magit-diff-arguments 'magit-diff-mode))
                             (opencode-magit--filter-files root start))))

(defun opencode-magit--turn-diff (root)
  "Show the changes OpenCode recorded in ROOT during a turn.
Return the Magit diff buffer."
  (pcase-let* ((`(,git-dir ,from . ,to) (opencode-magit--snapshots root))
               (default-directory (opencode-changes-root-directory root))
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
        (let ((directory (opencode-changes-root-directory root)))
          (magit-insert-section (opencode-changes-root root)
            (magit-insert-heading
              (propertize (abbreviate-file-name directory) 'font-lock-face 'magit-section-heading)
              "  "
              (propertize (opencode-magit--describe-worktree directory)
                          'font-lock-face 'magit-branch-local)
              "  "
              (mapconcat (lambda (id) (alist-get id titles nil nil #'equal))
                         (opencode-changes-root-sessions root)
                         ", "))
            (dolist (file (opencode-changes-root-files root))
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
    (while (and section (not (eq (oref section type) 'opencode-changes-root)))
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
         (ids (opencode-changes-root-sessions (opencode-magit--root-at-point)))
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
(keymap-set opencode-sessions-mode-map "C-c C-d" opencode-magit-map)
(keymap-set opencode-sessions-mode-map "=" #'opencode-magit-diff)
(with-eval-after-load 'evil
  (evil-define-key* 'normal opencode-sessions-mode-map "=" #'opencode-magit-diff))

(provide 'emacs-opencode-magit)

;;; emacs-opencode-magit.el ends here
