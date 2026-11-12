;;; emacs-opencode-magit-test.el --- Tests for the Magit integration  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-opencode-magit)

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
  (opencode-changes-root-create
   :directory worktree :files (or files '("a.txt"))
   :steps (list (opencode-changes-step-create :time 1 :start from :end to))))

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

(ert-deftest test-opencode-magit/context-in-session-overview ()
  "In the session overview, the session at point is the current session."
  (let ((opencode-sessions--tracker (opencode-sessions--tracker-create :connection 'conn)))
    (opencode-sessions--add '((id . "s1") (title . "First")))
    (with-temp-buffer
      (opencode-sessions-mode)
      (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda (&rest _) "s1")))
        (should (equal (opencode-magit--context)
                       '(:connection conn :session-id "s1" :title "First")))))))

(ert-deftest test-opencode-magit/overview-diff-key ()
  "In the session overview, `=' diffs the worktree of the session at point."
  (should (eq (keymap-lookup opencode-sessions-mode-map "=") #'opencode-magit-diff)))

(provide 'emacs-opencode-magit-test)

;;; emacs-opencode-magit-test.el ends here
