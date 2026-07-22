;;; emacs-opencode-session-render.el --- Session message rendering  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'ansi-color)
(require 'emacs-opencode-session-vars)
(require 'emacs-opencode-message)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-session)
(require 'emacs-opencode-session-fontify)
(require 'emacs-opencode-sse-profile)

(defvar-local opencode-session--retry-banner-start nil
  "Marker for the start of the inline retry banner region.

Nil when no banner is currently rendered.  The banner sits in the
message log just above the input prompt area and is replaced or
deleted on every status update.")

(defvar-local opencode-session--retry-banner-end nil
  "Marker for the end of the inline retry banner region.")

(declare-function opencode--ensure-session-buffer "emacs-opencode")

(defcustom opencode-session-show-reasoning nil
  "When non-nil, display reasoning/thinking blocks in the session buffer."
  :type 'boolean
  :group 'emacs-opencode)

(defface opencode-session-user-face
  '((t :inherit default))
  "Face used for user messages."
  :group 'emacs-opencode)

(defface opencode-session-user-prefix-face
  '((t :inherit font-lock-constant-face))
  "Face used for the user message line indicator."
  :group 'emacs-opencode)

(defface opencode-session-assistant-face
  '((t :inherit default))
  "Face used for assistant messages."
  :group 'emacs-opencode)

(defface opencode-session-reasoning-face
  '((t :inherit shadow :slant italic))
  "Face used for reasoning/thinking blocks."
  :group 'emacs-opencode)

(defface opencode-session-tool-face
  '((t :inherit shadow))
  "Face used for tool call lines."
  :group 'emacs-opencode)

(defface opencode-session-compaction-face
  '((t :inherit shadow :slant italic))
  "Face used for session compaction boundary markers."
  :group 'emacs-opencode)

(defcustom opencode-session-bash-output-max-lines 10
  "Maximum number of shell output lines to show before collapsing."
  :type 'integer
  :group 'emacs-opencode)

;;; Collapse / expand for long tool output

(defvar opencode-session--collapse-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'opencode-session-toggle-collapse)
    (define-key map (kbd "TAB") #'opencode-session-toggle-collapse)
    (define-key map [mouse-1] #'opencode-session-toggle-collapse)
    map)
  "Keymap for collapsible tool output indicators.")

(defun opencode-session--collapse-symbol (part-id)
  "Return a unique invisibility symbol for PART-ID."
  (intern (format "opencode-collapse-%s" part-id)))

(defun opencode-session-toggle-collapse ()
  "Toggle collapsed tool output at point."
  (interactive)
  (let ((sym (get-text-property (point) 'opencode-collapse-sym)))
    (when sym
      (if (memq sym buffer-invisibility-spec)
          (progn
            ;; Expanding: remove from spec and remember user choice.
            (remove-from-invisibility-spec sym)
            (unless opencode-session--expanded-collapse-syms
              (setq-local opencode-session--expanded-collapse-syms
                          (make-hash-table :test 'eq)))
            (puthash sym t opencode-session--expanded-collapse-syms)
            (let ((inhibit-read-only t))
              (opencode-session--update-collapse-indicator (point) t)))
        ;; Collapsing: add back to spec and forget user expansion.
        (add-to-invisibility-spec sym)
        (when opencode-session--expanded-collapse-syms
          (remhash sym opencode-session--expanded-collapse-syms))
        (let ((inhibit-read-only t))
          (opencode-session--update-collapse-indicator (point) nil))))))

(defun opencode-session--update-collapse-indicator (pos expanded)
  "Update the collapse indicator text near POS for EXPANDED state."
  (let* ((sym (get-text-property pos 'opencode-collapse-sym))
         (count (get-text-property pos 'opencode-collapse-count)))
    (when (and sym count)
      (save-excursion
        ;; Find the indicator line by scanning for matching symbol
        (goto-char (point-min))
        (let ((found nil))
          (while (and (not found) (< (point) (point-max)))
            (if (eq (get-text-property (point) 'opencode-collapse-indicator) sym)
                (setq found t)
              (goto-char (next-single-property-change
                          (point) 'opencode-collapse-indicator nil (point-max)))))
          (when found
            (let* ((line-start (line-beginning-position))
                   (line-end (line-end-position))
                   (new-text (if expanded
                                 "▼ collapse"
                               (format "▶ %d more lines" count))))
              (delete-region line-start line-end)
              (insert (propertize new-text
                                  'opencode-part-type "tool"
                                  'opencode-collapse-sym sym
                                  'opencode-collapse-count count
                                  'opencode-collapse-indicator sym
                                  'keymap opencode-session--collapse-keymap
                                  'mouse-face 'highlight)))))))))

;;; Task tool interactivity

(defvar opencode-session--task-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'opencode-session-open-subagent)
    (define-key map (kbd "TAB") #'opencode-session-open-subagent)
    (define-key map [mouse-1] #'opencode-session-open-subagent)
    map)
  "Keymap for clickable task tool blocks.")

(defun opencode-session-open-subagent ()
  "Open the subagent session at point."
  (interactive)
  (let ((session-id (get-text-property (point) 'opencode-subagent-session-id)))
    (if session-id
        (let ((connection opencode-session--connection))
          (if connection
              (opencode--ensure-session-buffer
               (opencode-session-create :id session-id)
               connection)
            (message "OpenCode: no connection available")))
      (message "OpenCode: no subagent session at point"))))

;;; Strip ANSI escape sequences

(defun opencode-session--strip-ansi (string)
  "Remove ANSI escape sequences from STRING."
  (when (stringp string)
    (ansi-color-filter-apply string)))

;;; Markdown table alignment

(defun opencode-session--table-line-p (line)
  "Return non-nil if LINE looks like a markdown table row."
  (string-match-p "^[[:blank:]]*|.*|[[:blank:]]*$" line))

(defun opencode-session--table-separator-p (line)
  "Return non-nil if LINE is a markdown table separator row.
A separator row contains only pipes, dashes, colons, and whitespace."
  (string-match-p
   "^[[:blank:]]*|\\(?:[[:blank:]]*:?-+:?[[:blank:]]*|\\)+[[:blank:]]*$"
   line))

(defun opencode-session--split-table-cells (line)
  "Split a markdown table LINE into a list of trimmed cell strings.
Leading and trailing pipes are removed."
  (let ((trimmed (string-trim line)))
    ;; Strip leading/trailing pipe
    (when (string-prefix-p "|" trimmed)
      (setq trimmed (substring trimmed 1)))
    (when (string-suffix-p "|" trimmed)
      (setq trimmed (substring trimmed 0 -1)))
    (mapcar #'string-trim (split-string trimmed "|"))))

(defun opencode-session--build-table-separator (widths)
  "Build a separator row like |---|---| for column WIDTHS.
Each width includes the padding spaces on either side of the cell
content."
  (concat "| "
          (mapconcat (lambda (w) (make-string w ?-))
                     widths " | ")
          " |"))

(defun opencode-session--build-table-row (cells widths)
  "Build a table row from CELLS padded to WIDTHS.
Each cell is left-aligned and padded with spaces."
  (let ((parts nil))
    (dotimes (i (length widths))
      (let* ((cell (or (nth i cells) ""))
             (w (nth i widths))
             (padded (concat cell (make-string (max 0 (- w (length cell))) ?\s))))
        (push padded parts)))
    (concat "| " (mapconcat #'identity (nreverse parts) " | ") " |")))

(defun opencode-session--align-markdown-tables (text)
  "Rewrite markdown tables in TEXT so columns are evenly padded.
Consecutive lines that look like table rows are grouped, column
widths are measured, and each cell is padded to the column maximum.
Non-table text passes through unchanged."
  (let ((lines (split-string text "\n"))
        (result nil)
        (table-lines nil))
    (cl-labels
        ((flush-table ()
           (when table-lines
             (setq table-lines (nreverse table-lines))
             (let* ((rows (mapcar #'opencode-session--split-table-cells
                                  table-lines))
                    ;; Determine the number of columns from the widest row
                    (ncols (apply #'max (mapcar #'length rows)))
                    ;; Compute max width per column (minimum 3 for separator
                    ;; dashes)
                    (widths (cl-loop for col below ncols
                                     collect
                                     (max 3
                                          (cl-loop
                                           for row in rows
                                           for cell = (or (nth col row) "")
                                           unless
                                           (opencode-session--table-separator-p
                                            (nth (cl-position row rows) table-lines))
                                           maximize (length cell))))))
               (dolist (line table-lines)
                 (if (opencode-session--table-separator-p line)
                     (push (opencode-session--build-table-separator widths)
                           result)
                   (let ((cells (opencode-session--split-table-cells line)))
                     (push (opencode-session--build-table-row cells widths)
                           result)))))
             (setq table-lines nil))))
      (dolist (line lines)
        (if (opencode-session--table-line-p line)
            (push line table-lines)
          (flush-table)
          (push line result)))
      (flush-table)
      (mapconcat #'identity (nreverse result) "\n"))))

;;; Format input parameters for generic/MCP tools

(defun opencode-session--format-input-params (input)
  "Format primitive values from INPUT alist as [key=value, ...].
Only includes string, number, and boolean values."
  (when (listp input)
    (let ((parts nil))
      (dolist (pair input)
        (when (consp pair)
          (let ((key (car pair))
                (value (cdr pair)))
            (when (or (stringp value)
                      (numberp value)
                      (eq value t)
                      (eq value :json-false))
              (let ((val-str (cond
                              ((eq value t) "true")
                              ((eq value :json-false) "false")
                              (t (format "%s" value)))))
                (push (format "%s=%s" key val-str) parts))))))
      (when parts
        (format "[%s]" (string-join (nreverse parts) ", "))))))

(defun opencode-session--render-messages ()
  "Render all messages for the session."
  (dolist (message opencode-session--messages)
    (opencode-session--render-message message)))

(defun opencode-session--render-message (message)
  "Render MESSAGE into the buffer."
  (let ((render-start (and opencode-sse-profile-enabled
                           (opencode-sse-profile--now))))
    (let ((text (opencode-session--message-text message)))
      (opencode-session--replace-message message text nil))
    (when render-start
      (opencode-sse-profile-add-render-time
       (opencode-sse-profile--elapsed-ms render-start)))))

(defun opencode-session--replace-message (message text face)
  "Replace MESSAGE region with TEXT using FACE."
  (let ((start (opencode-message-start-marker message))
        (end (opencode-message-end-marker message)))
    (if (and start end)
        (opencode-session--replace-message-region start end text face message)
      (opencode-session--insert-message message text face))))

(defun opencode-session--replace-message-region (start end text face message)
  "Replace text between START and END with TEXT, FACE, and MESSAGE."
  (let* ((old-start (marker-position start))
         (old-end (marker-position end))
         (window-states
          (opencode-session--message-render-window-states old-start old-end))
         (inhibit-read-only t)
         new-start
         new-end)
    (save-excursion
      (goto-char old-start)
      (delete-region old-start old-end)
      (setq new-start (point))
      (insert text)
      (setq new-end (point))
      (set-marker start new-start)
      (set-marker end new-end)
      (opencode-session--apply-message-properties new-start new-end face message))
    (opencode-session--restore-render-window-states
     window-states new-start new-end)))

(defun opencode-session--message-render-window-states (start end)
  "Return window state to preserve while replacing START to END."
  (let ((buffer (current-buffer))
        states)
    (dolist (window (get-buffer-window-list buffer nil t))
      (let* ((point (window-point window))
             (window-start (window-start window))
             (window-end (window-end window t))
             (input-start (and (markerp opencode-session--input-start-marker)
                               (marker-position opencode-session--input-start-marker)))
             (follow-bottom (and window-end
                                 (= window-end (point-max))
                                 input-start
                                 (<= input-start point)))
             (point-offset (and (<= start point) (<= point end)
                                (- point start)))
             (window-start-offset (and (<= start window-start)
                                       (<= window-start end)
                                       (- window-start start))))
        (push (list :window window
                    :point-marker (copy-marker point)
                    :point-offset point-offset
                    :window-start-marker (copy-marker window-start)
                    :window-start-offset window-start-offset
                    :follow-bottom follow-bottom)
              states)))
    states))

(defun opencode-session--restore-render-window-states (states new-start new-end)
  "Restore window STATES after replacing text with NEW-START and NEW-END."
  (dolist (state states)
    (let ((window (plist-get state :window)))
      (when (window-live-p window)
        (let* ((follow-bottom (plist-get state :follow-bottom))
               (point-offset (plist-get state :point-offset))
               (point-marker (plist-get state :point-marker))
               (window-start-offset (plist-get state :window-start-offset))
               (window-start-marker (plist-get state :window-start-marker))
               (restored-point
                (if point-offset
                    (min new-end (+ new-start point-offset))
                  (marker-position point-marker)))
               (restored-start
                (if window-start-offset
                    (min new-end (+ new-start window-start-offset))
                  (marker-position window-start-marker))))
          (if follow-bottom
              (set-window-point window (point-max))
            (when restored-start
              (set-window-start window restored-start t))
            (when restored-point
              (set-window-point window restored-point)))
          (set-marker point-marker nil)
          (set-marker window-start-marker nil))))))

(declare-function opencode-session--ensure-input-prompt "emacs-opencode-session-mode")

(defun opencode-session--insert-message (message text face)
  "Insert MESSAGE with TEXT and FACE at the end of the log.

Clears the retry banner before inserting so the new message lands
above the banner, then re-renders the banner just above the input
prompt."
  (opencode-session--clear-retry-banner)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (marker-position opencode-session--input-start-marker))
      (let ((start (point)))
        (insert text)
        (let ((end (point)))
          (setf (opencode-message-start-marker message) (copy-marker start))
          (setf (opencode-message-end-marker message) (copy-marker end)))
        (insert "\n")
        (set-marker opencode-session--input-start-marker (point))
        (set-marker opencode-session--input-marker (point-max))
        (opencode-session--ensure-input-prompt)
        (opencode-session--apply-message-properties start (point) face message))))
  (opencode-session--render-retry-banner))

(defun opencode-session--retry-banner-text (status)
  "Return the banner text for retry STATUS, or nil.

STATUS is an `opencode-status' struct.  Returns nil when no banner
should be displayed (status is not retry, or no message)."
  (when (and (opencode-status-p status)
             (string= (or (opencode-status-type status) "") "retry"))
    (let* ((msg (opencode-status-message status))
           (attempt (opencode-status-attempt status))
           (next (opencode-status-next status))
           (suffix-parts nil))
      (when (numberp attempt)
        (push (format "attempt #%d" attempt) suffix-parts))
      (when (numberp next)
        (let* ((now-ms (* 1000.0 (float-time)))
               (remaining (max 0 (round (/ (- next now-ms) 1000.0)))))
          (push (format "retrying in %ds" remaining) suffix-parts)))
      (let ((suffix (when suffix-parts
                      (format " [%s]"
                              (string-join (nreverse suffix-parts) ", ")))))
        (concat (or msg "retrying...")
                (or suffix ""))))))

(defun opencode-session--clear-retry-banner ()
  "Delete the inline retry banner region, if any."
  (when (and (markerp opencode-session--retry-banner-start)
             (markerp opencode-session--retry-banner-end))
    (let ((start (marker-position opencode-session--retry-banner-start))
          (end (marker-position opencode-session--retry-banner-end))
          (inhibit-read-only t))
      (when (and start end (< start end))
        (save-excursion
          (delete-region start end))))
    (set-marker opencode-session--retry-banner-start nil)
    (set-marker opencode-session--retry-banner-end nil)
    (setq-local opencode-session--retry-banner-start nil)
    (setq-local opencode-session--retry-banner-end nil)))

(defun opencode-session--render-retry-banner ()
  "Render or remove the inline retry banner above the input prompt.

Reads the current session status and either inserts the banner
between dedicated markers (replacing any existing banner) or clears
the region when no retry status is active.  The banner is fontified
with `opencode-session-error-face' so it stands out as an error."
  (when (and opencode-session--session
             (markerp opencode-session--input-start-marker))
    (let* ((status (opencode-session-status opencode-session--session))
           (text (opencode-session--retry-banner-text status)))
      (if (null text)
          (opencode-session--clear-retry-banner)
        (opencode-session--insert-retry-banner text)))))

(defun opencode-session--insert-retry-banner (text)
  "Insert TEXT as the inline retry banner above the input prompt.

Always clears any existing banner first and re-inserts at the
current `opencode-session--input-start-marker' position.  Pushes
`opencode-session--input-start-marker' past the banner and
re-anchors the prompt overlay so the banner sits above the prompt.

TEXT does not include a trailing newline; one is added so the
banner occupies its own line."
  (opencode-session--clear-retry-banner)
  (let ((inhibit-read-only t)
        (rendered (concat (propertize text
                                      'font-lock-face 'opencode-session-error-face
                                      'opencode-part-type "retry-banner"
                                      'read-only t
                                      'front-sticky t
                                      'rear-nonsticky t)
                          (propertize "\n\n"
                                      'read-only t
                                      'front-sticky t
                                      'rear-nonsticky t))))
    (save-excursion
      (goto-char (marker-position opencode-session--input-start-marker))
      (let ((insert-start (point)))
        (insert rendered)
        (let ((insert-end (point)))
          (setq-local opencode-session--retry-banner-start
                      (copy-marker insert-start))
          (setq-local opencode-session--retry-banner-end
                      (copy-marker insert-end))
          ;; Push only the input boundary past the banner.  User input
          ;; remains the text from that boundary through `point-max'.
          (set-marker opencode-session--input-start-marker insert-end)
          (when (markerp opencode-session--input-marker)
            (set-marker opencode-session--input-marker (point-max)))
          (opencode-session--ensure-input-prompt))))))

(defun opencode-session--apply-message-properties (start end _face message)
  "Apply read-only properties from START to END for MESSAGE.
Individual parts carry their own `face' and `opencode-part-type'
properties set during rendering; this function only adds structural
properties and the user prefix indicator."
  (add-text-properties start end '(read-only t front-sticky t rear-nonsticky t))
  (opencode-session--apply-user-prefix message start end))

(defun opencode-session--apply-user-prefix (message start end)
  "Apply a line indicator for user MESSAGE between START and END."
  (when (and message (string= (opencode-message-role message) "user"))
    (let* ((color (face-foreground 'opencode-session-user-prefix-face nil t))
           (marker-face (if color `(:background ,color) 'opencode-session-user-prefix-face))
           (marker (propertize " " 'face marker-face 'display '(space :width 0.3)))
           (padding (propertize " " 'display '(space :width 0.9)))
           (prefix (concat marker padding))
           (prefix-end (if (and (> end start)
                                (eq (char-before end) ?\n))
                           (1- end)
                         end)))
      (when (> prefix-end start)
        (add-text-properties start prefix-end
                             `(line-prefix ,prefix wrap-prefix ,prefix))))))

(defun opencode-session--message-text (message)
  "Return the renderable text for MESSAGE."
  (let* ((parts (opencode-message-parts message))
         (base (if (and parts (listp parts))
                   (opencode-session--render-message-parts message parts)
                 (or (opencode-message-text message) "")))
         (error-text (opencode-session--message-error-text message)))
    (if error-text
        (if (string-empty-p (string-trim-right base))
            error-text
          (concat (string-trim-right base) "\n\n" error-text))
      base)))

(defun opencode-session--message-error-text (message)
  "Return a formatted assistant error string for MESSAGE, or nil."
  (when (and (string= (opencode-message-role message) "assistant")
             (consp (opencode-message-error message)))
    (let* ((error-info (opencode-message-error message))
           (name (alist-get 'name error-info))
           (data (alist-get 'data error-info))
           (detail (and (listp data)
                        (opencode-session--nonempty-string (alist-get 'message data)))))
      (unless (string= name "MessageAbortedError")
        (propertize (format "Error: %s" (or detail "An error occurred"))
                    'opencode-part-type "assistant-text"
                    'face 'error)))))

(defun opencode-session--render-message-parts (message parts)
  "Render PARTS for MESSAGE into a string."
  (let ((output "")
        (rendered-compaction nil))
    (dolist (entry parts)
      (let* ((part (cdr entry))
             (part-type (opencode-message-part-type part))
             (tool (opencode-message-part-tool part))
             (rendered (opencode-session--render-message-part message part))
             (tool-part (string= part-type "tool"))
             (block-tool (and tool-part (member tool '("todowrite" "todoread"
                                                        "edit" "apply_patch"
                                                        "bash" "task")))))
        (when (and rendered
                   (not (and (string= part-type "compaction")
                             rendered-compaction)))
          (when (string= part-type "compaction")
            (setq rendered-compaction t))
          (cond
           ((or (member part-type '("text" "reasoning" "compaction")) block-tool)
            (when (and (not (string-empty-p output))
                       (not (string-match-p "\\n\\n+\\'" output)))
              (setq output (concat output "\n")))
            (setq output (concat output rendered "\n")))
           (tool-part
            (when (and (not (string-empty-p output))
                       (not (string-match-p "\\n\\'" output)))
              (setq output (concat output "\n")))
            (setq output (concat output rendered)))
           (t
            (setq output (concat output rendered)))))))
    output))

(defun opencode-session--render-message-part (message part)
  "Render a single message PART for MESSAGE."
  (let ((part-type (opencode-message-part-type part)))
    (cond
     ((string= part-type "text")
      (let ((text (opencode-session--align-markdown-tables
                   (or (opencode-message-part-text part) "")))
            (synthetic (opencode-message-part-synthetic part))
            (ignored (opencode-message-part-ignored part))
            (role (opencode-message-role message)))
        (unless (or synthetic ignored (string-empty-p (string-trim text)))
          (let ((ptype (if (string= role "user") "user-text" "assistant-text")))
            (propertize text 'opencode-part-type ptype)))))
     ((string= part-type "reasoning")
      (when opencode-session--show-reasoning
        (let ((text (or (opencode-message-part-text part) "")))
          (unless (string-empty-p (string-trim text))
            (propertize (concat "Thinking:\n" text)
                        'opencode-part-type "reasoning")))))
      ((string= part-type "tool")
       (opencode-session--tool-part-line part))
      ((string= part-type "compaction")
       (opencode-session--compaction-line part))
      (t nil))))

(defun opencode-session--compaction-line (part)
  "Render a compaction PART as a boundary line."
  (let* ((automatic (opencode-message-part-auto part))
         (overflow (opencode-message-part-overflow part))
         (tail-start-id (opencode-message-part-tail-start-id part))
         (complete (or tail-start-id (opencode-message-part-time-end part)))
         (label (cond
                 ((not complete) "Session compacting...")
                 (automatic "Session auto-compacted")
                 (t "Session compacted")))
         (detail (cond
                  ((not complete) "Earlier messages are being summarized for future context.")
                  (overflow "Earlier messages were summarized after context overflow and are no longer sent verbatim.")
                  (t "Earlier messages were summarized and are no longer sent verbatim."))))
    (propertize (format "%s\n%s" label detail)
                'opencode-part-type "compaction"
                'face 'opencode-session-compaction-face)))

(defun opencode-session--tool-propertize (text)
  "Add the tool part-type property to TEXT, preserving existing properties."
  (let ((result (copy-sequence text)))
    (add-text-properties 0 (length result)
                         '(opencode-part-type "tool") result)
    result))

(defun opencode-session--tool-part-line (part)
  "Render a tool call PART as a formatted line or block."
  (let* ((tool (opencode-message-part-tool part))
         (state (opencode-message-part-state part))
         (input (alist-get 'input state))
         (metadata (alist-get 'metadata state))
         (status (or (alist-get 'status state) "pending"))
         (text (opencode-session--tool-summary tool input metadata status state))
         (error-line (opencode-session--tool-error-line status state))
         (extra (opencode-session--tool-extra-block tool input metadata part))
         (is-diff (and extra
                       (not (string-empty-p (string-trim extra)))
                       (member tool '("edit" "apply_patch")))))
    (setq text (opencode-session--tool-attach-status text status))
    (when error-line
      (setq text (concat text "\n" error-line)))
    (if is-diff
        ;; Diff extra block: tool summary tagged as tool, diff tagged for font-lock
        (concat (opencode-session--tool-propertize text)
                "\n"
                (propertize extra 'opencode-part-type "diff"))
      ;; Non-diff extra: everything tagged as tool
      (when (and extra (not (string-empty-p (string-trim extra))))
        (setq text (concat text "\n" extra)))
      (opencode-session--tool-propertize text))))

(defun opencode-session--tool-attach-status (text status)
  "Append STATUS to the first line of TEXT when missing.
Preserves text properties on existing text."
  (if (and (stringp text)
           (stringp status)
           (member status '("pending" "running" "error")))
      (let ((suffix (format " [%s]" status)))
        (if (string-match-p (regexp-quote (format "[%s]" status)) text)
            text
          (let* ((lines (split-string text "\n"))
                 (first (or (car lines) ""))
                 (rest (cdr lines))
                 (first-line (if (string-empty-p first)
                                 (string-trim-left suffix)
                               (concat first suffix))))
            (string-join (cons first-line rest) "\n"))))
    text))

(defun opencode-session--tool-error-line (status state)
  "Return a formatted error line when STATUS indicates failure."
  (when (string= status "error")
    (opencode-session--nonempty-string (alist-get 'error state))))

(defun opencode-session--tool-summary (tool input metadata status state)
  "Return the formatted summary for TOOL using INPUT and METADATA.

STATUS and STATE provide additional context for fallbacks."
  (cond
   ((string= tool "todowrite")
    (opencode-session--tool-todos "# Todos" input metadata))
   ((string= tool "todoread")
    (opencode-session--tool-todos "# Todos" input metadata))
   ((string= tool "glob")
    (opencode-session--tool-glob input metadata))
   ((string= tool "grep")
    (opencode-session--tool-grep input metadata))
   ((string= tool "read")
    (opencode-session--tool-read input))
   ((string= tool "bash")
    (opencode-session--tool-bash input metadata))
   ((string= tool "edit")
    (opencode-session--tool-edit-write "Edit" input metadata))
   ((string= tool "apply_patch")
     (opencode-session--tool-apply-patch input metadata status state))
   ((string= tool "write")
    (opencode-session--tool-edit-write "Write" input metadata))
   ((string= tool "task")
    (opencode-session--tool-task input metadata))
   ((string= tool "webfetch")
    (opencode-session--tool-webfetch input))
   (t
    (opencode-session--tool-generic tool input status state))))

(defun opencode-session--tool-todos (title input metadata)
  "Render todo list TITLE using INPUT and METADATA.

Returns a multi-line string."
  (let* ((todos (opencode-session--tool-extract-todos input metadata))
         (lines (list title)))
    (dolist (todo todos)
      (let* ((status (alist-get 'status todo))
             (content (or (alist-get 'content todo) ""))
             (marker (opencode-session--todo-marker status)))
        (push (format "[%s] %s" marker content) lines)))
    (string-join (nreverse lines) "\n")))

(defun opencode-session--tool-extract-todos (input metadata)
  "Return todo list items from INPUT or METADATA."
  (let ((todos (or (alist-get 'todos metadata)
                   (alist-get 'todos input))))
    (cond
     ((vectorp todos) (append todos nil))
     ((listp todos) todos)
     (t nil))))

(defun opencode-session--todo-marker (status)
  "Return a checkbox marker for STATUS."
  (cond
   ((string= status "completed") "✓")
   ((string= status "in_progress") "•")
   (t " ")))

(defun opencode-session--tool-glob (input metadata)
  "Render a summary line for the glob tool."
  (let* ((pattern (alist-get 'pattern input))
         (path (alist-get 'path input))
         (count (alist-get 'count metadata))
         (truncated (alist-get 'truncated metadata))
         (location (opencode-session--format-location path))
         (matches (opencode-session--format-count count truncated))
         (pattern-text (opencode-session--format-quoted pattern)))
    (string-join
     (delq nil (list "✱ Glob" pattern-text location matches))
     " ")))

(defun opencode-session--tool-grep (input metadata)
  "Render a summary line for the grep tool."
  (let* ((pattern (alist-get 'pattern input))
         (path (alist-get 'path input))
         (include (alist-get 'include input))
         (matches (alist-get 'matches metadata))
         (truncated (alist-get 'truncated metadata))
         (location (opencode-session--format-location path))
         (match-text (opencode-session--format-count matches truncated))
         (pattern-text (opencode-session--format-quoted pattern))
         (args (opencode-session--format-args (delq nil (list (when include
                                                                (format "include=%s" include)))))))
    (string-join
     (delq nil (list "✱ Grep" pattern-text location args match-text))
     " ")))

(defun opencode-session--tool-read (input)
  "Render a summary line for the read tool."
  (let* ((file-path (or (alist-get 'filePath input) ""))
         (offset (alist-get 'offset input))
         (limit (alist-get 'limit input))
         (args (opencode-session--format-args
                (delq nil (list (when offset (format "offset=%s" offset))
                                (when limit (format "limit=%s" limit))))))
         (path (or (opencode-session--display-path file-path) "")))
    (format "→ Read %s%s" path (if args (concat " " args) ""))))

(defun opencode-session--tool-bash (input metadata)
  "Render a summary line for the bash tool."
  (let* ((description (or (alist-get 'description input)
                          (alist-get 'description metadata)))
         (command (alist-get 'command input)))
    (cond
     (description (format "✱ Shell %s" description))
     (command (format "✱ Shell %s" command))
     (t "✱ Shell"))))

(defun opencode-session--tool-edit-write (label input metadata)
  "Render a summary line for edit or write LABEL.

INPUT and METADATA may include the file path."
  (let* ((file-path (or (alist-get 'filePath input)
                        (alist-get 'filepath metadata)
                        ""))
         (path (or (opencode-session--display-path file-path) "")))
    (format "→ %s %s" label path)))

(defun opencode-session--tool-apply-patch (_input _metadata status state)
  "Render a summary line for patch tool calls."
  (let ((title (opencode-session--nonempty-string (alist-get 'title state))))
    (if (and title (string= status "completed"))
        title
      "→ Patch")))

(defun opencode-session--tool-extra-block (tool input metadata &optional part)
  "Return extra block content for TOOL from INPUT or METADATA.
PART is the full message part, used for collapse identifiers."
  (cond
   ((string= tool "read")
    (opencode-session--read-loaded-block metadata part))
   ((member tool '("edit" "apply_patch"))
    (when (listp metadata)
      (opencode-session--nonempty-string (alist-get 'diff metadata))))
   ((string= tool "bash")
    (opencode-session--bash-extra-block input metadata part))))

(defun opencode-session--read-loaded-block (metadata part)
  "Render instruction paths loaded by a completed read PART.
METADATA is the read tool's completion metadata."
  (let* ((state (and part (opencode-message-part-state part)))
         (status (alist-get 'status state))
         (time (alist-get 'time state))
         (loaded (and (string= status "completed")
                      (not (alist-get 'compacted time))
                      (alist-get 'loaded metadata)))
         (paths (cond
                 ((vectorp loaded) (append loaded nil))
                 ((listp loaded) loaded))))
    (when paths
      (mapconcat (lambda (path)
                   (format "↳ Loaded %s" (opencode-session--display-path path)))
                 (cl-remove-if-not #'stringp paths)
                 "\n"))))

(defun opencode-session--bash-extra-block (input metadata part)
  "Build the extra block for a bash tool call.
Shows the command and output from INPUT and METADATA.
PART provides the part ID for collapse identifiers.
Output beyond `opencode-session-bash-output-max-lines' is
hidden with a per-part invisibility symbol and a clickable
toggle indicator."
  (let* ((command (or (alist-get 'command input)
                      (when (listp metadata)
                        (alist-get 'command metadata))))
         (raw-output (when (listp metadata)
                       (alist-get 'output metadata)))
         (output (when (opencode-session--nonempty-string raw-output)
                   (opencode-session--strip-ansi (string-trim raw-output))))
         (cmd-line (when (opencode-session--nonempty-string command)
                     (format "$ %s" command)))
         (max-lines opencode-session-bash-output-max-lines))
    (when (or cmd-line output)
      (let ((result (concat "\n" (or cmd-line ""))))
        (when (opencode-session--nonempty-string output)
          (let* ((lines (split-string output "\n"))
                 (total (length lines)))
            (if (and (> total max-lines) part)
                (let* ((part-id (opencode-message-part-id part))
                       (sym (opencode-session--collapse-symbol part-id))
                       (visible (string-join (cl-subseq lines 0 max-lines) "\n"))
                       (hidden (string-join (cl-subseq lines max-lines) "\n"))
                       (overflow (- total max-lines))
                       (indicator (format "▶ %d more lines" overflow)))
                  ;; Collapse unless the user has manually expanded this block.
                  (let ((user-expanded
                         (and opencode-session--expanded-collapse-syms
                              (gethash sym opencode-session--expanded-collapse-syms))))
                    (unless user-expanded
                      (add-to-invisibility-spec sym))
                    (setq result (concat result "\n" visible "\n"
                                         (propertize (concat hidden "\n")
                                                     'invisible sym)
                                         (propertize (if user-expanded "▼ collapse" indicator)
                                                     'opencode-part-type "tool"
                                                     'opencode-collapse-sym sym
                                                     'opencode-collapse-count overflow
                                                     'opencode-collapse-indicator sym
                                                      'keymap opencode-session--collapse-keymap
                                                      'mouse-face 'highlight)))))
              (setq result (concat result "\n" output)))))
        (concat result "\n")))))

(defun opencode-session--task-current-tool (tools)
  "Return the latest non-pending tool entry from TOOLS."
  (cl-loop for item in (reverse tools)
           for state = (alist-get 'state item)
           for status = (alist-get 'status state)
           when (and status (not (string= status "pending")))
           return item))

(defun opencode-session--task-tool-line (item)
  "Return a formatted line for tool ITEM."
  (let* ((tool (alist-get 'tool item))
         (state (alist-get 'state item))
         (status (alist-get 'status state))
         (title (and (string= status "completed")
                     (alist-get 'title state)))
         (tool-label (and tool (capitalize tool)))
         (title-text (and title (not (string-empty-p title)) title)))
    (when tool-label
      (string-join (delq nil (list tool-label title-text)) " "))))

(defun opencode-session--tool-task (input metadata)
  "Render a summary line for the task tool.
Uses live subagent tool tracking data when available."
  (let* ((subagent (or (alist-get 'subagent_type input)
                       (alist-get 'subagent-type input)
                       "task"))
         (description (or (alist-get 'description input)
                          (alist-get 'title metadata)))
         (agent-label (format "%s Task" (capitalize subagent)))
         (session-id (alist-get 'sessionId metadata))
         (tools (and session-id
                     (opencode-session--subagent-tools-for session-id)))
         (count (length tools))
         (current (and tools (opencode-session--task-current-tool tools)))
         (current-line (and current (opencode-session--task-tool-line current)))
         (text
          (if (> count 0)
              (let ((lines (list (format "# %s" agent-label))))
                (if (and description (not (string-empty-p description)))
                    (push (format "%s (%d toolcalls)" description count) lines)
                  (push (format "%d toolcalls" count) lines))
                (when current-line
                  (push (format "└ %s" current-line) lines))
                (string-join (nreverse lines) "\n"))
            (if (and description (not (string-empty-p description)))
                (format "# %s %s" agent-label description)
              (format "# %s" agent-label)))))
    ;; Apply interactive properties when a subagent session exists
    (if session-id
        (propertize text
                    'opencode-subagent-session-id session-id
                    'keymap opencode-session--task-keymap
                    'mouse-face 'highlight
                    'help-echo "RET: open subagent session")
      text)))

(defun opencode-session--tool-webfetch (input)
  "Render a summary line for the webfetch tool."
  (let* ((url (alist-get 'url input))
         (format-type (alist-get 'format input))
         (args (opencode-session--format-args
                (delq nil (list (when format-type (format "format=%s" format-type)))))))
    (string-join
     (delq nil (list "✱ Webfetch" url args "↗"))
     " ")))

(defun opencode-session--tool-generic (tool input _status _state)
  "Render a fallback summary line for TOOL.

INPUT is used to extract primitive parameters for display."
  (let* ((name (or tool "tool"))
         (params (opencode-session--format-input-params input)))
    (string-join (delq nil (list (format "⚙ %s" name) params)) " ")))

(defun opencode-session--nonempty-string (value)
  "Return VALUE when it is a non-empty string."
  (when (and (stringp value)
             (not (string-empty-p value)))
    value))

(defun opencode-session--display-path (path)
  "Return PATH formatted for display."
  (when (and path (stringp path))
    (let ((directory (and opencode-session--connection
                          (opencode-connection-directory opencode-session--connection))))
      (if (and directory (file-name-absolute-p path))
          (file-relative-name path directory)
        path))))

(defun opencode-session--format-location (path)
  "Format PATH as a location suffix."
  (when (and path (stringp path))
    (format "in %s" (opencode-session--display-path path))))

(defun opencode-session--format-count (count truncated)
  "Format COUNT and TRUNCATED into a match suffix."
  (when (numberp count)
    (format "(%s matches)" (if truncated (format "%s+" count) count))))

(defun opencode-session--format-args (args)
  "Format ARGS list into a bracket suffix."
  (when (and args (listp args))
    (let ((clean (delq nil args)))
      (when clean
        (format "[%s]" (string-join clean ", "))))))

(defun opencode-session--format-quoted (value)
  "Quote VALUE for display when present."
  (when (and value (stringp value))
    (format "\"%s\"" value)))

(defun opencode-session--role-face (message)
  "Return the face for MESSAGE role."
  (let ((role (opencode-message-role message)))
    (if (string= role "user")
        'opencode-session-user-face
      'opencode-session-assistant-face)))

(provide 'emacs-opencode-session-render)

;;; emacs-opencode-session-render.el ends here
