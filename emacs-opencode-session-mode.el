;;; emacs-opencode-session-mode.el --- OpenCode session buffer  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-message)
(require 'emacs-opencode-session)
(require 'emacs-opencode-sse)
(require 'emacs-opencode-client)

(defgroup emacs-opencode nil
  "Emacs client for the OpenCode server."
  :group 'applications)

(defcustom opencode-session-spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Spinner frames used in the session header for busy states."
  :type '(repeat string)
  :group 'emacs-opencode)

(defcustom opencode-session-spinner-interval 0.1
  "Seconds between session header spinner frames."
  :type 'number
  :group 'emacs-opencode)

(defcustom opencode-session-default-agent "plan"
  "Default agent name for new OpenCode sessions."
  :type 'string
  :group 'emacs-opencode)

(defcustom opencode-session-input-prompt "❯ "
  "Prompt string shown before the session input area."
  :type 'string
  :group 'emacs-opencode)

(defcustom opencode-session-completion-providers
  '(opencode-session--complete-command)
  "Completion providers for `opencode-session-mode` input.

Each function is called with point at the current input position and should
return a completion-at-point result or nil. Providers are tried in order until
one returns a completion result."
  :type '(repeat function)
  :group 'emacs-opencode)


(defface opencode-session-user-face
  '((t :inherit default))
  "Face used for user messages."
  :group 'emacs-opencode)

(defface opencode-session-user-prefix-face
  '((t :inherit font-lock-constant-face))
  "Face used for the user message line indicator."
  :group 'emacs-opencode)

(defface opencode-session-input-prompt-face
  '((t :inherit font-lock-constant-face))
  "Face used for the session input prompt."
  :group 'emacs-opencode)

(defface opencode-session-assistant-face
  '((t :inherit default))
  "Face used for assistant messages."
  :group 'emacs-opencode)

(defface opencode-session-header-face
  '((t :inherit default :weight bold))
  "Face used for session header text."
  :group 'emacs-opencode)

(defface opencode-session-status-face
  '((t :inherit shadow))
  "Face used for session status text."
  :group 'emacs-opencode)

(defface opencode-session-spinner-face
  '((t :inherit font-lock-type-face))
  "Face used for session spinner text."
  :group 'emacs-opencode)

(defface opencode-session-agent-face
  '((t :inherit (mode-line-emphasis success) :weight bold))
  "Face used for the active agent label."
  :group 'emacs-opencode)

(defface opencode-session-tool-face
  '((t :inherit shadow))
  "Face used for tool call lines."
  :group 'emacs-opencode)

(defvar opencode-session--buffers (make-hash-table :test 'equal)
  "Registry mapping session IDs to buffers.")

(defvar opencode-session--spinner-timer nil
  "Timer used to animate session header spinners.")

(defvar opencode-session-send-input-hook nil
  "Hook run when input is submitted.

Each function receives SESSION and INPUT as arguments.")

(defvar opencode-command-arguments-history nil
  "History list for OpenCode command arguments.")

(defvar-local opencode-session--session nil
  "Session object for the current buffer.")

(defvar-local opencode-session--messages nil
  "List of message objects for the current buffer.")

(defvar-local opencode-session--connection nil
  "Connection used for the current session buffer.")

(defvar-local opencode-session--input-start-marker nil
  "Marker indicating the start of the input region.")

(defvar-local opencode-session--input-marker nil
  "Marker indicating the end of the input region.")

(defvar-local opencode-session--input-prompt-overlay nil
  "Overlay used to display the input prompt.")


(defvar-local opencode-session--agent nil
  "Selected agent name for the current session buffer.")

(defvar-local opencode-session--agent-index nil
  "Index of the selected agent in the available agents list.")

(defvar-local opencode-session--provider-id nil
  "Selected provider ID for the current session buffer.")

(defvar-local opencode-session--model-id nil
  "Selected model ID for the current session buffer.")

(defvar opencode-session--recent-models nil
  "Global list of recently selected (PROVIDER-ID . MODEL-ID) pairs.
Most recently selected first.")

(defvar-local opencode-session--spinner-index 0
  "Current spinner frame index for the session buffer.")

(defvar opencode-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'opencode-session-send-input)
    (define-key map (kbd "C-c C-a") #'opencode-session-select-agent)
    (define-key map (kbd "C-c C-n") #'opencode-session-next-agent)
    (define-key map (kbd "C-c C-p") #'opencode-session-previous-agent)
    (define-key map (kbd "C-c C-r") #'opencode-session-refresh-agents)
    (define-key map (kbd "C-c C-k") #'opencode-session-interrupt)
    (define-key map (kbd "C-c C-l") #'opencode-session-select-model)
    (define-key map (kbd "C-c C-o") #'opencode-command)
    (define-key map (kbd "S-TAB") #'opencode-session-previous-agent)
    (define-key map (kbd "<backtab>") #'opencode-session-previous-agent)
    (define-key map (kbd "RET") #'newline)
    (define-key map (kbd "C-<tab>") #'completion-at-point)
    (define-key map [remap self-insert-command] #'opencode-session-self-insert)
    (define-key map [remap yank] #'opencode-session-yank)
    (define-key map [remap delete-backward-char] #'opencode-session-delete-backward)
    (define-key map [remap backward-delete-char-untabify] #'opencode-session-delete-backward)
    map)
  "Keymap for `opencode-session-mode`.")

(define-derived-mode opencode-session-mode text-mode "OpenCode-Session"
  "Major mode for OpenCode session buffers."
  (use-local-map opencode-session-mode-map)
  (when (and (bound-and-true-p evil-mode)
             (fboundp 'evil-define-key))
    (evil-define-key '(normal insert) (current-local-map)
      (kbd "TAB") #'opencode-session-next-agent
      (kbd "S-TAB") #'opencode-session-previous-agent
      (kbd "<backtab>") #'opencode-session-previous-agent))
  (setq-local buffer-read-only nil)
  (setq-local opencode-session--messages nil)
  (setq-local opencode-session--agent nil)
  (setq-local opencode-session--agent-index nil)
  (setq-local opencode-session--provider-id nil)
  (setq-local opencode-session--model-id nil)
  (opencode-session--ensure-markers)
  (add-hook 'completion-at-point-functions
            #'opencode-session--completion-at-point
            nil
            t))

(defun opencode-session-open (session &optional connection on-history-loaded)
  "Open a session buffer for SESSION and return it.

When CONNECTION is provided, load existing session messages. If
ON-HISTORY-LOADED is non-nil, call it with BUFFER after the history
request completes."
  (let* ((name (opencode-session--buffer-name session))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (opencode-session-mode)
      (setq-local opencode-session--session session)
      (setq-local opencode-session--connection connection)
      (opencode-session--register-buffer session buffer)
      (when connection
        (opencode-session--ensure-agents connection))
      (opencode-session--render-buffer))
    (if (and connection (opencode-session-id session))
        (opencode-session--load-history connection session buffer on-history-loaded)
      (when on-history-loaded
        (funcall on-history-loaded buffer)))
    (pop-to-buffer buffer)
    buffer))

;;;###autoload
(defun opencode-session-insert-input (input)
  "Insert INPUT into the session input area."
  (unless (derived-mode-p 'opencode-session-mode)
    (error "Not in an OpenCode session buffer"))
  (opencode-session--ensure-markers)
  (opencode-session--ensure-input-region)
  (let ((inhibit-read-only t))
    (delete-region (marker-position opencode-session--input-start-marker)
                   (marker-position opencode-session--input-marker))
    (goto-char (marker-position opencode-session--input-marker))
    (insert input))
  (opencode-session--goto-input))

(defun opencode-session-send-input ()
  "Send the current input region content."
  (interactive)
  (let ((input (opencode-session--current-input)))
    (if (string-empty-p (string-trim input))
        (message "OpenCode input is empty")
      (unless (and opencode-session--connection opencode-session--session)
        (error "OpenCode session is not connected"))
      (if (string-prefix-p "/" input)
          (opencode-session--maybe-send-command opencode-session--connection
                                                opencode-session--session
                                                input)
        (opencode-session--send-input opencode-session--connection
                                      opencode-session--session
                                      input)
        (opencode-session--clear-input)
        (message "OpenCode message submitted")))))

;;;###autoload
(defun opencode-command ()
  "Prompt for an OpenCode command and send it to the current session."
  (interactive)
  (unless (derived-mode-p 'opencode-session-mode)
    (error "Not in an OpenCode session buffer"))
  (unless (and opencode-session--connection opencode-session--session)
    (error "OpenCode session is not connected"))
  (let ((connection opencode-session--connection)
        (session-id (opencode-session-id opencode-session--session))
        (agent opencode-session--agent)
        (model (opencode-session--selected-model-string)))
    (opencode-client-commands
     connection
     :success (lambda (&rest args)
                (let* ((data (plist-get args :data))
                       (items (opencode-session--command-items data))
                       (names (opencode-session--command-names items)))
                  (unless names
                    (error "No OpenCode commands available"))
                  (let* ((command (completing-read "OpenCode command: " names nil t))
                         (arguments (read-from-minibuffer
                                     "OpenCode command args (optional): "
                                     nil nil nil
                                     'opencode-command-arguments-history)))
                    (opencode-client-session-command
                     connection
                     session-id
                     command
                     arguments
                     :agent agent
                     :model model
                     :success (lambda (&rest _args)
                                (message "OpenCode command queued"))
                     :error (lambda (&rest _args)
                              (message "OpenCode: failed to send command"))))))
     :error (lambda (&rest _args)
              (error "Failed to fetch OpenCode commands")))))

(defun opencode-session-self-insert (n)
  "Insert N characters into the session input area."
  (interactive "p")
  (opencode-session--maybe-goto-input)
  (self-insert-command n))

(defun opencode-session-yank (arg)
  "Yank ARG into the session input area."
  (interactive "P")
  (opencode-session--maybe-goto-input)
  (yank arg))

(defun opencode-session-delete-backward (arg)
  "Delete ARG characters backward inside the input area."
  (interactive "p")
  (opencode-session--maybe-goto-input)
  (backward-delete-char-untabify arg))

(defun opencode-session--ensure-markers ()
  "Ensure input markers exist."
  (unless opencode-session--input-start-marker
    (setq-local opencode-session--input-start-marker (copy-marker (point-max))))
  (unless opencode-session--input-marker
    (setq-local opencode-session--input-marker (copy-marker (point-max) t))))

(defun opencode-session--buffer-name (session)
  "Return a buffer name for SESSION."
  (let ((title (string-trim (or (opencode-session-title session) ""))))
    (format "*OpenCode: %s*"
            (if (string-empty-p title)
                (or (opencode-session-slug session)
                    (opencode-session-id session)
                    "session")
              title))))

(defun opencode-session--register-buffer (session buffer)
  "Register BUFFER for SESSION."
  (when-let ((session-id (opencode-session-id session)))
    (puthash session-id buffer opencode-session--buffers)
    (opencode-session--maybe-start-spinner)))

(defun opencode-session--buffer-for-session (session-id)
  "Return the session buffer for SESSION-ID, if any."
  (gethash session-id opencode-session--buffers))

(defun opencode-session--render-buffer ()
  "Render the session buffer contents."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (opencode-session--ensure-markers)
  (opencode-session--render-header)
  (opencode-session--render-messages)
  (opencode-session--ensure-input-region))

(defun opencode-session--render-header ()
  "Render the header line for the session."
  (let* ((title (or (opencode-session-title opencode-session--session)
                    "OpenCode Session"))
          (status (or (opencode-session-status opencode-session--session)
                      "idle"))
          (agent opencode-session--agent)
          (agent-label (when (and agent (not (string-empty-p agent)))
                         (format "[%s]" agent)))
          (status-label (opencode-session--header-status-label status))
          (right (opencode-session--header-right)))
    (setq header-line-format
          (opencode-session--align-header
           (string-join
            (delq nil
                  (list (propertize title 'face 'opencode-session-header-face)
                        (when agent-label
                          (propertize agent-label 'face 'opencode-session-agent-face))
                        (when (not (string-empty-p status-label))
                          (propertize status-label
                                      'face 'opencode-session-spinner-face))))
            " ")
           right))))

(defun opencode-session--align-header (left right)
  "Align LEFT and RIGHT strings for the header line.

RIGHT is aligned to the far edge when provided."
  (if (and right (not (string-empty-p right)))
      (concat left
              (propertize " "
                          'display
                          `(space :align-to (- right ,(string-width right))))
              right)
    left))

(defun opencode-session--header-right ()
  "Return right-aligned header metadata when available."
  (when opencode-session--connection
    (opencode-session--ensure-providers opencode-session--connection))
  (let* ((model (opencode-session--header-model-string))
         (context (opencode-session--header-context-string))
         (cost (and
                (not (string-empty-p (string-trim (or context ""))))
                (opencode-session--header-cost-string))))
    (when (or model context cost)
      (propertize
       (string-join
        (delq nil (list model context (when cost (format "(%s)" cost))))
        " ")
       'face 'opencode-session-status-face))))

(defun opencode-session--header-model-string ()
  "Return the active provider/model string for the session header."
  (when-let* ((model (or (and opencode-session--provider-id opencode-session--model-id
                              (cons opencode-session--provider-id opencode-session--model-id))
                         (opencode-session--last-message-model))))
    (format "%s/%s" (car model) (cdr model))))

(defun opencode-session--last-message-model ()
  "Return the latest (PROVIDER-ID . MODEL-ID) from session messages."
  (cl-loop for message in (reverse opencode-session--messages)
           for provider-id = (opencode-message-provider-id message)
           for model-id = (opencode-message-model-id message)
           when (and (stringp provider-id)
                     (stringp model-id)
                     (not (string-empty-p provider-id))
                     (not (string-empty-p model-id)))
           return (cons provider-id model-id)))

(defun opencode-session--session-used-models ()
  "Return distinct (PROVIDER-ID . MODEL-ID) pairs used in this session.
Most recently used first."
  (let (seen result)
    (dolist (message (reverse opencode-session--messages))
      (let* ((provider-id (opencode-message-provider-id message))
             (model-id (opencode-message-model-id message))
             (key (and (stringp provider-id)
                       (stringp model-id)
                       (not (string-empty-p provider-id))
                       (not (string-empty-p model-id))
                       (cons provider-id model-id))))
        (when (and key (not (member key seen)))
          (push key seen)
          (push key result))))
    result))

(defun opencode-session--header-cost-string ()
  "Return total assistant cost formatted as currency."
  (let ((total (opencode-session--assistant-cost-total)))
    (when (and (numberp total)
               (or (> total 0)
                   (opencode-session--has-assistant-messages-p)))
      (format "$%.2f" total))))

(defun opencode-session--has-assistant-messages-p ()
  "Return non-nil when session has assistant messages."
  (cl-loop for message in opencode-session--messages
           for info = (opencode-message-info message)
           for role = (alist-get 'role info)
           when (string= role "assistant")
           return t))

(defun opencode-session--assistant-cost-total ()
  "Return the total cost for assistant messages in the session."
  (let ((total 0.0))
    (dolist (message opencode-session--messages total)
      (let* ((info (opencode-message-info message))
             (role (alist-get 'role info))
             (cost (alist-get 'cost info)))
        (when (and (string= role "assistant") (numberp cost))
          (setq total (+ total cost)))))))

(defun opencode-session--header-context-string ()
  "Return the context usage string for the session header."
  (when-let ((info (opencode-session--last-assistant-info)))
    (let* ((tokens (alist-get 'tokens info))
           (total (opencode-session--tokens-total tokens)))
      (when (numberp total)
        (let* ((count (opencode-session--format-number total))
               (percent (opencode-session--context-percent info total)))
          (if percent
              (format "%s  %s%%%%" count percent)
            count))))))

(defun opencode-session--last-assistant-info ()
  "Return the last assistant message info with output tokens."
  (cl-loop for message in (reverse opencode-session--messages)
           for info = (opencode-message-info message)
           for role = (alist-get 'role info)
           for tokens = (alist-get 'tokens info)
           for output = (alist-get 'output tokens)
           when (and (string= role "assistant")
                     (numberp output)
                     (> output 0))
           return info))

(defun opencode-session--tokens-total (tokens)
  "Return total tokens from TOKENS metadata."
  (when (listp tokens)
    (+ (opencode-session--safe-number (alist-get 'input tokens))
       (opencode-session--safe-number (alist-get 'output tokens))
       (opencode-session--safe-number (alist-get 'reasoning tokens))
       (let ((cache (alist-get 'cache tokens)))
         (+ (opencode-session--safe-number (alist-get 'read cache))
            (opencode-session--safe-number (alist-get 'write cache)))))))

(defun opencode-session--safe-number (value)
  "Return VALUE as a number or zero."
  (if (numberp value) value 0))

(defun opencode-session--format-number (value)
  "Return VALUE formatted with thousands separators."
  (let* ((number (max 0 (truncate value)))
         (string (number-to-string number))
         (len (length string))
         (pos len)
         (parts nil))
    (while (> pos 3)
      (push (substring string (- pos 3) pos) parts)
      (setq pos (- pos 3)))
    (push (substring string 0 pos) parts)
    (string-join parts ",")))

(defun opencode-session--context-percent (info total)
  "Return context usage percent string for INFO and TOTAL tokens."
  (when (and opencode-session--connection (numberp total))
    (let* ((provider-id (alist-get 'providerID info))
           (model-id (alist-get 'modelID info))
           (limit (opencode-session--model-context-limit provider-id model-id)))
      (when (and (numberp limit) (> limit 0))
        (number-to-string (round (* (/ (float total) limit) 100)))))))

(defun opencode-session--model-context-limit (provider-id model-id)
  "Return the context limit for PROVIDER-ID and MODEL-ID."
  (when (and opencode-session--connection provider-id model-id)
    (when-let* ((providers (opencode-connection-providers opencode-session--connection))
                (provider (cl-find provider-id providers
                                   :key (lambda (item) (alist-get 'id item))
                                   :test #'string=))
                (models (alist-get 'models provider))
                (model (alist-get model-id models nil nil #'string=))
                (limit (alist-get 'limit model))
                (context (alist-get 'context limit)))
      context)))

(defun opencode-session--header-status-label (status)
  "Return STATUS label for the header line."
  (if (opencode-session--status-busy-p status)
      (opencode-session--spinner-frame)
    ""))

(defun opencode-session--status-busy-p (status)
  "Return non-nil when STATUS should show a spinner."
  (not (string= status "idle")))

(defun opencode-session--spinner-frame ()
  "Return the current spinner frame.

Fallback to a plain busy label when frames are unavailable."
  (let* ((frames (if (and opencode-session-spinner-frames
                          (listp opencode-session-spinner-frames))
                     opencode-session-spinner-frames
                   '("…")))
         (count (length frames)))
    (if (> count 0)
        (nth (mod opencode-session--spinner-index count) frames)
      "busy")))

(defun opencode-session--advance-spinner ()
  "Advance spinner frames for visible session buffers."
  (maphash
   (lambda (_session-id buffer)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (and opencode-session--session
                    (opencode-session--status-busy-p
                     (or (opencode-session-status opencode-session--session)
                         "idle")))
           (setq opencode-session--spinner-index
                 (1+ opencode-session--spinner-index))
           (opencode-session--render-header)))))
   opencode-session--buffers))

(defun opencode-session--maybe-start-spinner ()
  "Start the session spinner timer when needed."
  (when (and (null opencode-session--spinner-timer)
             (opencode-session--spinner-needed-p))
    (setq opencode-session--spinner-timer
          (run-with-timer 0 opencode-session-spinner-interval
                          #'opencode-session--advance-spinner))))

(defun opencode-session--maybe-stop-spinner ()
  "Stop the session spinner timer when idle."
  (unless (opencode-session--spinner-needed-p)
    (when (timerp opencode-session--spinner-timer)
      (cancel-timer opencode-session--spinner-timer))
    (setq opencode-session--spinner-timer nil)))

(defun opencode-session--spinner-needed-p ()
  "Return non-nil when any session buffer is busy."
  (let (busy)
    (maphash
     (lambda (_session-id buffer)
       (when (and (not busy) (buffer-live-p buffer))
         (with-current-buffer buffer
           (when (and opencode-session--session
                      (opencode-session--status-busy-p
                       (or (opencode-session-status opencode-session--session)
                           "idle")))
             (setq busy t)))))
     opencode-session--buffers)
    busy))

(defun opencode-session--render-messages ()
  "Render all messages for the session."
  (dolist (message opencode-session--messages)
    (opencode-session--render-message message)))

(defun opencode-session--render-message (message)
  "Render MESSAGE into the buffer."
  (let ((text (opencode-session--message-text message)))
    (opencode-session--replace-message message text nil)))

(defun opencode-session--replace-message (message text face)
  "Replace MESSAGE region with TEXT using FACE."
  (let ((start (opencode-message-start-marker message))
        (end (opencode-message-end-marker message)))
    (if (and start end)
        (opencode-session--replace-message-region start end text face message)
      (opencode-session--insert-message message text face))))

(defun opencode-session--replace-message-region (start end text face message)
  "Replace text between START and END with TEXT, FACE, and MESSAGE."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (marker-position start))
      (delete-region (marker-position start) (marker-position end))
      (let ((new-start (point)))
        (insert text)
        (let ((new-end (point)))
          (set-marker start new-start)
          (set-marker end new-end)
          (opencode-session--apply-message-properties new-start new-end face message))))))

(defun opencode-session--insert-message (message text face)
  "Insert MESSAGE with TEXT and FACE at the end of the log."
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
        (set-marker opencode-session--input-marker (point))
        (opencode-session--ensure-input-prompt)
        (opencode-session--apply-message-properties start (point) face message)))))

(defun opencode-session--apply-message-properties (start end face message)
  "Apply message properties from START to END with FACE for MESSAGE."
  (let ((properties '(read-only t front-sticky t rear-nonsticky t)))
    (add-text-properties start end (if face (append properties `(face ,face)) properties))
    (opencode-session--apply-user-prefix message start end)))

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
  (let ((parts (opencode-message-parts message)))
    (if (and parts (listp parts))
        (opencode-session--render-message-parts message parts)
      (or (opencode-message-text message) ""))))

(defun opencode-session--render-message-parts (message parts)
  "Render PARTS for MESSAGE into a string."
  (let ((output ""))
    (dolist (entry parts)
      (let* ((part (cdr entry))
             (part-type (opencode-message-part-type part))
             (tool (opencode-message-part-tool part))
             (rendered (opencode-session--render-message-part message part))
             (tool-part (string= part-type "tool"))
             (block-tool (and tool-part (member tool '("todowrite" "todoread"
                                                       "edit" "apply_patch"
                                                       "bash")))))
        (when rendered
          (cond
           ((or (string= part-type "text") block-tool)
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
      (let ((text (or (opencode-message-part-text part) ""))
            (synthetic (opencode-message-part-synthetic part))
            (ignored (opencode-message-part-ignored part))
            (face (opencode-session--role-face message)))
        (unless (or synthetic ignored (string-empty-p (string-trim text)))
          (propertize text 'face face))))
     ((string= part-type "tool")
      (opencode-session--tool-part-line part))
     (t nil))))

(defun opencode-session--tool-part-line (part)
  "Render a tool call PART as a formatted line or block."
  (let* ((tool (opencode-message-part-tool part))
         (state (opencode-message-part-state part))
         (input (alist-get 'input state))
         (metadata (alist-get 'metadata state))
         (status (or (alist-get 'status state) "pending"))
         (text (opencode-session--tool-summary tool input metadata status state))
         (error-line (opencode-session--tool-error-line status state))
         (extra (opencode-session--tool-extra-block tool input metadata)))
    (setq text (opencode-session--tool-attach-status text status))
    (when error-line
      (setq text (concat text "\n" error-line)))
    (when (and extra (not (string-empty-p (string-trim extra))))
      (setq text (concat text "\n" extra)))
    (propertize text 'face 'opencode-session-tool-face)))

(defun opencode-session--tool-attach-status (text status)
  "Append STATUS to the first line of TEXT when missing."
  (if (and (stringp text)
           (stringp status)
           (member status '("pending" "running" "error")))
      (let ((suffix (format "[%s]" status)))
        (if (string-match-p (regexp-quote suffix) text)
            text
          (let* ((lines (split-string text "\n"))
                 (first (or (car lines) ""))
                 (rest (cdr lines))
                 (first-line (if (string-empty-p first)
                                 suffix
                               (format "%s %s" first suffix))))
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

(defun opencode-session--tool-extra-block (tool input metadata)
  "Return extra block content for TOOL from INPUT or METADATA."
  (cond
   ((member tool '("edit" "apply_patch"))
    (when (listp metadata)
      (opencode-session--nonempty-string (alist-get 'diff metadata))))
   ((string= tool "bash")
    (let ((command (or (alist-get 'command input)
                       (when (listp metadata)
                         (alist-get 'command metadata)))))
      (when (opencode-session--nonempty-string command)
        (format "\n$ %s\n" command))))))

(defun opencode-session--task-summary-current (summary)
  "Return the latest non-pending summary item from SUMMARY."
  (cl-loop for item in (reverse summary)
           for state = (alist-get 'state item)
           for status = (alist-get 'status state)
           when (and status (not (string= status "pending")))
           return item))

(defun opencode-session--task-summary-line (item)
  "Return a summary line for ITEM."
  (let* ((tool (alist-get 'tool item))
         (state (alist-get 'state item))
         (title (alist-get 'title state))
         (tool-label (and tool (capitalize tool)))
         (title-text (and title (not (string-empty-p title)) title)))
    (when tool-label
      (string-join (delq nil (list tool-label title-text)) " "))))

(defun opencode-session--tool-task (input metadata)
  "Render a summary line for the task tool."
  (let* ((subagent (or (alist-get 'subagent_type input)
                       (alist-get 'subagent-type input)
                       "task"))
         (description (or (alist-get 'description input)
                          (alist-get 'title metadata)))
         (agent-label (format "%s Task" (capitalize subagent)))
         (summary (opencode-session--normalize-items (alist-get 'summary metadata)))
         (count (length summary))
         (current (opencode-session--task-summary-current summary))
         (current-line (and current (opencode-session--task-summary-line current))))
    (if (> count 0)
        (let ((lines (list (format "✱ %s" agent-label))))
          (if (and description (not (string-empty-p description)))
              (push (format "%s (%s toolcalls)" description count) lines)
            (push (format "%s toolcalls" count) lines))
          (when current-line
            (push (format "└ %s" current-line) lines))
          (string-join (nreverse lines) "\n"))
      (if (and description (not (string-empty-p description)))
          (format "✱ %s %s" agent-label description)
        (format "✱ %s" agent-label)))))

(defun opencode-session--tool-webfetch (input)
  "Render a summary line for the webfetch tool."
  (let* ((url (alist-get 'url input))
         (format-type (alist-get 'format input))
         (args (opencode-session--format-args
                (delq nil (list (when format-type (format "format=%s" format-type)))))))
    (string-join
     (delq nil (list "✱ Webfetch" url args "↗"))
     " ")))

(defun opencode-session--tool-generic (tool input status state)
  "Render a fallback summary line for TOOL.

INPUT, STATUS, and STATE provide context for the description."
  (let* ((description (or (opencode-session--nonempty-string
                           (alist-get 'description input))
                          (opencode-session--nonempty-string
                           (alist-get 'title state))
                          tool
                          "tool"))
         (suffix (and status (format "[%s]" status))))
    (string-join (delq nil (list (format "✱ %s" description) suffix)) " ")))

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

(defun opencode-session--ensure-input-region ()
  "Ensure the input marker sits at the end of the buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (set-marker opencode-session--input-marker (point)))
  (opencode-session--ensure-input-prompt)
  (opencode-session--goto-input))

(defun opencode-session--ensure-input-prompt ()
  "Ensure the input prompt overlay is up to date."
  (when opencode-session--input-start-marker
    (let ((prompt opencode-session-input-prompt))
      (if (or (null prompt) (string-empty-p prompt))
          (when (overlayp opencode-session--input-prompt-overlay)
            (delete-overlay opencode-session--input-prompt-overlay)
            (setq opencode-session--input-prompt-overlay nil))
        (let ((pos (marker-position opencode-session--input-start-marker)))
          (unless (overlayp opencode-session--input-prompt-overlay)
            (setq opencode-session--input-prompt-overlay (make-overlay pos pos)))
          (move-overlay opencode-session--input-prompt-overlay pos pos)
          (overlay-put opencode-session--input-prompt-overlay
                       'before-string
                       (propertize prompt 'face 'opencode-session-input-prompt-face)))))))

(defun opencode-session--goto-input ()
  "Move point to the input region."
  (when opencode-session--input-marker
    (let ((input-pos (marker-position opencode-session--input-marker)))
      (when (< (point) input-pos)
        (goto-char input-pos)))))

(defun opencode-session--maybe-goto-input ()
  "Move point to input when outside the input markers."
  (if (and opencode-session--input-start-marker
           opencode-session--input-marker)
      (let ((start (marker-position opencode-session--input-start-marker))
            (end (marker-position opencode-session--input-marker)))
        (when (or (< (point) start)
                  (> (point) end))
          (opencode-session--goto-input)))
    (opencode-session--goto-input)))

(defun opencode-session--current-input ()
  "Return current input contents as a string."
  (if opencode-session--input-marker
      (buffer-substring-no-properties (marker-position opencode-session--input-start-marker)
                                      (marker-position opencode-session--input-marker))
    ""))

(defun opencode-session--selected-model ()
  "Return the selected model as a cons (PROVIDER-ID . MODEL-ID) or nil."
  (when (and opencode-session--provider-id opencode-session--model-id)
    (cons opencode-session--provider-id opencode-session--model-id)))

(defun opencode-session--selected-model-string ()
  "Return the selected model as a \"provider/model\" string or nil."
  (when (and opencode-session--provider-id opencode-session--model-id)
    (format "%s/%s" opencode-session--provider-id opencode-session--model-id)))

(defun opencode-session--send-input (connection session input)
  "Send INPUT to SESSION using CONNECTION.

Restores INPUT when the request fails."
  (let ((session-id (opencode-session-id session))
        (payload `(("type" . "text") ("text" . ,input)))
        (agent opencode-session--agent)
        (model (opencode-session--selected-model)))
    (opencode-client-session-prompt-async
     connection
     session-id
     (list payload)
     :agent agent
     :model model
     :success (lambda (&rest _args)
                (message "OpenCode: message queued"))
     :error (lambda (&rest _args)
              (opencode-session--restore-input input)
                (message "OpenCode: failed to send message")))))

(defun opencode-session--maybe-send-command (connection session input)
  "Send slash command INPUT to SESSION using CONNECTION when applicable.

Falls back to a normal prompt when INPUT does not match an available command."
  (let ((buffer (current-buffer)))
    (opencode-client-commands
     connection
     :success (lambda (&rest args)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (let* ((command-info (opencode-session--parse-command-input input))
                           (command (car command-info))
                           (arguments (cadr command-info))
                           (data (plist-get args :data))
                           (items (opencode-session--command-items data))
                           (names (opencode-session--command-names items))
                           (matched (and command (member command names))))
                      (if matched
                          (progn
                            (opencode-client-session-command
                             connection
                             (opencode-session-id session)
                             command
                             arguments
                             :agent opencode-session--agent
                             :model (opencode-session--selected-model-string)
                             :success (lambda (&rest _args)
                                        (message "OpenCode command queued"))
                             :error (lambda (&rest _args)
                                      (opencode-session--restore-input input)
                                      (message "OpenCode: failed to send command")))
                            (opencode-session--clear-input))
                        (opencode-session--send-input connection session input)
                        (opencode-session--clear-input)
                        (message "OpenCode message submitted"))))))
     :error (lambda (&rest _args)
              (error "Failed to fetch OpenCode commands")))))

(defun opencode-session--restore-input (input)
  "Restore INPUT into the input area."
  (let ((inhibit-read-only t))
    (goto-char (marker-position opencode-session--input-marker))
    (insert input))
  (opencode-session--goto-input))

(defun opencode-session--clear-input ()
  "Clear the input region."
  (let ((inhibit-read-only t))
    (delete-region (marker-position opencode-session--input-start-marker)
                   (marker-position opencode-session--input-marker)))
  (opencode-session--goto-input))

(defun opencode-session--find-message (message-id)
  "Return the message with MESSAGE-ID, if any."
  (cl-find message-id opencode-session--messages
           :key #'opencode-message-id
           :test #'string=))

(defun opencode-session--upsert-message (info)
  "Update message list using INFO."
  (let* ((message-id (alist-get 'id info))
         (message (opencode-session--find-message message-id)))
    (if message
        (opencode-session--update-message message info)
      (setq message (opencode-session--message-from-info info))
      (setq opencode-session--messages
            (append opencode-session--messages (list message))))
    (when message
      (opencode-session--adopt-model-from-message message)
      (opencode-session--render-header))))

(defun opencode-session--adopt-model-from-message (message)
  "Adopt provider/model from MESSAGE for header display."
  (when (and (opencode-message-p message)
             (stringp (opencode-message-provider-id message))
             (stringp (opencode-message-model-id message))
             (not (string-empty-p (opencode-message-provider-id message)))
             (not (string-empty-p (opencode-message-model-id message))))
    (setq-local opencode-session--provider-id (opencode-message-provider-id message))
    (setq-local opencode-session--model-id (opencode-message-model-id message))))

(defun opencode-session--update-message (message info)
  "Update MESSAGE fields from INFO."
  (let* ((time (alist-get 'time info))
         (model (alist-get 'model info))
         (created (alist-get 'created time))
         (completed (alist-get 'completed time))
         (provider-id (or (alist-get 'providerID info)
                          (alist-get 'providerID model)))
         (model-id (or (alist-get 'modelID info)
                       (alist-get 'modelID model))))
    (setf (opencode-message-session-id message) (alist-get 'sessionID info))
    (setf (opencode-message-role message) (alist-get 'role info))
    (setf (opencode-message-parent-id message) (alist-get 'parentID info))
    (setf (opencode-message-model-id message) model-id)
    (setf (opencode-message-provider-id message) provider-id)
    (setf (opencode-message-mode message) (alist-get 'mode info))
    (setf (opencode-message-agent message) (alist-get 'agent info))
    (setf (opencode-message-path message) (alist-get 'path info))
    (setf (opencode-message-time-created message) created)
    (setf (opencode-message-time-completed message) completed)
    (setf (opencode-message-finish message) (alist-get 'finish info))
    (setf (opencode-message-summary message) (alist-get 'summary info))
    (setf (opencode-message-info message) info)))

(defun opencode-session--message-from-info (info)
  "Create a message object from INFO."
  (when info
    (let ((message (opencode-message-create :id (alist-get 'id info))))
      (opencode-session--update-message message info)
      message)))

(defun opencode-session--update-message-part (part delta)
  "Update message part from PART with optional DELTA."
  (let* ((message-id (alist-get 'messageID part))
         (message (opencode-session--find-message message-id)))
    (when message
      (let* ((part-id (alist-get 'id part))
             (existing (assoc part-id (opencode-message-parts message)))
             (entry (or existing (cons part-id nil)))
             (data (opencode-session--message-part-from-info part))
             (previous (cdr entry)))
        (setcdr entry data)
        (when (and delta (opencode-message-part-p previous)
                   (string= (opencode-message-part-type data) "text"))
          (setf (opencode-message-part-text data)
                (concat (opencode-message-part-text previous) delta)))
        (if existing
            (setf (opencode-message-parts message)
                  (cl-subst entry existing (opencode-message-parts message)))
          (setf (opencode-message-parts message)
                (append (opencode-message-parts message) (list entry))))
        (setf (opencode-message-text message)
              (opencode-session--message-text message))
        (opencode-session--render-message message)))))

(defun opencode-session--message-part-from-info (info)
  "Create a message part object from INFO."
  (let* ((time (alist-get 'time info))
         (start (alist-get 'start time))
         (end (alist-get 'end time)))
    (opencode-message-part-create
     :id (alist-get 'id info)
     :session-id (alist-get 'sessionID info)
     :message-id (alist-get 'messageID info)
     :type (alist-get 'type info)
     :text (alist-get 'text info)
     :metadata (alist-get 'metadata info)
     :synthetic (alist-get 'synthetic info)
     :ignored (alist-get 'ignored info)
     :time-start start
     :time-end end
     :snapshot (alist-get 'snapshot info)
     :reason (alist-get 'reason info)
     :cost (alist-get 'cost info)
     :tokens (alist-get 'tokens info)
     :tool (alist-get 'tool info)
     :state (alist-get 'state info))))

(defun opencode-session--update-session (info)
  "Update the buffer session from INFO."
  (let* ((time (alist-get 'time info))
         (created (alist-get 'created time))
         (updated (alist-get 'updated time))
         (previous-name (and opencode-session--session
                             (opencode-session--buffer-name opencode-session--session))))
    (unless opencode-session--session
      (setq opencode-session--session (opencode-session-create :id (alist-get 'id info))))
    (setf (opencode-session-slug opencode-session--session) (alist-get 'slug info))
    (setf (opencode-session-version opencode-session--session) (alist-get 'version info))
    (setf (opencode-session-project-id opencode-session--session) (alist-get 'projectID info))
    (setf (opencode-session-directory opencode-session--session) (alist-get 'directory info))
    (setf (opencode-session-title opencode-session--session) (alist-get 'title info))
    (setf (opencode-session-time-created opencode-session--session) created)
    (setf (opencode-session-time-updated opencode-session--session) updated)
    (setf (opencode-session-summary opencode-session--session) (alist-get 'summary info))
    (setf (opencode-session-info opencode-session--session) info)
    (opencode-session--rename-buffer previous-name)
    (opencode-session--render-header)))

(defun opencode-session--rename-buffer (previous-name)
  "Rename the current buffer when session metadata changes.

PREVIOUS-NAME is the previous buffer name to compare against."
  (when opencode-session--session
    (let ((new-name (opencode-session--buffer-name opencode-session--session)))
      (when (and previous-name
                 (not (string= previous-name new-name))
                 (string= (buffer-name) previous-name))
        (rename-buffer new-name t)))))

(defun opencode-session--update-status (session-id status)
  "Update STATUS for SESSION-ID."
  (when-let ((buffer (opencode-session--buffer-for-session session-id)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when opencode-session--session
          (setf (opencode-session-status opencode-session--session) status)
          (opencode-session--render-header)
          (opencode-session--maybe-start-spinner)
          (opencode-session--maybe-stop-spinner))))))

(defun opencode-session--handle-session-created (_event data)
  "Handle the session.created SSE DATA."
  (let* ((info (alist-get 'info (alist-get 'properties data)))
         (session-id (alist-get 'id info)))
    (when-let ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--update-session info))))))

(defun opencode-session--handle-session-updated (_event data)
  "Handle the session.updated SSE DATA."
  (let* ((info (alist-get 'info (alist-get 'properties data)))
         (session-id (alist-get 'id info)))
    (when-let ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--update-session info))))))

(defun opencode-session--handle-session-status (_event data)
  "Handle the session.status SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (status-info (alist-get 'status properties))
         (status (alist-get 'type status-info)))
    (opencode-session--update-status session-id status)))

(defun opencode-session--normalize-agents (data)
  "Normalize agent list DATA into a list of names."
  (let* ((agents (cond
                  ((vectorp data) (append data nil))
                  ((listp data) data)
                  (t nil)))
         (primary (cl-remove-if-not (lambda (agent)
                                      (or (stringp agent)
                                          (and (string= (alist-get 'mode agent) "primary")
                                               (not (alist-get 'hidden agent)))))
                                    agents))
         (names (mapcar (lambda (agent)
                          (cond
                           ((stringp agent) agent)
                           ((listp agent) (or (alist-get 'id agent)
                                              (alist-get 'name agent)))
                           (t nil)))
                        primary)))
    (delq nil names)))

(defun opencode-session--command-items (data)
  "Normalize command list DATA into a list."
  (cond
   ((vectorp data) (append data nil))
   ((listp data) data)
   (t nil)))

(defun opencode-session--command-names (items)
  "Return command names for ITEMS."
  (delq nil (mapcar (lambda (item)
                      (when (listp item)
                        (alist-get 'name item)))
                    items)))

(defun opencode-session--completion-in-input-p ()
  "Return non-nil when point is within the session input region."
  (when (and opencode-session--input-start-marker
             opencode-session--input-marker)
    (let ((start (marker-position opencode-session--input-start-marker))
          (end (marker-position opencode-session--input-marker))
          (pos (point)))
      (and (<= start pos) (<= pos end)))))

(defun opencode-session--completion-at-point ()
  "Return completion data for the session input area."
  (when (opencode-session--completion-in-input-p)
    (let ((providers opencode-session-completion-providers)
          (result nil))
      (while (and providers (not result))
        (setq result (funcall (car providers)))
        (setq providers (cdr providers)))
      result)))

(defun opencode-session--command-completion-bounds ()
  "Return bounds for a leading slash command completion.

Returns a cons cell (START . END) or nil when the input is not a slash command."
  (when (and opencode-session--input-start-marker
             opencode-session--input-marker)
    (let ((start (marker-position opencode-session--input-start-marker))
          (end (marker-position opencode-session--input-marker))
          (pos (point)))
      (when (and (<= start pos) (<= pos end))
        (save-excursion
          (goto-char start)
          (skip-chars-forward " \t" end)
          (when (and (< (point) end) (eq (char-after) ?/))
            (forward-char 1)
            (let ((command-start (point)))
              (skip-chars-forward "^ \t\n" end)
              (let ((command-end (point)))
                (when (and (<= command-start pos) (<= pos command-end))
                  (cons command-start command-end))))))))))

(defun opencode-session--fetch-commands (connection)
  "Fetch and cache available commands for CONNECTION."
  (let ((session-buffer (current-buffer)))
    (opencode-connection-ensure-commands
     connection
     (lambda (_items)
       (when (buffer-live-p session-buffer)
         (with-current-buffer session-buffer
           (when (eq opencode-session--connection connection)
             (completion-at-point))))))))

(defun opencode-session--complete-command ()
  "Return completion data for leading slash commands."
  (when-let ((bounds (opencode-session--command-completion-bounds)))
    (let* ((start (car bounds))
           (end (cdr bounds))
           (connection opencode-session--connection))
      (when connection
        (let ((commands (opencode-connection-commands connection)))
          (cond
           ((eq commands :loading) nil)
           ((null commands)
            (opencode-session--fetch-commands connection)
            (message "OpenCode: loading commands")
            nil)
           (t
            (let* ((items (opencode-session--command-items commands))
                   (names (opencode-session--command-names items)))
              (when (and names (listp names))
                (list start end names
                      :exclusive 'no
                      :company-prefix-length 0))))))))))

(defun opencode-session--parse-command-input (input)
  "Return (COMMAND ARGUMENTS) parsed from INPUT.

COMMAND is nil when INPUT is not a slash command." 
  (if (and (string-prefix-p "/" input)
           (string-match "^/\\([^ ]+\\)\\(?: \\(.*\\)\\)?$" input))
      (let ((command (match-string 1 input))
            (arguments (or (match-string 2 input) "")))
        (list command arguments))
    (list nil "")))

(defun opencode-session--maybe-fetch-agents (connection)
  "Fetch and cache agents for CONNECTION when needed."
  (unless (opencode-connection-agents connection)
    (let ((session-buffer (current-buffer)))
      (opencode-client-agents
       connection
       :success (lambda (&rest args)
                  (let* ((data (plist-get args :data))
                         (agents (opencode-session--normalize-agents data)))
                    (setf (opencode-connection-agents connection) agents)
                    (when (buffer-live-p session-buffer)
                      (with-current-buffer session-buffer
                        (opencode-session--apply-default-agent connection)))))
       :error (lambda (&rest _args)
                (error "OpenCode: failed to load agents"))))))

(defun opencode-session--apply-default-agent (connection)
  "Apply the default agent for the current session buffer."
  (when (and (eq connection opencode-session--connection)
             (not opencode-session--agent))
    (let ((agents (opencode-connection-agents connection)))
      (when (and agents (listp agents))
        (let* ((preferred opencode-session-default-agent)
               (index (and preferred
                           (cl-position preferred agents :test #'string=)))
               (agent (if index (nth index agents) (car agents)))
               (final-index (or index 0)))
          (setq-local opencode-session--agent agent)
          (setq-local opencode-session--agent-index final-index)
          (opencode-session--render-header))))))

(defun opencode-session--ensure-agents (connection)
  "Ensure agent list is available for CONNECTION."
  (if (opencode-connection-agents connection)
      (opencode-session--apply-default-agent connection)
    (opencode-session--maybe-fetch-agents connection)))

(defun opencode-session--ensure-providers (connection)
  "Ensure provider list is available for CONNECTION."
  (when connection
    (let ((providers (opencode-connection-providers connection)))
      (unless (or (and providers (or (vectorp providers) (listp providers)))
                  (eq providers :loading))
        (opencode-connection-ensure-providers
         connection
         (lambda (_items)
           (opencode-session--refresh-headers connection)))))))

(defun opencode-session--provider-catalog (connection)
  "Return provider catalog payload for CONNECTION."
  (when connection
    (let ((catalog (opencode-connection-provider-catalog connection)))
      (unless (eq catalog :loading)
        catalog))))

(defun opencode-session--connected-provider-ids (connection)
  "Return a list of connected provider IDs for CONNECTION."
  (let* ((catalog (opencode-session--provider-catalog connection))
         (connected (and catalog (alist-get 'connected catalog))))
    (cl-remove-if-not #'stringp (opencode-session--normalize-items connected))))

(defun opencode-session--provider-model-items (provider)
  "Return provider model entries from PROVIDER."
  (let ((models (alist-get 'models provider)))
    (cond
     ((hash-table-p models)
      (let (items)
        (maphash (lambda (model-id model-info)
                   (push (cons model-id model-info) items))
                 models)
        (nreverse items)))
     ((listp models)
      (cl-remove-if-not #'consp models))
     (t nil))))

(defun opencode-session--provider-model-candidate-display (provider-id model-id connected-p)
  "Return completion display text for PROVIDER-ID and MODEL-ID.

CONNECTED-P indicates whether PROVIDER-ID is already connected."
  (format "%s/%s%s"
          provider-id
          model-id
          (if connected-p " (connected)" "")))

(defun opencode-session--provider-model-candidates (&optional connection)
  "Return provider/model completion candidates for CONNECTION.

Each candidate is a plist with provider/model IDs and display text."
  (let* ((conn (or connection opencode-session--connection))
         (catalog (opencode-session--provider-catalog conn))
         (providers (or (opencode-session--normalize-items (and catalog (alist-get 'all catalog)))
                        (opencode-session--normalize-items (and conn (opencode-connection-providers conn)))))
         (connected (opencode-session--connected-provider-ids conn))
         entries)
    (dolist (provider providers)
      (let* ((provider-id (alist-get 'id provider))
             (provider-name (or (alist-get 'name provider) provider-id))
             (connected-p (and (stringp provider-id)
                               (member provider-id connected))))
        (when (stringp provider-id)
          (dolist (entry (opencode-session--provider-model-items provider))
            (let* ((model-id-raw (car entry))
                   (model-id (cond
                              ((stringp model-id-raw) model-id-raw)
                              ((symbolp model-id-raw) (symbol-name model-id-raw))
                              (t nil)))
                   (model-info (cdr entry))
                   (model-name (or (alist-get 'name model-info) model-id))
                   (status (alist-get 'status model-info)))
              (when (and (stringp model-id)
                         (not (string= status "deprecated")))
                (push (list :provider-id provider-id
                            :provider-name provider-name
                            :model-id model-id
                            :model-name model-name
                            :connected-p connected-p
                            :display (opencode-session--provider-model-candidate-display
                                      provider-id
                                      model-id
                                      connected-p))
                      entries)))))))
    (opencode-session--sort-model-candidates entries)))

(defun opencode-session--model-candidate-tier (candidate recent-models session-models)
  "Return the sort tier for CANDIDATE.

RECENT-MODELS is the global recently-selected list.
SESSION-MODELS is the list of models used in the current session.
Tier 0 = recently selected, 1 = session-used, 2 = connected, 3 = other."
  (let ((key (cons (plist-get candidate :provider-id)
                   (plist-get candidate :model-id))))
    (cond
     ((member key recent-models) 0)
     ((member key session-models) 1)
     ((plist-get candidate :connected-p) 2)
     (t 3))))

(defun opencode-session--model-candidate-rank (candidate tier ranked-list)
  "Return positional rank for CANDIDATE within TIER.

RANKED-LIST is the ordered list for tiers 0 and 1."
  (if (<= tier 1)
      (let ((key (cons (plist-get candidate :provider-id)
                       (plist-get candidate :model-id))))
        (or (cl-position key ranked-list :test #'equal) 0))
    0))

(defun opencode-session--sort-model-candidates (entries)
  "Sort ENTRIES by tier: recent, session-used, connected, other."
  (let ((recent opencode-session--recent-models)
        (session (opencode-session--session-used-models)))
    (sort entries
          (lambda (a b)
            (let* ((a-tier (opencode-session--model-candidate-tier a recent session))
                   (b-tier (opencode-session--model-candidate-tier b recent session))
                   (a-rank (opencode-session--model-candidate-rank a a-tier
                            (if (= a-tier 0) recent session)))
                   (b-rank (opencode-session--model-candidate-rank b b-tier
                            (if (= b-tier 0) recent session))))
              (cond
               ((< a-tier b-tier) t)
               ((> a-tier b-tier) nil)
               ((/= a-tier b-tier) nil)
               ;; Within tiers 0 and 1, sort by positional rank
               ((<= a-tier 1)
                (< a-rank b-rank))
               ;; Within tiers 2 and 3, sort alphabetically
               (t
                (let ((a-provider (downcase (or (plist-get a :provider-name) "")))
                      (b-provider (downcase (or (plist-get b :provider-name) "")))
                      (a-model (downcase (or (plist-get a :model-name) "")))
                      (b-model (downcase (or (plist-get b :model-name) ""))))
                  (if (string= a-provider b-provider)
                      (string-lessp a-model b-model)
                    (string-lessp a-provider b-provider))))))))))

(defun opencode-session--provider-model-completion-data (&optional connection)
  "Return provider/model completion data for CONNECTION.

The return value is a cons of (CHOICES . LOOKUP)."
  (let ((lookup (make-hash-table :test #'equal))
        choices)
    (dolist (candidate (opencode-session--provider-model-candidates connection))
      (let ((display (plist-get candidate :display)))
        (when (and (stringp display)
                   (not (gethash display lookup)))
          (push display choices)
          (puthash display candidate lookup))))
    (cons (nreverse choices) lookup)))

(defun opencode-session--refresh-headers (connection)
  "Re-render headers for buffers using CONNECTION."
  (maphash
   (lambda (_session-id buffer)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (eq opencode-session--connection connection)
           (opencode-session--render-header)))))
   opencode-session--buffers))

(defun opencode-session--refresh-agents (connection)
  "Refresh the cached agent list for CONNECTION."
  (setf (opencode-connection-agents connection) nil)
  (opencode-session--maybe-fetch-agents connection))

(defun opencode-session--available-agents ()
  "Return available agents for the current session buffer."
  (when opencode-session--connection
    (opencode-connection-agents opencode-session--connection)))

(defun opencode-session--set-agent (agent index)
  "Set the current session agent to AGENT at INDEX."
  (setq-local opencode-session--agent agent)
  (setq-local opencode-session--agent-index index)
  (opencode-session--render-header)
  (message "OpenCode agent: %s" agent))

(defun opencode-session-select-agent (agent)
  "Select AGENT for the current session buffer."
  (interactive
   (progn
     (unless opencode-session--connection
       (error "OpenCode session is not connected"))
     (opencode-session--ensure-agents opencode-session--connection)
     (let ((agents (opencode-session--available-agents)))
       (unless agents
         (error "OpenCode agents not available"))
       (list (completing-read "OpenCode agent: " agents nil t
                              (or opencode-session--agent (car agents)))))))
  (let* ((agents (opencode-session--available-agents))
         (index (and agents (cl-position agent agents :test #'string=))))
    (if (and index agents)
        (opencode-session--set-agent agent index)
      (message "OpenCode: unknown agent %s" agent))))

(defun opencode-session--cycle-agent (step)
  "Cycle the current agent by STEP positions."
  (unless opencode-session--connection
    (error "OpenCode session is not connected"))
  (opencode-session--ensure-agents opencode-session--connection)
  (let ((agents (opencode-session--available-agents)))
    (unless agents
      (error "OpenCode agents not available"))
    (let* ((count (length agents))
           (current (or opencode-session--agent-index 0))
           (next (mod (+ current step) count)))
      (opencode-session--set-agent (nth next agents) next))))

(defun opencode-session-next-agent ()
  "Select the next available agent."
  (interactive)
  (opencode-session--cycle-agent 1))

(defun opencode-session-previous-agent ()
  "Select the previous available agent."
  (interactive)
  (opencode-session--cycle-agent -1))

(defun opencode-session-refresh-agents ()
  "Refresh the available agents list for the session."
  (interactive)
  (unless opencode-session--connection
    (error "OpenCode session is not connected"))
  (opencode-session--refresh-agents opencode-session--connection))

(defun opencode-session--apply-model-selection (provider-id model-id)
  "Apply PROVIDER-ID and MODEL-ID as the active model.
Update recent models list, buffer state, and header."
  (let ((key (cons provider-id model-id)))
    (setq opencode-session--recent-models
          (cons key (cl-remove key opencode-session--recent-models
                               :test #'equal))))
  (setq-local opencode-session--provider-id provider-id)
  (setq-local opencode-session--model-id model-id)
  (opencode-session--render-header)
  (message "OpenCode model: %s/%s" provider-id model-id))

(defun opencode-session-select-model ()
  "Select a provider and model for the current session buffer."
  (interactive)
  (unless opencode-session--connection
    (error "OpenCode session is not connected"))
  (opencode-session--ensure-providers opencode-session--connection)
  (let ((data (opencode-session--provider-model-completion-data)))
    (unless (car data)
      (error "OpenCode providers not available"))
    (let* ((choices (car data))
           (lookup (cdr data))
           (completion-extra-properties
            '(:display-sort-function identity :cycle-sort-function identity))
           (selection (completing-read "OpenCode model: " choices nil t))
           (candidate (gethash selection lookup)))
      (unless candidate
        (error "OpenCode: unknown model selection"))
      (let ((provider-id (plist-get candidate :provider-id))
            (model-id (plist-get candidate :model-id))
            (connected-p (plist-get candidate :connected-p)))
        (if connected-p
            (opencode-session--apply-model-selection provider-id model-id)
          (let ((buffer (current-buffer)))
            (opencode-session--connect-provider
             provider-id
             (lambda (&rest _ignored)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (opencode-session--apply-model-selection
                    provider-id model-id)))))))))))

(defalias 'opencode-session-connect-provider #'opencode-session-select-model
  "Select a provider and model for the current session buffer.")

(defun opencode-session--refresh-providers (connection &optional on-success)
  "Force refresh the provider cache for CONNECTION.

ON-SUCCESS is called when providers are loaded."
  (setf (opencode-connection-providers connection) nil)
  (setf (opencode-connection-provider-catalog connection) nil)
  (opencode-connection-ensure-providers
   connection
   (lambda (items)
     (opencode-session--refresh-headers connection)
     (when on-success
       (funcall on-success items)))))

(defun opencode-session--post-auth-refresh (connection callback)
  "Dispose instance state for CONNECTION, then refresh providers.

CALLBACK is passed through to `opencode-session--refresh-providers'."
  (let ((restart-sse
         (lambda ()
           (opencode-sse-close connection)
           (opencode-sse-open connection))))
    (opencode-client-instance-dispose
     connection
     :success (lambda (&rest _args)
                (funcall restart-sse)
                (opencode-session--refresh-providers connection callback))
     :error (lambda (&rest _args)
              (message "OpenCode: failed to dispose instance; refreshing providers")
              (funcall restart-sse)
              (opencode-session--refresh-providers connection callback)))))

(defun opencode-session--connect-provider (provider-id callback)
  "Run the auth flow for PROVIDER-ID, then call CALLBACK on success."
  (let ((connection opencode-session--connection))
    (unless connection
      (error "OpenCode session is not connected"))
    (message "OpenCode: fetching auth methods for %s..." provider-id)
    (opencode-client-provider-auth-methods
     connection
     :success (lambda (&rest args)
                (let* ((data (plist-get args :data))
                       (methods (opencode-session--provider-auth-methods
                                 provider-id data)))
                  (opencode-session--run-auth-flow
                   connection provider-id methods callback)))
     :error (lambda (&rest _args)
              (message "OpenCode: failed to fetch auth methods, trying API key")
              (opencode-session--run-auth-flow
               connection provider-id
               '(((type . "api") (label . "API key")))
               callback)))))

(defun opencode-session--provider-auth-methods (provider-id data)
  "Return auth methods for PROVIDER-ID from DATA.

Falls back to a single API key method when none are found."
  (let* ((methods (or (alist-get (intern provider-id) data)
                      (alist-get provider-id data nil nil #'string=))))
    (if (and methods (or (listp methods) (vectorp methods)))
        (opencode-session--normalize-items methods)
      '(((type . "api") (label . "API key"))))))

(defun opencode-session--run-auth-flow (connection provider-id methods callback)
  "Run auth for PROVIDER-ID on CONNECTION using METHODS, then CALLBACK."
  (let* ((method (if (= (length methods) 1)
                     (car methods)
                   (opencode-session--choose-auth-method methods)))
         (method-type (alist-get 'type method))
         (method-index (cl-position method methods :test #'equal)))
    (cond
     ((string= method-type "api")
      (opencode-session--auth-api-key connection provider-id callback))
     ((string= method-type "oauth")
      (opencode-session--auth-oauth
       connection provider-id method-index callback))
     (t (error "OpenCode: unsupported auth method type %s" method-type)))))

(defun opencode-session--choose-auth-method (methods)
  "Prompt the user to choose from METHODS."
  (let* ((labels (mapcar (lambda (m) (alist-get 'label m)) methods))
         (completion-extra-properties
          '(:display-sort-function identity :cycle-sort-function identity))
         (selection (completing-read "OpenCode auth method: " labels nil t))
         (index (cl-position selection labels :test #'string=)))
    (nth index methods)))

(defun opencode-session--auth-api-key (connection provider-id callback)
  "Prompt for an API key for PROVIDER-ID on CONNECTION, then CALLBACK."
  (let ((key (read-string (format "API key for %s: " provider-id))))
    (when (string-empty-p key)
      (error "OpenCode: API key cannot be empty"))
    (message "OpenCode: setting API key for %s..." provider-id)
    (opencode-client-auth-set
     connection
     provider-id
     `((type . "api") (key . ,key))
     :success (lambda (&rest _args)
                (message "OpenCode: %s connected" provider-id)
                (opencode-session--post-auth-refresh connection callback))
     :error (lambda (&rest _args)
              (message "OpenCode: failed to set API key for %s" provider-id)))))

(defun opencode-session--auth-oauth (connection provider-id method-index callback)
  "Run OAuth flow for PROVIDER-ID on CONNECTION using METHOD-INDEX.

CALLBACK is called on successful authorization."
  (message "OpenCode: starting OAuth for %s..." provider-id)
  (opencode-client-provider-oauth-authorize
   connection
   provider-id
   method-index
   :success (lambda (&rest args)
              (let* ((data (plist-get args :data))
                     (url (alist-get 'url data))
                     (method (alist-get 'method data))
                     (instructions (alist-get 'instructions data)))
                (when instructions
                  (message "OpenCode: %s" instructions))
                (when url
                  (let ((browse-url-browser-function #'browse-url-default-browser))
                    (browse-url url)))
                (cond
                 ((string= method "code")
                  (opencode-session--auth-oauth-code
                   connection provider-id method-index callback))
                 ((string= method "auto")
                  (opencode-session--auth-oauth-auto
                   connection provider-id method-index callback))
                 (t (error "OpenCode: unknown OAuth method %s" method)))))
   :error (lambda (&rest _args)
            (message "OpenCode: OAuth authorization failed for %s"
                     provider-id))))

(defun opencode-session--auth-oauth-code (connection provider-id method-index callback)
  "Complete OAuth code flow for PROVIDER-ID on CONNECTION.

METHOD-INDEX identifies the auth method.  CALLBACK is called on success."
  (let ((code (read-string
               (format "Authorization code for %s: " provider-id))))
    (when (string-empty-p code)
      (error "OpenCode: authorization code cannot be empty"))
    (message "OpenCode: completing OAuth for %s..." provider-id)
    (opencode-client-provider-oauth-callback
     connection
     provider-id
     method-index
     :code code
     :success (lambda (&rest _args)
                (message "OpenCode: %s connected" provider-id)
                (opencode-session--post-auth-refresh connection callback))
     :error (lambda (&rest _args)
              (message "OpenCode: OAuth callback failed for %s"
                       provider-id)))))

(defun opencode-session--auth-oauth-auto (connection provider-id method-index callback)
  "Complete OAuth auto flow for PROVIDER-ID on CONNECTION.

METHOD-INDEX identifies the auth method.  CALLBACK is called on success.
This is a long-polling call that waits for browser authorization."
  (message "OpenCode: waiting for browser authorization for %s..." provider-id)
  (opencode-client-provider-oauth-callback
   connection
   provider-id
   method-index
   :success (lambda (&rest _args)
              (message "OpenCode: %s connected" provider-id)
              (opencode-session--post-auth-refresh connection callback))
   :error (lambda (&rest _args)
            (message "OpenCode: OAuth callback failed for %s"
                     provider-id))))

(defun opencode-session-interrupt ()
  "Interrupt the active prompt for the current session."
  (interactive)
  (unless (and opencode-session--connection opencode-session--session)
    (error "OpenCode session is not connected"))
  (let ((session-id (opencode-session-id opencode-session--session)))
    (unless session-id
      (error "OpenCode session ID is missing"))
    (opencode-client-session-abort
     opencode-session--connection
     session-id
     :success (lambda (&rest _args)
                (message "OpenCode: interrupt requested"))
     :error (lambda (&rest _args)
              (message "OpenCode: failed to interrupt session")))))

(defun opencode-session--handle-session-idle (_event data)
  "Handle the session.idle SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties)))
    (opencode-session--update-status session-id "idle")))

(defun opencode-session--permission-patterns (permission)
  "Return a list of pattern strings from PERMISSION."
  (let ((patterns (alist-get 'patterns permission)))
    (cond
     ((vectorp patterns) (append patterns nil))
     ((listp patterns) patterns)
     (t nil))))

(defun opencode-session--permission-detail (permission)
  "Return a detail string for PERMISSION when available."
  (let* ((kind (alist-get 'permission permission))
         (metadata (alist-get 'metadata permission))
         (patterns (opencode-session--permission-patterns permission))
         (pattern (car patterns)))
    (cond
     ((and (string= kind "read") (alist-get 'filePath metadata))
      (format "read %s" (alist-get 'filePath metadata)))
     ((and (string= kind "edit") (alist-get 'filepath metadata))
      (format "edit %s" (alist-get 'filepath metadata)))
     ((and (string= kind "glob") (alist-get 'pattern metadata))
      (format "glob %s" (alist-get 'pattern metadata)))
     ((and (string= kind "grep") (alist-get 'pattern metadata))
      (format "grep %s" (alist-get 'pattern metadata)))
     ((and (string= kind "list") (alist-get 'path metadata))
      (format "list %s" (alist-get 'path metadata)))
     ((and (string= kind "bash") (alist-get 'command metadata))
      (if-let ((description (alist-get 'description metadata)))
          (format "%s (%s)" description (alist-get 'command metadata))
        (format "%s" (alist-get 'command metadata))))
     ((and (string= kind "task") (alist-get 'subagent_type metadata))
      (format "task %s" (alist-get 'subagent_type metadata)))
     ((and (string= kind "webfetch") (alist-get 'url metadata))
      (format "web search %s" (alist-get 'url metadata)))
     ((and (member kind '("websearch" "codesearch")) (alist-get 'query metadata))
      (format "%s %s" (capitalize kind) (alist-get 'query metadata)))
     ((and (string= kind "external_directory") pattern)
      (format "access external directory %s" pattern))
     (pattern
      (format "%s" pattern))
     (t nil))))

(defun opencode-session--permission-prompt-label (permission)
  "Return the minibuffer prompt label for PERMISSION."
  (let* ((kind (alist-get 'permission permission))
         (detail (opencode-session--permission-detail permission))
         (fallback (if kind (format "use %s" kind) "proceed")))
    (format "OpenCode wants to %s: " (or detail fallback))))

(defun opencode-session--prompt-permission (permission)
  "Prompt for PERMISSION and send a response."
  (let* ((request-id (alist-get 'id permission))
         (session-id (alist-get 'sessionID permission))
         (choices '("Allow once" "Allow always" "Deny"))
         (prompt (opencode-session--permission-prompt-label permission))
         (selection (condition-case nil
                        (completing-read prompt choices nil t)
                      (quit "Deny")))
         (reply (cond
                 ((string= selection "Allow always") "always")
                 ((string= selection "Allow once") "once")
                 (t "reject"))))
    (unless opencode-session--connection
      (error "OpenCode session is not connected"))
    (unless request-id
      (error "OpenCode permission request is missing ID"))
    (opencode-client-permission-reply
     opencode-session--connection
     request-id
     reply
     :success (lambda (&rest _args)
                (message "OpenCode permission reply sent"))
     :error (lambda (&rest _args)
              (message "OpenCode: failed to reply to permission request")))))

(defun opencode-session--question-list (questions)
  "Normalize QUESTIONS into a list."
  (cond
   ((vectorp questions) (append questions nil))
   ((listp questions) questions)
   (t nil)))

(defun opencode-session--question-options (question)
  "Return option labels for QUESTION."
  (let ((options (alist-get 'options question)))
    (mapcar (lambda (option) (alist-get 'label option))
            (opencode-session--normalize-items options))))

(defun opencode-session--question-multiple-p (question)
  "Return non-nil if QUESTION allows multiple answers."
  (eq (alist-get 'multiple question) t))

(defun opencode-session--question-custom-p (question)
  "Return non-nil if QUESTION allows custom answers."
  (let ((custom (alist-get 'custom question :missing)))
    (not (or (eq custom :json-false)
             (eq custom json-false)
             (eq custom nil)))))

(defun opencode-session--question-prompt-label (question)
  "Return the minibuffer prompt label for QUESTION."
  (let ((header (alist-get 'header question))
        (text (alist-get 'question question)))
    (if (and header (not (string-empty-p header)))
        (format "OpenCode %s: %s " header text)
      (format "OpenCode: %s " text))))

(defun opencode-session--question-read-custom (prompt)
  "Read a custom answer using PROMPT."
  (read-string (concat prompt "(Other): ")))

(defun opencode-session--question-read-single (question)
  "Prompt for a single answer to QUESTION.

Returns a list containing one answer string."
  (let* ((prompt (opencode-session--question-prompt-label question))
         (options (opencode-session--question-options question))
         (custom (opencode-session--question-custom-p question))
         (choices (if custom (append options '("Other")) options))
         (selection (completing-read prompt choices nil t)))
    (if (and custom (string= selection "Other"))
        (list (opencode-session--question-read-custom prompt))
      (list selection))))

(defun opencode-session--question-read-multiple (question)
  "Prompt for multiple answers to QUESTION.

Returns a list of answer strings."
  (let* ((prompt (opencode-session--question-prompt-label question))
         (options (opencode-session--question-options question))
         (custom (opencode-session--question-custom-p question))
         (choices (if custom (append options '("Other")) options))
         (selection (completing-read-multiple prompt choices nil t)))
    (if (and custom (member "Other" selection))
        (let ((custom-answer (opencode-session--question-read-custom prompt)))
          (append (remove "Other" selection) (list custom-answer)))
      selection)))

(defun opencode-session--question-answers (questions)
  "Return answers for QUESTIONS via minibuffer prompts."
  (mapcar (lambda (question)
            (if (opencode-session--question-multiple-p question)
                (opencode-session--question-read-multiple question)
              (opencode-session--question-read-single question)))
          questions))

(defun opencode-session--prompt-question (payload)
  "Prompt for question PAYLOAD and send a response."
  (let* ((request-id (alist-get 'id payload))
         (session-id (alist-get 'sessionID payload))
         (questions (opencode-session--question-list (alist-get 'questions payload)))
         (answers (condition-case nil
                      (opencode-session--question-answers questions)
                    (quit :reject))))
    (unless opencode-session--connection
      (error "OpenCode session is not connected"))
    (unless request-id
      (error "OpenCode question request is missing ID"))
    (if (eq answers :reject)
        (opencode-client-question-reject
         opencode-session--connection
         request-id
         :success (lambda (&rest _args)
                    (message "OpenCode question rejected"))
         :error (lambda (&rest _args)
                  (message "OpenCode: failed to reject question")))
      (opencode-client-question-reply
       opencode-session--connection
       request-id
       answers
       :success (lambda (&rest _args)
                  (message "OpenCode question reply sent"))
       :error (lambda (&rest _args)
                (message "OpenCode: failed to reply to question"))))))

(defun opencode-session--handle-permission-asked (_event data)
  "Handle the permission.asked SSE DATA."
  (let* ((permission (alist-get 'properties data))
         (session-id (alist-get 'sessionID permission)))
    (when-let ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--prompt-permission permission))))))

(defun opencode-session--handle-question-asked (_event data)
  "Handle the question.asked SSE DATA."
  (let* ((question (alist-get 'properties data))
         (session-id (alist-get 'sessionID question))
         (request-id (alist-get 'id question)))
    (message "OpenCode: question.asked for %s (session %s)" request-id session-id)
    (when-let ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (message "OpenCode: prompting question %s in %s" request-id (buffer-name))
          (opencode-session--prompt-question question))))))

(defun opencode-session--handle-message-updated (_event data)
  "Handle the message.updated SSE DATA."
  (let* ((info (alist-get 'info (alist-get 'properties data)))
         (session-id (alist-get 'sessionID info)))
    (when-let ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--upsert-message info))))))

(defun opencode-session--handle-message-part-updated (_event data)
  "Handle the message.part.updated SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (part (alist-get 'part properties))
         (session-id (alist-get 'sessionID part))
         (delta (alist-get 'delta properties)))
    (when-let ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (member (alist-get 'type part) '("text" "tool"))
            (opencode-session--update-message-part part delta)))))))

(defun opencode-session--connection-directories ()
  "Return directories for active OpenCode connections."
  (let (directories)
    (maphash (lambda (_session-id buffer)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when-let ((connection opencode-session--connection)
                              (directory (opencode-connection-directory connection)))
                     (push directory directories)))))
             opencode-session--buffers)
    (delete-dups (delq nil directories))))

(defun opencode-session--normalize-file-path (path)
  "Normalize PATH for buffer lookup.

Returns nil when PATH is not a string."
  (when (stringp path)
    (let* ((expanded (expand-file-name path))
           (directories (opencode-session--connection-directories)))
      (cond
       ((file-name-absolute-p expanded)
        (if (file-exists-p expanded)
            (file-truename expanded)
          expanded))
       (directories
        (let ((candidate (cl-find-if
                          #'file-exists-p
                          (mapcar (lambda (directory)
                                    (expand-file-name path directory))
                                  directories))))
          (if candidate
              (file-truename candidate)
            (expand-file-name path (car directories)))))
       (t expanded)))))

(defun opencode-session--event-file-paths (data)
  "Return a list of file paths from SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (file (alist-get 'file properties))
         (path (alist-get 'path properties))
         (file-path (cond
                     ((stringp file) file)
                     ((listp file) (or (alist-get 'path file)
                                       (alist-get 'name file)))))
         (paths (or (alist-get 'paths properties)
                    (alist-get 'files properties))))
    (cond
     ((and paths (vectorp paths)) (append paths nil))
     ((listp paths) paths)
     ((stringp path) (list path))
     ((stringp file-path) (list file-path))
     (t nil))))

(defun opencode-session--maybe-revert-buffer (path)
  "Revert buffers visiting PATH when safe."
  (let ((normalized (opencode-session--normalize-file-path path)))
    (when normalized
      (dolist (buffer (buffer-list))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when-let ((buffer-path (buffer-file-name buffer)))
              (let ((normalized-buffer (opencode-session--normalize-file-path buffer-path)))
                (when (and normalized-buffer
                           (string= normalized normalized-buffer))
                  (if (buffer-modified-p)
                      (message "OpenCode: buffer has unsaved changes (%s)" (buffer-name buffer))
                    (revert-buffer :ignore-auto :noconfirm)
                    (message "OpenCode: reloaded %s" (buffer-name buffer))))))))))))

(defun opencode-session--handle-file-updated (_event data)
  "Handle SSE file update DATA by reverting buffers."
  (dolist (path (opencode-session--event-file-paths data))
    (opencode-session--maybe-revert-buffer path)))

(defun opencode-session--load-history (connection session buffer &optional on-history-loaded)
  "Load existing messages for SESSION using CONNECTION into BUFFER.

Call ON-HISTORY-LOADED with BUFFER after the request completes."
  (opencode-client-session-messages
   connection
   (opencode-session-id session)
   :success (lambda (&rest args)
              (let* ((data (plist-get args :data))
                     (items (cond
                             ((listp data) data)
                             ((vectorp data) (append data nil))
                             (t nil))))
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (setq opencode-session--messages nil)
                    (dolist (item items)
                      (opencode-session--hydrate-message item))
                    (opencode-session--render-buffer))
                  (when on-history-loaded
                    (funcall on-history-loaded buffer)))))
   :error (lambda (&rest _args)
            (message "OpenCode: failed to load session history")
            (when on-history-loaded
              (funcall on-history-loaded buffer)))))

(defun opencode-session--hydrate-message (item)
  "Add a message ITEM returned from the API."
  (let* ((info (alist-get 'info item))
         (parts (alist-get 'parts item))
         (message (opencode-session--message-from-info info)))
    (when message
      (setf (opencode-message-parts message)
            (opencode-session--hydrate-parts parts))
      (setf (opencode-message-text message)
            (opencode-session--message-text message))
      (setq opencode-session--messages
            (append opencode-session--messages (list message))))))

(defun opencode-session--hydrate-parts (parts)
  "Hydrate PARTS into an alist of message parts."
  (let (result)
    (dolist (part (opencode-session--normalize-items parts))
      (let* ((part-id (alist-get 'id part))
             (data (opencode-session--message-part-from-info part))
             (existing (assoc part-id result)))
        (if existing
            (setcdr existing data)
          (push (cons part-id data) result))))
    (nreverse result)))

(defun opencode-session--normalize-items (items)
  "Normalize ITEMS to a list when vector or list."
  (cond
   ((vectorp items) (append items nil))
   ((listp items) items)
   (t nil)))

(opencode-sse-define-handler session-created "session.created" (_event data)
  (opencode-session--handle-session-created _event data))

(opencode-sse-define-handler session-updated "session.updated" (_event data)
  (opencode-session--handle-session-updated _event data))

(opencode-sse-define-handler session-status "session.status" (_event data)
  (opencode-session--handle-session-status _event data))

(opencode-sse-define-handler session-idle "session.idle" (_event data)
  (opencode-session--handle-session-idle _event data))

(opencode-sse-define-handler permission-asked "permission.asked" (_event data)
  (opencode-session--handle-permission-asked _event data))

(opencode-sse-define-handler question-asked "question.asked" (_event data)
  (opencode-session--handle-question-asked _event data))

(opencode-sse-define-handler message-updated "message.updated" (_event data)
  (opencode-session--handle-message-updated _event data))

(opencode-sse-define-handler message-part-updated "message.part.updated" (_event data)
  (opencode-session--handle-message-part-updated _event data))

(opencode-sse-define-handler file-edited "file.edited" (_event data)
  (opencode-session--handle-file-updated _event data))

(opencode-sse-define-handler file-watcher-updated "file.watcher.updated" (_event data)
  (opencode-session--handle-file-updated _event data))

(provide 'emacs-opencode-session-mode)

;;; emacs-opencode-session-mode.el ends here
