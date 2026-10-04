;;; emacs-opencode-session-header.el --- Session header and spinner  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-opencode-session-vars)
(require 'emacs-opencode-message)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-session)

(declare-function opencode-session--ensure-providers "emacs-opencode-session-model")
(declare-function opencode-session--provider-model-info "emacs-opencode-session-model")
(declare-function opencode-session--render-retry-banner "emacs-opencode-session-render")

(defcustom opencode-session-spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Spinner frames used in the session header for busy states."
  :type '(repeat string)
  :group 'emacs-opencode)

(defcustom opencode-session-spinner-interval 0.1
  "Seconds between session header spinner frames."
  :type 'number
  :group 'emacs-opencode)

(defcustom opencode-session-header-retry-message-max 60
  "Maximum width in characters for retry messages shown in the header."
  :type 'integer
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

(defface opencode-session-error-face
  '((t :inherit error))
  "Face used for inline session error and retry messages."
  :group 'emacs-opencode)

(defvar opencode-session--spinner-timer nil
  "Timer used to animate session header spinners.")

(defvar-local opencode-session--spinner-index 0
  "Current spinner frame index for the session buffer.")

(defun opencode-session--render-header ()
  "Render the header line for the session."
  (let* ((title (or (opencode-session-title opencode-session--session)
                    "OpenCode Session"))
          (status (opencode-session-status opencode-session--session))
          (agent opencode-session--agent)
          (agent-label (when (and agent (not (string-empty-p agent)))
                         (format "[%s]" agent)))
          (spinner (opencode-session--header-spinner-segment status))
          (retry (opencode-session--header-retry-segment status))
          (right (opencode-session--header-right)))
    (setq header-line-format
          (opencode-session--align-header
           (string-join
            (delq nil
                  (list (propertize title 'face 'opencode-session-header-face)
                        (when agent-label
                          (propertize agent-label 'face 'opencode-session-agent-face))
                        spinner
                        retry))
            " ")
           right))))

(defun opencode-session--header-spinner-segment (status)
  "Return the propertized spinner segment for STATUS, or nil."
  (let ((label (opencode-session--header-status-label status)))
    (unless (or (null label) (string-empty-p label))
      (propertize label 'face 'opencode-session-spinner-face))))

(defun opencode-session--header-retry-segment (status)
  "Return a propertized retry segment for STATUS, or nil."
  (when (and (opencode-status-p status)
             (string= (or (opencode-status-type status) "") "retry"))
    (let* ((message (opencode-status-message status))
           (attempt (opencode-status-attempt status))
           (next (opencode-status-next status))
           (truncated (opencode-session--truncate-retry-message message))
           (suffix (opencode-session--retry-countdown-suffix attempt next))
           (text (string-join (delq nil (list truncated suffix)) " ")))
      (unless (string-empty-p text)
        (propertize text 'face 'opencode-session-error-face)))))

(defun opencode-session--truncate-retry-message (message)
  "Truncate MESSAGE for header display."
  (when (and (stringp message) (not (string-empty-p message)))
    (let ((max opencode-session-header-retry-message-max))
      (if (and (numberp max) (> (length message) max))
          (concat (substring message 0 (max 0 (- max 3))) "...")
        message))))

(defun opencode-session--retry-countdown-suffix (attempt next)
  "Build a [#ATTEMPT, in Ns] suffix from ATTEMPT and NEXT.

NEXT is in milliseconds since epoch, as sent by the server."
  (let* ((parts nil))
    (when (numberp attempt)
      (push (format "#%d" attempt) parts))
    (when (numberp next)
      (let* ((now-ms (* 1000.0 (float-time)))
             (remaining (max 0 (round (/ (- next now-ms) 1000.0)))))
        (push (format "in %ds" remaining) parts)))
    (when parts
      (format "[%s]" (string-join (nreverse parts) ", ")))))

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
         (variant (opencode-session--header-variant-string))
         (context (opencode-session--header-context-string))
         (cost (and
                (not (string-empty-p (string-trim (or context ""))))
                (opencode-session--header-cost-string))))
    (when (or model variant context cost)
      (propertize
       (string-join
        (delq nil (list model variant context (when cost (format "(%s)" cost))))
        " ")
       'face 'opencode-session-status-face))))

(defun opencode-session--header-model-string ()
  "Return the active provider/model string for the session header."
  (when-let* ((model (opencode-session--active-model)))
    (format "%s/%s" (car model) (cdr model))))

(defun opencode-session--header-variant-string ()
  "Return the active variant string for the session header."
  (when (and (opencode-session--header-model-string)
             (stringp opencode-session--variant)
             (not (string-empty-p opencode-session--variant)))
    (format "[%s]" opencode-session--variant)))

(defun opencode-session--active-model ()
  "Return active (PROVIDER-ID . MODEL-ID) for this buffer."
  (or (and opencode-session--provider-id opencode-session--model-id
           (cons opencode-session--provider-id opencode-session--model-id))
      (opencode-session--last-message-model)))

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
  "Return the session's total cost formatted as currency.
The server's session total includes title and compaction requests; the
sum of assistant messages is the fallback."
  (let ((total (or (let ((cost (and opencode-session--session
                                    (alist-get 'cost (opencode-session-info
                                                      opencode-session--session)))))
                     (and (numberp cost) cost))
                   (opencode-session--assistant-cost-total))))
    (when (and (numberp total)
               (or (> total 0)
                   (opencode-session--has-assistant-messages-p)))
      (format "$%.2f" total))))

(defun opencode-session--has-assistant-messages-p ()
  "Return non-nil when session has assistant messages."
  (cl-loop for message in opencode-session--messages
           when (equal (opencode-message-role message) "assistant")
           return t))

(defun opencode-session--assistant-cost-total ()
  "Return the total cost for assistant messages in the session."
  (let ((total 0.0))
    (dolist (message opencode-session--messages total)
      (let ((cost (alist-get 'cost (opencode-message-info message))))
        (when (and (equal (opencode-message-role message) "assistant") (numberp cost))
          (setq total (+ total cost)))))))

(defun opencode-session--header-context-string ()
  "Return the context usage string for the session header."
  (when-let* ((info (opencode-session--last-assistant-info)))
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
           for tokens = (alist-get 'tokens info)
           for output = (alist-get 'output tokens)
           when (and (equal (opencode-message-role message) "assistant")
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
    (let* ((model (alist-get 'model info))
           (provider-id (alist-get 'providerID model))
           (model-id (alist-get 'id model))
           (limit (opencode-session--model-context-limit provider-id model-id)))
      (when (and (numberp limit) (> limit 0))
        (number-to-string (round (* (/ (float total) limit) 100)))))))

(defun opencode-session--model-context-limit (provider-id model-id)
  "Return the context limit for PROVIDER-ID and MODEL-ID."
  (when (and opencode-session--connection provider-id model-id)
    (alist-get 'context
               (alist-get 'limit (opencode-session--provider-model-info
                                  provider-id model-id opencode-session--connection)))))

(defun opencode-session--header-status-label (status)
  "Return STATUS label for the header line."
  (if (opencode-session--status-busy-p status)
      (opencode-session--spinner-frame)
    ""))

(defun opencode-session--status-busy-p (status)
  "Return non-nil when STATUS should show a spinner.

STATUS may be an `opencode-status' struct (preferred), a legacy
string status type, or nil.  Nil and \"idle\" are not busy."
  (opencode-status-busy-p status))

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
  "Advance spinner frames for visible session buffers.

Also re-renders the header and the inline retry banner so the retry
countdown stays accurate while the timer is running."
  (maphash
   (lambda (_session-id buffer)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (and opencode-session--session
                    (opencode-session--status-busy-p
                     (opencode-session-status opencode-session--session)))
           (setq opencode-session--spinner-index
                 (1+ opencode-session--spinner-index))
           (opencode-session--render-header)
           (opencode-session--render-retry-banner)))))
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
                       (opencode-session-status opencode-session--session)))
             (setq busy t)))))
     opencode-session--buffers)
    busy))

(provide 'emacs-opencode-session-header)

;;; emacs-opencode-session-header.el ends here
