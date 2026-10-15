;;; emacs-opencode-session-model.el --- Agent, model, and variant selection  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-opencode-session-vars)
(require 'emacs-opencode-session)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-client)

(declare-function opencode-session--render-header "emacs-opencode-session-header")
(declare-function opencode-session--active-model "emacs-opencode-session-header")
(declare-function opencode-session--session-used-models "emacs-opencode-session-header")
(declare-function opencode-session--ensure-connection "emacs-opencode-session-mode" (callback))
(declare-function opencode-session--read-form-answer "emacs-opencode-session-handlers" (fields))
(declare-function browse-url-default-browser "browse-url" (url &rest args))

(defcustom opencode-session-default-agent "plan"
  "Default agent name for new OpenCode sessions."
  :type 'string
  :group 'emacs-opencode)

(defcustom opencode-session-default-variant nil
  "Default model variant name for new OpenCode sessions.

When nil, do not auto-select a model variant."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Variant name"))
  :group 'emacs-opencode)

(defcustom opencode-session-oauth-poll-interval 2
  "Seconds between checks while waiting for a browser OAuth sign-in."
  :type 'number
  :group 'emacs-opencode)

(defvar opencode-session--recent-models nil
  "Global list of recently selected (PROVIDER-ID . MODEL-ID) pairs.
Most recently selected first.")

(defvar-local opencode-session--agent-index nil
  "Index of the selected agent in the available agents list.")

(defvar-local opencode-session--variant-index nil
  "Index of the selected variant in the available variants list.")

(defun opencode-session--session-id ()
  "Return the current buffer's session ID, or signal an error."
  (or (and opencode-session--session
           (opencode-session-id opencode-session--session))
      (error "OpenCode session is not connected")))

;;; Agent management

(defun opencode-session--normalize-agent-data (data)
  "Normalize raw agent DATA into a list of alists.
Each element is an alist with at least `name' (or `id') and `mode' keys."
  (let ((agents (cond
                 ((vectorp data) (append data nil))
                 ((listp data) data)
                 (t nil))))
    (cl-remove-if-not (lambda (agent)
                        (or (stringp agent) (listp agent)))
                      agents)))

(defun opencode-session--agent-name (agent)
  "Return the name string for AGENT.
AGENT may be a string or an alist."
  (cond
   ((stringp agent) agent)
   ((listp agent) (or (alist-get 'id agent)
                      (alist-get 'name agent)))
   (t nil)))

(defun opencode-session--normalize-agents (data)
  "Normalize agent list DATA into a list of selectable primary agent names.
Agents in mode \"all\" can run as primary agents too."
  (let* ((agents (opencode-session--normalize-agent-data data))
         (primary (cl-remove-if-not (lambda (agent)
                                      (or (stringp agent)
                                          (and (member (alist-get 'mode agent) '("primary" "all"))
                                               (not (alist-get 'hidden agent)))))
                                    agents))
         (names (mapcar #'opencode-session--agent-name primary)))
    (delq nil names)))

(defun opencode-session--completable-agent-names (data)
  "Return a list of agent names suitable for @-mention completion from DATA.
Includes non-hidden agents that are not in primary mode (i.e., subagents)."
  (let* ((agents (opencode-session--normalize-agent-data data))
         (completable (cl-remove-if-not
                       (lambda (agent)
                         (and (listp agent)
                              (not (alist-get 'hidden agent))
                              (not (string= (alist-get 'mode agent) "primary"))))
                       agents))
         (names (mapcar #'opencode-session--agent-name completable)))
    (delq nil names)))

(defun opencode-session--store-agents (connection data)
  "Cache agent list DATA on CONNECTION."
  (let ((cache (opencode-connection-context connection)))
    (setf (opencode-connection-agents cache) (opencode-session--normalize-agents data))
    (setf (opencode-connection-agents-raw cache) (opencode-session--normalize-agent-data data))))

(defun opencode-session--maybe-fetch-agents (connection)
  "Fetch and cache agents for CONNECTION when needed."
  (unless (opencode-connection-agents (opencode-connection-context connection))
    (let ((session-buffer (current-buffer)))
      (opencode-client-agents
       connection
       :success (lambda (&rest args)
                  (opencode-session--store-agents connection (plist-get args :data))
                  (when (buffer-live-p session-buffer)
                    (with-current-buffer session-buffer
                      (opencode-session--apply-default-agent connection))))
       :error (lambda (&rest _args)
                (message "OpenCode: failed to load agents"))))))

(defun opencode-session--apply-default-agent (connection)
  "Show the agent the current session will use when none is known yet.
A session without an agent runs the server's default, the first primary
agent."
  (when (and (eq connection opencode-session--connection)
             (not opencode-session--agent))
    (let ((agents (opencode-connection-agents (opencode-connection-context connection)))
          (session-agent (and opencode-session--session
                              (alist-get 'agent (opencode-session-info opencode-session--session)))))
      (when (and agents (listp agents))
        (opencode-session--set-agent-from-server (or session-agent (car agents)))))))

(defconst opencode-session--agent-load-attempts 20
  "Times to ask for agents while a freshly started server loads them.")

(defun opencode-session--load-agents (connection on-ready &optional attempt)
  "Fetch CONNECTION's agents and call ON-READY with the primary names.
A server answers with an empty list until it has loaded the directory's
configuration, so retry briefly; ATTEMPT counts the tries so far."
  (let ((attempt (or attempt 1))
        (directory default-directory))
    (opencode-client-agents
     connection
     :success (lambda (&rest args)
                (let ((data (plist-get args :data)))
                  (if (and (null data) (< attempt opencode-session--agent-load-attempts))
                      (run-at-time
                       0.25 nil
                       (lambda ()
                         (let ((default-directory directory))
                           (opencode-session--load-agents connection on-ready (1+ attempt)))))
                    (opencode-session--store-agents connection data)
                    (funcall on-ready (opencode-connection-agents (opencode-connection-context connection))))))
     :error (lambda (&rest _args)
              (funcall on-ready nil)))))

(defun opencode-session--ensure-agents (connection &optional on-ready)
  "Ensure agent list is available for CONNECTION.
ON-READY, when non-nil, is called with the primary agent names once
they are loaded."
  (if (opencode-connection-agents (opencode-connection-context connection))
      (progn
        (opencode-session--apply-default-agent connection)
        (when on-ready
          (funcall on-ready (opencode-connection-agents (opencode-connection-context connection)))))
    (if on-ready
        (let ((buffer (current-buffer)))
          (opencode-session--load-agents
           connection
           (lambda (agents)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (derived-mode-p 'opencode-session-mode)
                   (opencode-session--apply-default-agent connection))))
             (funcall on-ready agents))))
      (opencode-session--maybe-fetch-agents connection))))

(defun opencode-session--refresh-agents (connection)
  "Refresh the cached agent list for CONNECTION."
  (setf (opencode-connection-agents (opencode-connection-context connection)) nil)
  (setf (opencode-connection-agents-raw (opencode-connection-context connection)) nil)
  (opencode-session--maybe-fetch-agents connection))

(defun opencode-session--available-agents ()
  "Return available agents for the current session buffer."
  (when opencode-session--connection
    (opencode-connection-agents (opencode-connection-context opencode-session--connection))))

(defun opencode-session--available-completable-agents ()
  "Return agent names available for @-mention completion.
These are non-hidden, non-primary agents (subagents)."
  (when opencode-session--connection
    (let ((raw (opencode-connection-agents-raw (opencode-connection-context opencode-session--connection))))
      (when raw
        (opencode-session--completable-agent-names raw)))))

(defun opencode-session--set-agent-from-server (agent)
  "Record AGENT as the session's agent without contacting the server."
  (setq-local opencode-session--agent agent)
  (setq-local opencode-session--agent-index
              (cl-position agent (opencode-session--available-agents) :test #'equal))
  (opencode-session--render-header))

(defun opencode-session--set-agent (agent _index)
  "Switch the current session to AGENT.
The session keeps the agent for subsequent turns."
  (let ((buffer (current-buffer)))
    (opencode-client-session-switch-agent
     opencode-session--connection
     (opencode-session--session-id)
     agent
     :success (lambda (&rest _args)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (opencode-session--set-agent-from-server agent)))
                (message "OpenCode agent: %s" agent))
     :error (lambda (&rest args)
              (message "OpenCode: failed to switch agent%s"
                       (if-let* ((detail (opencode-client-format-error args)))
                           (format " (%s)" detail)
                         ""))))))

(defun opencode-session-select-agent ()
  "Select an agent for the current session buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (opencode-session--ensure-agents connection)
           (let ((agents (opencode-session--available-agents)))
             (unless agents
               (error "OpenCode agents not available"))
             (let* ((agent (completing-read "OpenCode agent: " agents nil t
                                            (or opencode-session--agent (car agents))))
                    (index (cl-position agent agents :test #'string=)))
               (if (and index agents)
                   (opencode-session--set-agent agent index)
                 (message "OpenCode: unknown agent %s" agent))))))))))

(defun opencode-session--cycle-agent (step)
  "Cycle the current agent by STEP positions."
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (opencode-session--ensure-agents connection)
           (let ((agents (opencode-session--available-agents)))
             (unless agents
               (error "OpenCode agents not available"))
             (let* ((count (length agents))
                    (current (or opencode-session--agent-index 0))
                    (next (mod (+ current step) count)))
               ;; Update locally first so rapid cycling advances from the
               ;; newest choice instead of waiting for each reply.
               (setq-local opencode-session--agent-index next)
               (opencode-session--set-agent (nth next agents) next)))))))))

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
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (opencode-session--refresh-agents connection)))))))

;;; Provider and model selection

(defun opencode-session--ensure-providers (connection)
  "Ensure the model catalog is available for CONNECTION."
  (when (and connection (null (opencode-connection-models-state (opencode-connection-context connection))))
    (opencode-connection-ensure-models
     connection
     (lambda (_models)
       (opencode-session--refresh-headers connection)))))

(defun opencode-session--model-list (&optional connection)
  "Return the cached model list for CONNECTION, or nil while loading."
  (let ((conn (or connection opencode-session--connection)))
    (and conn (opencode-connection-models (opencode-connection-context conn)))))

(defun opencode-session--provider-model-info (provider-id model-id &optional connection)
  "Return model metadata for PROVIDER-ID and MODEL-ID from CONNECTION."
  (cl-find-if (lambda (model)
                (and (equal (alist-get 'providerID model) provider-id)
                     (equal (alist-get 'id model) model-id)))
              (opencode-session--model-list connection)))

(defun opencode-session--integration-connected-p (integration)
  "Return non-nil when INTEGRATION has a usable connection."
  (and (opencode-session--normalize-items (alist-get 'connections integration)) t))

(defun opencode-session--provider-model-candidate-display (provider-id model-id connected-p)
  "Return completion display text for PROVIDER-ID and MODEL-ID.

CONNECTED-P is nil for providers that must be connected before their
models can be listed; those show MODEL-ID as a connect action."
  (if connected-p
      (format "%s/%s" provider-id model-id)
    (format "%s/%s (connect)" provider-id model-id)))

(defun opencode-session--provider-model-candidates (&optional connection)
  "Return provider/model completion candidates for CONNECTION.

Each candidate is a plist with provider/model IDs and display text.
Integrations with no connection are listed once so they can be connected."
  (let* ((conn (or connection opencode-session--connection))
         (integrations (and conn (opencode-connection-integrations (opencode-connection-context conn))))
         (names (make-hash-table :test 'equal))
         entries)
    (dolist (integration integrations)
      (puthash (alist-get 'id integration) (alist-get 'name integration) names))
    (dolist (model (opencode-session--model-list conn))
      (let ((provider-id (alist-get 'providerID model))
            (model-id (alist-get 'id model)))
        (when (and (stringp provider-id)
                   (stringp model-id)
                   (not (eq (alist-get 'enabled model :missing) nil))
                   (not (equal (alist-get 'status model) "deprecated")))
          (push (list :provider-id provider-id
                      :provider-name (or (gethash provider-id names) provider-id)
                      :model-id model-id
                      :model-name (or (alist-get 'name model) model-id)
                      :connected-p t
                      :display (opencode-session--provider-model-candidate-display
                                provider-id model-id t))
                entries))))
    (dolist (integration integrations)
      (let ((id (alist-get 'id integration)))
        (unless (or (opencode-session--integration-connected-p integration)
                    (null (opencode-session--usable-auth-methods integration)))
          (push (list :provider-id id
                      :provider-name (or (alist-get 'name integration) id)
                      :model-id "*"
                      :model-name ""
                      :connected-p nil
                      :display (opencode-session--provider-model-candidate-display id "*" nil))
                entries))))
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

(defun opencode-session--provider-model-completion-data (&optional connection provider-id)
  "Return provider/model completion data for CONNECTION.

When PROVIDER-ID is non-nil, offer only that provider's models.  The
return value is a cons of (CHOICES . LOOKUP)."
  (let ((lookup (make-hash-table :test #'equal))
        choices)
    (dolist (candidate (opencode-session--provider-model-candidates connection))
      (let ((display (plist-get candidate :display)))
        (when (and (stringp display)
                   (or (null provider-id)
                       (and (plist-get candidate :connected-p)
                            (equal (plist-get candidate :provider-id) provider-id)))
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
            (opencode-session--sync-variant-index)
            (opencode-session--render-header)))))
   opencode-session--buffers))

(defun opencode-session--model-ref (provider-id model-id &optional variant)
  "Return a server model reference for PROVIDER-ID, MODEL-ID, and VARIANT."
  (append `((providerID . ,provider-id) (id . ,model-id))
          (when variant `((variant . ,variant)))))

(defun opencode-session--set-model-from-server (model)
  "Record server model reference MODEL as the session's model."
  (setq-local opencode-session--provider-id (alist-get 'providerID model))
  (setq-local opencode-session--model-id (alist-get 'id model))
  (setq-local opencode-session--variant (alist-get 'variant model))
  (opencode-session--sync-variant-index)
  (opencode-session--render-header))

(defun opencode-session--switch-model (provider-id model-id variant &optional on-success)
  "Switch the session to PROVIDER-ID/MODEL-ID with VARIANT.
ON-SUCCESS is called after the server accepts the switch."
  (let ((buffer (current-buffer))
        (model (opencode-session--model-ref provider-id model-id variant)))
    (opencode-client-session-switch-model
     opencode-session--connection
     (opencode-session--session-id)
     model
     :success (lambda (&rest _args)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (opencode-session--set-model-from-server model)
                    (when on-success
                      (funcall on-success)))))
     :error (lambda (&rest args)
              (message "OpenCode: failed to switch model%s"
                       (if-let* ((detail (opencode-client-format-error args)))
                           (format " (%s)" detail)
                         ""))))))

(defun opencode-session--variant-for-model (provider-id model-id)
  "Return the variant to keep when switching to PROVIDER-ID/MODEL-ID.
The current variant carries over when the new model offers it; otherwise
`opencode-session-default-variant' applies when available."
  (let ((variants (opencode-session--variant-keys
                   (alist-get 'variants (opencode-session--provider-model-info
                                         provider-id model-id)))))
    (cond
     ((member opencode-session--variant variants) opencode-session--variant)
     ((member opencode-session-default-variant variants) opencode-session-default-variant))))

(defun opencode-session--apply-model-selection (provider-id model-id)
  "Apply PROVIDER-ID and MODEL-ID as the session model.
Update recent models list, buffer state, and header."
  (let ((key (cons provider-id model-id)))
    (setq opencode-session--recent-models
          (cons key (cl-remove key opencode-session--recent-models
                               :test #'equal))))
  (opencode-session--switch-model
   provider-id model-id
   (opencode-session--variant-for-model provider-id model-id)
   (lambda ()
     (message "OpenCode model: %s/%s" provider-id model-id))))

(defun opencode-session--read-model (prompt &optional provider-id)
  "Read a model candidate with PROMPT, limited to PROVIDER-ID when non-nil."
  (let ((data (opencode-session--provider-model-completion-data nil provider-id)))
    (unless (car data)
      (error "OpenCode models not available"))
    (let* ((completion-extra-properties
            '(:display-sort-function identity :cycle-sort-function identity))
           (selection (completing-read prompt (car data) nil t)))
      (or (gethash selection (cdr data))
          (error "OpenCode: unknown model selection")))))

(defun opencode-session-select-model ()
  "Select a provider and model for the current session buffer.
Choosing a provider that is not connected runs its sign-in flow first."
  (interactive)
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (opencode-connection-ensure-models
            connection
            (lambda (_models)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (let* ((candidate (opencode-session--read-model "OpenCode model: "))
                         (provider-id (plist-get candidate :provider-id)))
                    (if (plist-get candidate :connected-p)
                        (opencode-session--apply-model-selection
                         provider-id (plist-get candidate :model-id))
                      (opencode-session--connect-provider
                       provider-id
                       (lambda ()
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (let ((chosen (opencode-session--read-model
                                            (format "OpenCode %s model: " provider-id)
                                            provider-id)))
                               (opencode-session--apply-model-selection
                                provider-id (plist-get chosen :model-id)))))))))))))))))))

(defun opencode-session-connect-provider ()
  "Connect a provider integration, then choose one of its models."
  (interactive)
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (opencode-connection-ensure-models
            connection
            (lambda (_models)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (let* ((integrations
                          (cl-remove-if-not #'opencode-session--usable-auth-methods
                                            (opencode-connection-integrations (opencode-connection-context connection))))
                         (ids (mapcar (lambda (integration) (alist-get 'id integration))
                                      integrations))
                         (provider-id (completing-read "OpenCode provider: " ids nil t)))
                    (opencode-session--connect-provider
                     provider-id
                     (lambda ()
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (let ((chosen (opencode-session--read-model
                                          (format "OpenCode %s model: " provider-id)
                                          provider-id)))
                             (opencode-session--apply-model-selection
                              provider-id (plist-get chosen :model-id))))))))))))))))))

(defun opencode-session--refresh-providers (connection &optional on-success)
  "Force refresh the model catalog for CONNECTION.

ON-SUCCESS is called with the models when they are loaded."
  (opencode-connection-invalidate-models connection)
  (opencode-connection-ensure-models
   connection
   (lambda (models)
     (opencode-session--refresh-headers connection)
     (when on-success
       (funcall on-success models)))))

;;; Variant selection

(defun opencode-session--variant-keys (variants)
  "Return variant names from model VARIANTS metadata, in server order."
  (delete-dups
   (delq nil (mapcar (lambda (variant)
                       (let ((id (alist-get 'id variant)))
                         (and (stringp id) id)))
                     (opencode-session--normalize-items variants)))))

(defun opencode-session--available-variants ()
  "Return available variant names for the active model."
  (when-let* ((model (opencode-session--active-model))
              (model-info (opencode-session--provider-model-info
                           (car model)
                           (cdr model)
                           opencode-session--connection)))
    (opencode-session--variant-keys (alist-get 'variants model-info))))

(defun opencode-session--sync-variant-index ()
  "Sync the selected variant index with the active model's variants."
  (setq-local opencode-session--variant-index
              (and opencode-session--variant
                   (cl-position opencode-session--variant
                                (opencode-session--available-variants)
                                :test #'string=))))

(defun opencode-session--set-variant (variant _index)
  "Switch the session's model to VARIANT, or to no variant when nil."
  (let ((model (opencode-session--active-model)))
    (unless model
      (error "Select a model first"))
    (opencode-session--switch-model
     (car model) (cdr model) variant
     (lambda ()
       (message "OpenCode variant: %s" (or variant "default"))))))

(defconst opencode-session--no-variant-label "(default)"
  "Completion label representing no active model variant.")

(defun opencode-session-clear-variant ()
  "Clear the active model variant for the current session buffer."
  (interactive)
  (unless (derived-mode-p 'opencode-session-mode)
    (error "Not in an OpenCode session buffer"))
  (opencode-session--set-variant nil nil))

(defun opencode-session-select-variant ()
  "Select a model variant for the current session buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (opencode-session--ensure-providers connection)
           (unless (opencode-session--active-model)
             (error "Select a model first"))
           (let* ((variants (or (opencode-session--available-variants) nil))
                  (choices (cons opencode-session--no-variant-label variants))
                  (initial (or opencode-session--variant
                               opencode-session-default-variant
                               opencode-session--no-variant-label))
                  (variant (completing-read "OpenCode variant: " choices nil t nil nil initial)))
             (if (or (null variant)
                     (string= variant opencode-session--no-variant-label))
                 (opencode-session-clear-variant)
               (let* ((available (opencode-session--available-variants))
                      (index (and available
                                  (cl-position variant available :test #'string=))))
                 (if (and available index)
                     (opencode-session--set-variant variant index)
                   (message "OpenCode: unknown variant %s" variant)))))))))))

(defun opencode-session--cycle-variant (step)
  "Cycle the current model variant by STEP positions."
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (opencode-session--ensure-providers connection)
           (unless (opencode-session--active-model)
             (error "Select a model first"))
           (let* ((variants (or (opencode-session--available-variants) nil))
                  (cycle-values (cons nil variants))
                  (count (length cycle-values))
                  (current (or (and opencode-session--variant
                                    (let ((index (cl-position opencode-session--variant variants
                                                              :test #'string=)))
                                      (and index (1+ index))))
                               0))
                  (next (mod (+ current step) count))
                  (next-variant (nth next cycle-values)))
             (opencode-session--set-variant next-variant (and next-variant (1- next))))))))))

(defun opencode-session-next-variant ()
  "Select the next variant (including none) for the current model."
  (interactive)
  (opencode-session--cycle-variant 1))

(defun opencode-session-previous-variant ()
  "Select the previous variant (including none) for the current model."
  (interactive)
  (opencode-session--cycle-variant -1))

;;; Auth flows

(defun opencode-session--usable-auth-methods (integration)
  "Return the INTEGRATION sign-in methods Emacs can run.
Only key and OAuth methods are supported; environment-variable methods need
no interaction."
  (cl-remove-if-not (lambda (method) (member (alist-get 'type method) '("key" "oauth")))
                (opencode-session--normalize-items (alist-get 'methods integration))))

(defun opencode-session--auth-method-label (method)
  "Return a completion label for auth METHOD."
  (or (alist-get 'label method)
      (pcase (alist-get 'type method)
        ("key" "API key")
        (type type))))

(defun opencode-session--find-integration (connection integration-id)
  "Return the cached integration INTEGRATION-ID on CONNECTION."
  (cl-find integration-id (opencode-connection-integrations (opencode-connection-context connection))
           :key (lambda (integration) (alist-get 'id integration))
           :test #'equal))

(defun opencode-session--connect-provider (provider-id callback)
  "Run the sign-in flow for PROVIDER-ID, then call CALLBACK on success."
  (let* ((connection opencode-session--connection)
         (integration (and connection
                           (opencode-session--find-integration connection provider-id)))
         (methods (and integration (opencode-session--usable-auth-methods integration))))
    (unless connection
      (error "OpenCode session is not connected"))
    (unless methods
      (error "OpenCode: %s has no interactive sign-in method" provider-id))
    (opencode-session--run-auth-flow
     connection provider-id
     (if (= (length methods) 1)
         (car methods)
       (opencode-session--choose-auth-method methods))
     callback)))

(defun opencode-session--choose-auth-method (methods)
  "Prompt the user to choose from METHODS."
  (let* ((labels (mapcar #'opencode-session--auth-method-label methods))
         (completion-extra-properties
          '(:display-sort-function identity :cycle-sort-function identity))
         (selection (completing-read "OpenCode auth method: " labels nil t)))
    (nth (cl-position selection labels :test #'string=) methods)))

(defun opencode-session--auth-form-answer (method)
  "Return answers to METHOD's form, or nil when it has none."
  (when-let* ((fields (alist-get 'form method)))
    (opencode-session--read-form-answer fields)))

(defun opencode-session--run-auth-flow (connection provider-id method callback)
  "Sign in to PROVIDER-ID on CONNECTION using METHOD, then call CALLBACK."
  (pcase (alist-get 'type method)
    ("key" (opencode-session--auth-api-key connection provider-id method callback))
    ("oauth" (opencode-session--auth-oauth connection provider-id method callback))
    (type (error "OpenCode: unsupported auth method type %s" type))))

(defun opencode-session--auth-connected (connection provider-id callback)
  "Refresh CONNECTION's models after PROVIDER-ID connects, then call CALLBACK."
  (message "OpenCode: %s connected" provider-id)
  (opencode-session--refresh-providers connection (lambda (_models) (funcall callback))))

(defun opencode-session--auth-api-key (connection provider-id method callback)
  "Store an API key for PROVIDER-ID on CONNECTION, then call CALLBACK.
METHOD is the key method, whose form is answered first when it has one."
  (let* ((answer (opencode-session--auth-form-answer method))
         (key (read-passwd (format "API key for %s: " provider-id))))
    (when (string-empty-p key)
      (error "OpenCode: API key cannot be empty"))
    (message "OpenCode: setting API key for %s..." provider-id)
    (opencode-client-integration-connect-key
     connection provider-id key
     :answer answer
     :success (lambda (&rest _args)
                (opencode-session--auth-connected connection provider-id callback))
     :error (lambda (&rest args)
              (message "OpenCode: failed to set API key for %s%s" provider-id
                       (if-let* ((detail (opencode-client-format-error args)))
                           (format " (%s)" detail)
                         ""))))))

(defun opencode-session--auth-oauth (connection provider-id method callback)
  "Run OAuth METHOD for PROVIDER-ID on CONNECTION, then call CALLBACK."
  (let ((answer (opencode-session--auth-form-answer method)))
    (message "OpenCode: starting sign-in for %s..." provider-id)
    (opencode-client-integration-oauth-connect
     connection provider-id (alist-get 'id method)
     :answer answer
     :success (lambda (&rest args)
                (let* ((attempt (plist-get args :data))
                       (attempt-id (alist-get 'attemptID attempt))
                       (url (alist-get 'url attempt))
                       (instructions (alist-get 'instructions attempt)))
                  (when (and url (not (string-empty-p url)))
                    (let ((browse-url-browser-function #'browse-url-default-browser))
                      (browse-url url)))
                  (when (and instructions (not (string-empty-p instructions)))
                    (message "OpenCode: %s" instructions))
                  (if (equal (alist-get 'mode attempt) "code")
                      (opencode-session--auth-oauth-code
                       connection provider-id attempt-id callback)
                    (opencode-session--auth-oauth-poll
                     connection provider-id attempt-id callback))))
     :error (lambda (&rest _args)
              (message "OpenCode: sign-in failed for %s" provider-id)))))

(defun opencode-session--auth-oauth-code (connection provider-id attempt-id callback)
  "Finish code-based OAuth ATTEMPT-ID for PROVIDER-ID on CONNECTION.
CALLBACK is called on success."
  (let ((code (read-string (format "Authorization code for %s: " provider-id))))
    (when (string-empty-p code)
      (error "OpenCode: authorization code cannot be empty"))
    (message "OpenCode: completing sign-in for %s..." provider-id)
    (opencode-client-integration-oauth-complete
     connection provider-id attempt-id
     :code code
     :success (lambda (&rest _args)
                (opencode-session--auth-connected connection provider-id callback))
     :error (lambda (&rest _args)
              (message "OpenCode: sign-in failed for %s" provider-id)))))

(defun opencode-session--auth-oauth-poll (connection provider-id attempt-id callback)
  "Wait for browser OAuth ATTEMPT-ID for PROVIDER-ID on CONNECTION.
CALLBACK is called once the attempt completes."
  (message "OpenCode: waiting for browser sign-in for %s..." provider-id)
  (let ((directory default-directory))
    (opencode-client-integration-oauth-status
     connection provider-id attempt-id
     :success (lambda (&rest args)
                (let ((status (plist-get args :data)))
                  (pcase (alist-get 'status status)
                    ("complete"
                     (opencode-session--auth-connected connection provider-id callback))
                    ("pending"
                     (run-at-time
                      opencode-session-oauth-poll-interval nil
                      (lambda ()
                        (let ((default-directory directory))
                          (opencode-session--auth-oauth-poll
                           connection provider-id attempt-id callback)))))
                    (_
                     (message "OpenCode: sign-in for %s %s%s" provider-id
                              (or (alist-get 'status status) "failed")
                              (if-let* ((text (alist-get 'message status)))
                                  (format ": %s" text)
                                ""))))))
     :error (lambda (&rest _args)
              (message "OpenCode: sign-in failed for %s" provider-id)))))

(provide 'emacs-opencode-session-model)

;;; emacs-opencode-session-model.el ends here
