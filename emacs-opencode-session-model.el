;;; emacs-opencode-session-model.el --- Agent, model, and variant selection  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-opencode-session-vars)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-client)
(declare-function opencode-session--render-header "emacs-opencode-session-header")
(declare-function opencode-session--active-model "emacs-opencode-session-header")
(declare-function opencode-session--session-used-models "emacs-opencode-session-header")
(declare-function opencode-session--maybe-start-spinner "emacs-opencode-session-header")
(declare-function opencode-session--maybe-stop-spinner "emacs-opencode-session-header")
(declare-function opencode-session--ensure-connection "emacs-opencode-session-mode" (callback))

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

(defvar opencode-session--recent-models nil
  "Global list of recently selected (PROVIDER-ID . MODEL-ID) pairs.
Most recently selected first.")

(defvar-local opencode-session--agent-index nil
  "Index of the selected agent in the available agents list.")

(defvar-local opencode-session--variant-index nil
  "Index of the selected variant in the available variants list.")

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
  "Normalize agent list DATA into a list of primary agent names."
  (let* ((agents (opencode-session--normalize-agent-data data))
         (primary (cl-remove-if-not (lambda (agent)
                                      (or (stringp agent)
                                          (and (string= (alist-get 'mode agent) "primary")
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

(defun opencode-session--maybe-fetch-agents (connection)
  "Fetch and cache agents for CONNECTION when needed."
  (unless (opencode-connection-agents connection)
    (let ((session-buffer (current-buffer)))
      (opencode-client-agents
       connection
       :success (lambda (&rest args)
                  (let* ((data (plist-get args :data))
                         (raw (opencode-session--normalize-agent-data data))
                         (agents (opencode-session--normalize-agents data)))
                    (setf (opencode-connection-agents connection) agents)
                    (setf (opencode-connection-agents-raw connection) raw)
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

(defun opencode-session--refresh-agents (connection)
  "Refresh the cached agent list for CONNECTION."
  (setf (opencode-connection-agents connection) nil)
  (setf (opencode-connection-agents-raw connection) nil)
  (opencode-session--maybe-fetch-agents connection))

(defun opencode-session--available-agents ()
  "Return available agents for the current session buffer."
  (when opencode-session--connection
    (opencode-connection-agents opencode-session--connection)))

(defun opencode-session--available-completable-agents ()
  "Return agent names available for @-mention completion.
These are non-hidden, non-primary agents (subagents)."
  (when opencode-session--connection
    (let ((raw (opencode-connection-agents-raw opencode-session--connection)))
      (when raw
        (opencode-session--completable-agent-names raw)))))

(defun opencode-session--set-agent (agent index)
  "Set the current session agent to AGENT at INDEX."
  (setq-local opencode-session--agent agent)
  (setq-local opencode-session--agent-index index)
  (opencode-session--render-header)
  (message "OpenCode agent: %s" agent))

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

(defun opencode-session--provider-model-info (provider-id model-id &optional connection)
  "Return model metadata for PROVIDER-ID and MODEL-ID from CONNECTION."
  (let* ((conn (or connection opencode-session--connection))
         (catalog (opencode-session--provider-catalog conn))
         (providers (or (opencode-session--normalize-items (and catalog (alist-get 'all catalog)))
                        (opencode-session--normalize-items (and conn (opencode-connection-providers conn)))))
         (provider (and (stringp provider-id)
                        (cl-find provider-id providers
                                 :key (lambda (item) (alist-get 'id item))
                                 :test #'string=))))
    (when provider
      (cdr (cl-assoc model-id (opencode-session--provider-model-items provider)
                     :test #'string=)))))

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
            (opencode-session--sync-variant-selection)
            (opencode-session--render-header)))))
   opencode-session--buffers))

(defun opencode-session--apply-model-selection (provider-id model-id)
  "Apply PROVIDER-ID and MODEL-ID as the active model.
Update recent models list, buffer state, and header."
  (let ((key (cons provider-id model-id)))
    (setq opencode-session--recent-models
          (cons key (cl-remove key opencode-session--recent-models
                               :test #'equal))))
  (setq-local opencode-session--provider-id provider-id)
  (setq-local opencode-session--model-id model-id)
  (opencode-session--sync-variant-selection)
  (opencode-session--render-header)
  (message "OpenCode model: %s/%s" provider-id model-id))

(defun opencode-session-select-model ()
  "Select a provider and model for the current session buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (opencode-session--ensure-providers connection)
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
                    (opencode-session--connect-provider
                     provider-id
                     (lambda (&rest _ignored)
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (opencode-session--apply-model-selection
                            provider-id model-id)))))))))))))))



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

;;; Variant selection

(defun opencode-session--variant-keys (variants)
  "Return variant names from VARIANTS metadata."
  (let (keys)
    (cond
     ((hash-table-p variants)
      (maphash
       (lambda (key value)
         (let ((name (cond
                      ((stringp key) key)
                      ((symbolp key) (symbol-name key))
                      (t nil))))
           (unless (or (null name)
                       (and (listp value)
                            (eq (alist-get 'disabled value) t)))
             (push name keys))))
       variants))
     ((listp variants)
      (dolist (entry variants)
        (when (consp entry)
          (let* ((key (car entry))
                 (value (cdr entry))
                 (name (cond
                        ((stringp key) key)
                        ((symbolp key) (symbol-name key))
                        (t nil))))
            (unless (or (null name)
                        (and (listp value)
                             (eq (alist-get 'disabled value) t)))
              (push name keys)))))))
    (sort (delete-dups keys) #'string-lessp)))

(defun opencode-session--available-variants ()
  "Return available variant names for the active model."
  (when-let* ((model (opencode-session--active-model))
              (model-info (opencode-session--provider-model-info
                           (car model)
                           (cdr model)
                           opencode-session--connection)))
    (opencode-session--variant-keys (alist-get 'variants model-info))))

(defun opencode-session--sync-variant-selection ()
  "Sync selected variant with available variants for the active model."
  (let* ((variants (opencode-session--available-variants))
         (current opencode-session--variant)
         (current-index (and current
                             variants
                             (cl-position current variants :test #'string=))))
    (cond
     (current-index
      (setq-local opencode-session--variant-index current-index))
     ((and (stringp opencode-session-default-variant)
           variants
           (member opencode-session-default-variant variants))
      (setq-local opencode-session--variant opencode-session-default-variant)
      (setq-local opencode-session--variant-index
                  (cl-position opencode-session-default-variant variants :test #'string=)))
     (t
      (setq-local opencode-session--variant nil)
      (setq-local opencode-session--variant-index nil)))))

(defun opencode-session--set-variant (variant index)
  "Set model VARIANT at INDEX for the current session buffer."
  (setq-local opencode-session--variant variant)
  (setq-local opencode-session--variant-index (and variant index))
  (opencode-session--render-header)
  (message "OpenCode variant: %s" (or variant "none")))

(defconst opencode-session--no-variant-label "none"
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
             (if next-variant
                 (opencode-session--set-variant next-variant (1- next))
               (opencode-session-clear-variant)))))))))

(defun opencode-session-next-variant ()
  "Select the next variant (including none) for the current model."
  (interactive)
  (opencode-session--cycle-variant 1))

(defun opencode-session-previous-variant ()
  "Select the previous variant (including none) for the current model."
  (interactive)
  (opencode-session--cycle-variant -1))

;;; Auth flows

(defun opencode-session--post-auth-refresh (connection callback)
  "Dispose instance state for CONNECTION, then refresh providers.

CALLBACK is passed through to `opencode-session--refresh-providers'."
  (opencode-client-instance-dispose
   connection
   :success (lambda (&rest _args)
              (opencode-session--refresh-providers connection callback))
   :error (lambda (&rest _args)
            (message "OpenCode: failed to dispose instance; refreshing providers")
            (opencode-session--refresh-providers connection callback))))

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

(provide 'emacs-opencode-session-model)

;;; emacs-opencode-session-model.el ends here
