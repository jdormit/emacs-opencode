# OpenCode V2 Migration — Notes on what has been done

Status of the native V2 migration of this package (targets OpenCode V2 only;
V1 and V2 are not installed side-by-side). The migration is complete through
Phase 8; all phases are listed under Completed below.

## Steps and things that changed

- **Phase 1 — Connect, authenticate, stream URL.** Password auth
  (`OPENCODE_PASSWORD` + generated/configured `opencode-server-password`),
  readiness match on `server listening on`, health via `/api/info`, SSE via
  `/api/event`.
- **Phase 2 — Endpoint paths, directory scoping, envelopes.** All paths under
  `/api`, `location[directory]` deepObject / `?directory=` scoping, `{data: …}`
  unwrapping, fork/rename/compact/interrupt path + body changes, import
  `x-opencode-directory` header removed.
- **Phase 3 — Prompt / agent / model.** Session create with `{agent, model}`,
  agent/model pushed via `/api/session/{id}/agent` and `/api/session/{id}/model`,
  prompt body `{text, files, agents}` (no `parts`/`model`/`variant`), command
  `{name,text}`, shell `{command}`.
- **Phase 4 — Streaming handlers.** V2 event vocabulary rewritten into the
  existing message/part structs: `session.created|updated|deleted`,
  `session.step.*`, `session.execution.*`, `session.idle`, `session.text.*`,
  `session.tool.*`, `session.inbox.enqueued|delivered`, compaction,
  `permission.asked|replied`, `form.created|replied|cancelled`, `file.edited`.
  Bridge envelope updated to `data` (not `properties`).
- **Phase 5 — History hydration.** `opencode-client-session-messages` via
  `/api/session/{id}/message?order=asc`, V2 message/part mapping (`type`→role,
  `model.{id,providerID,variant}`, `content` parts, tool `state.content`→output,
  `idle`/`synthetic`/`compaction` types, streaming-part association by
  `assistantMessageID` + `ordinal`).
- **Phase 6 — Permission & forms.** Permission replies via
  `/api/session/{id}/permission/{requestID}/reply`; forms replace V1 questions
  (create/reply/cancel handlers, minibuffer prompts with validation, option
  handling, `{answer:{}}` empty-reply encoding, case-sensitive pattern
  matching, string-key answer lookups). Full ERT suite green (521 tests),
  byte-compile clean apart from pre-existing warnings, no new checkdoc warnings.
- **Phase 7 — Provider/model catalog + auth.** Providers, models and
  integrations are now three independent caches:
  - `opencode-connection-ensure-providers` stores the bare V2 list (no
    `(alist-get 'all data)`), and new `ensure-models` / `ensure-integrations`
    mirror it. The connection struct swaps `provider-catalog` for `models` +
    `integrations`.
  - The session completion UI reads the flat `/api/model` list
    (`opencode-session--provider-model-candidates` / `--provider-model-info`),
    keying candidates on the catalog `id` (the session's `ModelRef.id`), and
    skips `status: "deprecated"` entries. `--variant-keys` understands both V2
    variant objects (`{id, settings}`) and the legacy pair shape.
  - Connected state derives from integration `connections`;
    `--integration-id-for-provider` maps a provider to its `integrationID`.
  - Auth dispatch is on the V2 method `type` (`key` / `oauth` / `env`):
    `/api/integration/{id}/connect/key` for keys,
    `/api/integration/{id}/connect/oauth` for OAuth (browse URL + poll
    `/api/integration/attempt/{attemptID}` via `run-at-time`, or prompt for the
    code and POST `.../complete`). The V1 `/provider/auth`,
    `/provider/{id}/oauth/*`, and `PUT /auth/{id}` paths are gone.
  - `/instance/dispose` dropped — V2 has no equivalent; post-auth just restarts
    SSE and refreshes the caches.
  - The `declare-function` `&key` arg-count warnings in
    `emacs-opencode-connection.el` are fixed by declaring `(conn &rest args)`.
  - Full ERT suite green (550 tests), byte-compile unchanged apart from the two
    pre-existing warnings, no new checkdoc warnings.
- **Phase 8 — Tests & docs.** Final cleanup of V1 leftovers:
  - Tests: updated stale V1-shaped fixtures to the V2 `data` envelope and V2
    event names (`tests/emacs-opencode-sse-test.el`), and rewrote the
    long-broken JS bridge test to V2 event shapes
    (`tests/opencode-sse-bridge.test.js`, now 10/10 under both node and bun).
  - Docs: aligned `README.org` (V2 target note, `opencode-server-password`,
    shell-prompt wording), `ARCHITECTURE.org` (V2 `/api` endpoints and the
    `data` event envelope), and `TODO.org` (V2 migration link, V1 API
    references removed) with the V2 API and event vocabulary.
  - Audited the dead V1 question machinery: the live v2.0.26 server exposes
    `/api/form/...` and no question endpoints, so V2 asks questions with forms.
    Removed the obsolete code — the `opencode-session--question-*` helpers,
    `opencode-session--prompt-question`, `opencode--vectorize-answers`, the
    `opencode-client-question-reply`/`-reject` methods, and their tests.
  - Full ERT suite green (539 tests), byte-compile unchanged apart from the two
    pre-existing warnings, no new checkdoc warnings.

## Design notes carried over

- **Native V2, no adapter:** handlers map V2 events straight into the existing
  struct builders rather than faking V1 envelopes — the V2 model is
  event-sourced, so a compatibility layer would be lossy.
- **V2 only:** no V1 compatibility paths are kept.
- **Questions are forms in V2:** the live v2.0.26 server exposes
  `/api/session/{id}/form` (with `form.created|replied|cancelled` events) and no
  question endpoints, so V2 asks the user with forms.
