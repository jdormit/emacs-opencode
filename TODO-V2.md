# OpenCode V2 Migration — Remaining Phases

Status of the native V2 migration of this package (targets OpenCode V2 only;
V1 and V2 are not installed side-by-side). The core migration is done and
staged; the provider/model catalog, auth, and final tests/docs cleanup remain.

## Completed

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

## Remaining

### Phase 7 — Provider/model catalog + auth

- **Provider response shape.** `opencode-connection-ensure-providers` still
  reads the V1 `(alist-get 'all data)` shape; V2 returns `{data: […], location}`
  (bare list after unwrapping). Repoint the cache at the V2 shape.
  The pre-existing compile warnings (`opencode-client-commands` /
  `opencode-client-providers` "called with 5 arguments, but accepts only 4" in
  `emacs-opencode-connection.el`) are `cl-defmethod` `&key` arg-count
  mismatches at these call sites; worth revisiting while touching the cache.
- **Model catalog.** `/api/model` (models shape with `id`, `providerID`,
  `variant`); provider-level model lists if `/api/provider` embeds them.
  Session model selection already round-trips server-side from Phase 3; only the
  catalog/source that feeds the completion UI needs the V2 shape.
- **Auth via credentials/integrations.** Replace the remaining V1 endpoints:
  - `/provider/auth` → auth methods via `/api/credential` + `/api/integration`.
  - `/provider/{id}/oauth/authorize` and `/provider/{id}/oauth/callback` →
    integration/credential equivalents.
  - `/auth/{id}` (PUT auth-set) → credential creation.
- **`/instance/dispose`** — no public V2 equivalent confirmed; either drop the
  command or wire it to whatever the V2 server offers for instance teardown.

### Phase 8 — Tests & docs

- **Tests.** Any remaining ERT assertions still encoding V1 response shapes
  (provider `all`/`default`/`connected`, question payloads, V1 message
  envelopes); add coverage for the Phase 7 provider/model catalog shape.
- **Docs.** Audit `README.org` and `ARCHITECTURE.org` for anything still worded
  around V1 concepts (e.g., the "shell prompt endpoint" phrasing, session
  hydration/streaming model descriptions, the `TODO.org` roadmap) and align
  them with the V2 API and event vocabulary. `TODO.org` may link here for the
  deferred items.

## Design notes carried over

- **Native V2, no adapter:** handlers map V2 events straight into the existing
  struct builders rather than faking V1 envelopes — the V2 model is
  event-sourced, so a compatibility layer would be lossy.
- **V2 only:** no V1 compatibility paths are kept.