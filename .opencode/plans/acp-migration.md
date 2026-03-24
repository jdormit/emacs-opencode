# ACP Migration Plan

Replace the SSE + HTTP architecture with ACP (Agent Client Protocol) over stdio,
using the HTTP server embedded in `opencode acp` only for lightweight polling.

## Background

The current architecture spawns `opencode serve` (HTTP server), connects to it
via HTTP for API calls, and subscribes to an SSE event stream via curl for
real-time updates. The SSE stream carries multi-MB `message.updated` payloads
that cause head-of-line blocking in Emacs.

The new architecture spawns `opencode acp` which provides JSON-RPC over stdio
(ndjson framing) for all core operations, plus an embedded HTTP server for the
few things that need polling. This eliminates SSE entirely and replaces the
problematic large events with small incremental notifications.

## Architecture Overview

```
┌──────────────────────────────────────────┐
│              Emacs Client                │
│                                          │
│  ┌──────────────┐  ┌─────────────────┐   │
│  │  ACP Module   │  │  HTTP Poller    │   │
│  │  (stdio)      │  │  (lightweight)  │   │
│  │               │  │                 │   │
│  │ - initialize  │  │ - GET /question │   │
│  │ - newSession  │  │   (while busy)  │   │
│  │ - loadSession │  │ - GET /session  │   │
│  │ - listSessions│  │   (on idle)     │   │
│  │ - prompt      │  │                 │   │
│  │ - cancel      │  │                 │   │
│  │ - setMode     │  │                 │   │
│  │ - setModel    │  │                 │   │
│  │ - forkSession │  │                 │   │
│  │               │  │                 │   │
│  │ Notifications:│  │                 │   │
│  │ - session/    │  │                 │   │
│  │   update      │  │                 │   │
│  │ - session/    │  │                 │   │
│  │   request_    │  │                 │   │
│  │   permission  │  │                 │   │
│  │ - fs/write_   │  │                 │   │
│  │   text_file   │  │                 │   │
│  └──────┬───────┘  └───────┬─────────┘   │
│         │ stdio             │ HTTP GET    │
└─────────┼──────────────────┼─────────────┘
          │                  │
          ▼                  ▼
┌──────────────────────────────────────────┐
│        opencode acp --port <N>           │
│                                          │
│  ┌──────────────┐   ┌────────────────┐   │
│  │  ACP Agent    │   │  HTTP Server   │   │
│  │  (JSON-RPC)   │◄─►│  (port N)      │   │
│  └──────────────┘   └────────────────┘   │
└──────────────────────────────────────────┘
```

## Wire Protocol

Newline-delimited JSON (ndjson) over stdin/stdout. Each message is a complete
JSON-RPC 2.0 object followed by `\n`.

### Client -> Agent (requests)

```json
{"jsonrpc":"2.0","id":0,"method":"initialize","params":{...}}
{"jsonrpc":"2.0","id":1,"method":"session/new","params":{...}}
{"jsonrpc":"2.0","id":2,"method":"session/prompt","params":{...}}
```

### Agent -> Client (responses)

```json
{"jsonrpc":"2.0","id":0,"result":{...}}
{"jsonrpc":"2.0","id":2,"result":{"stopReason":"end_turn","usage":{...}}}
```

### Agent -> Client (notifications — no id, no response)

```json
{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"...","update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"Hello"}}}}
```

### Agent -> Client (requests — have id, expect response)

```json
{"jsonrpc":"2.0","id":0,"method":"session/request_permission","params":{...}}
{"jsonrpc":"2.0","id":1,"method":"fs/write_text_file","params":{...}}
```

## Implementation Phases

### Phase 1: New ACP transport module (`emacs-opencode-acp.el`)

Create a new module that handles JSON-RPC communication over stdio.

#### 1a. Process management

- `opencode-acp-start (directory port ready-callback)` — spawns `opencode acp
  --port PORT --hostname 127.0.0.1` as a subprocess with `start-process`,
  setting `default-directory` to DIRECTORY
  - Process uses `:connection-type 'pipe` for stdin/stdout
  - stderr goes to a separate hidden buffer for diagnostics
  - PORT is determined by the caller (see Phase 2)
  - READY-CALLBACK is invoked once the ACP initialize handshake succeeds
- `opencode-acp-stop (process)` — sends EOF to stdin (close pipe), waits
  briefly, then kills process
- Sentinel handles unexpected exits and cleans up

#### 1b. NDJSON framing / process filter

- Process filter accumulates stdout bytes in a buffer
- On each `\n`, extracts a complete JSON line and dispatches
- Uses `json-parse-string` for parsing (fast, built-in since Emacs 27)
- Must handle partial lines (data arriving without trailing newline yet)

#### 1c. Request/response tracking

- Outgoing requests get auto-incrementing integer IDs
- `opencode-acp--pending-requests` — hash-table: id -> (success-callback .
  error-callback)
- `opencode-acp-request (process method params &key success error)` — serializes
  JSON-RPC request, writes to process stdin, registers callbacks
- When a response arrives (has `id`, no `method`): look up pending request,
  call success or error callback, remove from pending table
- Timeout handling: timer per request, calls error callback on expiry

#### 1d. Notification dispatch

- When a notification arrives (has `method`, no `id`): dispatch to registered
  handlers
- `opencode-acp--handlers` — alist: method-string -> list of handler functions
- `opencode-acp-define-handler (name method &rest body)` — macro analogous to
  current `opencode-sse-define-handler`
- Key notifications to handle:
  - `"session/update"` — the main one, demux on `update.sessionUpdate` variant
  - No other notifications exist in the current ACP spec

#### 1e. Agent-to-client requests

- When an agent request arrives (has both `method` and `id`): dispatch and
  send response
- `opencode-acp--agent-request-handlers` — alist: method-string -> handler fn
- Handlers return a result value (or signal an error) which gets wrapped in a
  JSON-RPC response and written back to the process
- Key agent requests:
  - `"session/request_permission"` — prompt user, return approval/denial
  - `"fs/write_text_file"` — write file to disk, revert Emacs buffer
  - `"fs/read_text_file"` — read file from disk, return contents
  - Terminal requests (`terminal/create`, `terminal/output`, etc.) —
    for now, return errors (not implemented). These are optional in the
    protocol and OpenCode doesn't seem to use them for the core flow.

### Phase 2: Connection lifecycle rewrite (`emacs-opencode-connection.el`)

#### 2a. Struct changes

Replace HTTP-centric fields with ACP-centric ones:

```elisp
;; REMOVE: hostname, port, base-url, username, password,
;;         sse-process, sse-state
;; ADD:    acp-process, http-port, http-base-url
;; KEEP:   directory, timeout, agents, agents-raw, commands,
;;         providers, provider-catalog, process (rename to acp-process)
```

The `http-port` and `http-base-url` fields are for the lightweight HTTP
poller (question polling, session metadata refresh).

#### 2b. Port selection

```elisp
(defun opencode-connection--find-free-port ()
  "Find a free TCP port by briefly binding to port 0."
  (let* ((server (make-network-process
                  :name "opencode-port-probe"
                  :server t
                  :host "127.0.0.1"
                  :service 0
                  :family 'ipv4))
         (port (process-contact server :service)))
    (delete-process server)
    port))
```

#### 2c. Startup sequence

1. Find a free port via `opencode-connection--find-free-port`
2. Spawn `opencode acp --port <port> --hostname 127.0.0.1` via
   `opencode-acp-start`
3. Wait for process stdout to be ready (first bytes indicate process is alive)
4. Send `initialize` JSON-RPC request:
   ```json
   {
     "protocolVersion": 1,
     "clientInfo": { "name": "emacs-opencode", "version": "0.1" },
     "clientCapabilities": {
       "fs": { "readTextFile": true, "writeTextFile": true }
     }
   }
   ```
5. On `initialize` response: validate `protocolVersion`, cache
   `agentCapabilities`, invoke ready callback
6. Verify HTTP server is up via `GET http://127.0.0.1:<port>/path` (retry
   with backoff, max ~2s)
7. Cache providers and commands (via HTTP `GET /provider`, `GET /command`
   against the embedded server — or we could fetch these through the
   server's HTTP endpoint since ACP doesn't expose dedicated provider/command
   listing RPCs)

#### 2d. Shutdown sequence

1. Close stdin pipe (EOF signals ACP process to shut down)
2. Stop any active polling timers
3. Wait briefly for process exit
4. Kill process if still alive
5. Clean up buffers

### Phase 3: Client module rewrite (`emacs-opencode-client.el`)

Replace HTTP API calls with ACP JSON-RPC calls. The module's public API
surface (function names and callback signatures) stays the same where
possible to minimize changes in callers.

#### Operations that become ACP JSON-RPC calls:

| Current function | HTTP | New transport | ACP method |
|---|---|---|---|
| `opencode-client-session-prompt-async` | `POST /session/:id/prompt_async` | ACP request | `session/prompt` |
| `opencode-client-session-abort` | `POST /session/:id/abort` | ACP notification | `session/cancel` |
| `opencode-client-sessions` | `GET /session` | ACP request | `session/list` |
| `opencode-client-permission-reply` | `POST /permission/:id/reply` | N/A (handled by ACP permission flow) | — |
| `opencode-client-session-command` | `POST /session/:id/command` | ACP request (via prompt with `/cmd` prefix) | `session/prompt` |

#### Operations that use the embedded HTTP server:

| Current function | HTTP endpoint | Why not ACP |
|---|---|---|
| `opencode-client-health` | `GET /global/health` | Health check for HTTP server readiness |
| `opencode-client-providers` | `GET /provider` | ACP has no provider listing RPC |
| `opencode-client-commands` | `GET /command` | ACP sends `available_commands_update` but only as notification |
| `opencode-client-agents` | `GET /agent` | ACP has mode listing but via session lifecycle |
| `opencode-client-session-messages` | `GET /session/:id/message` | ACP replays via `loadSession` but no raw message fetch |
| `opencode-client-question-reply` | `POST /question/:id/reply` | ACP doesn't handle questions |
| `opencode-client-question-reject` | `POST /question/:id/reject` | ACP doesn't handle questions |
| Auth endpoints (OAuth, API key) | Various `/provider/*`, `/auth/*` | ACP has `authenticate` but it's limited |

#### Operations that go away entirely:

| Current function | Why |
|---|---|
| `opencode-client-instance-dispose` | Not needed — stopping ACP process handles cleanup |

#### Prompt submission changes:

The big semantic change: `session/prompt` in ACP is a **long-lived request**
that blocks until the agent finishes. The current HTTP `prompt_async` is
fire-and-forget. This means:

- Send `session/prompt` request (gets an id, e.g. 5)
- While waiting for response id=5, process incoming notifications
  (`session/update`, `session/request_permission`, etc.)
- When response id=5 arrives: prompt is complete
  - `stopReason: "end_turn"` — normal completion
  - `stopReason: "cancelled"` — user cancelled
  - JSON-RPC error — something went wrong

The `:success` callback fires when the prompt response arrives. During
execution, streaming updates arrive as notifications handled by the
notification dispatch (Phase 1d).

#### Cancel changes:

`cancel` is a JSON-RPC **notification** (no id, no response). We send it
while the `prompt` request is still pending. The prompt response will
eventually arrive with `stopReason: "cancelled"`.

#### Shell commands (`!cmd`):

ACP doesn't have a dedicated shell command RPC. Options:

1. **Keep HTTP for shell**: Use `POST /session/:id/shell` against the
   embedded HTTP server. This preserves the exact current behavior.
2. **Send as prompt**: Translate `!cmd` to a regular ACP prompt like
   "Run the following command: `cmd`". Different behavior (goes through LLM).
3. **Run locally**: Execute the command in Emacs via `shell-command`, display
   output. Different behavior (doesn't add to session context).

**Recommendation**: Option 1 (keep HTTP for shell) for the initial migration.
The HTTP server is already available. This is the only write operation that
still needs HTTP.

### Phase 4: Session handler rewrite (`emacs-opencode-session-handlers.el`)

Replace SSE event handlers with ACP notification handlers. Most handler
*implementations* stay the same — they receive parsed JSON and update UI
state. The registration mechanism and event names change.

#### 4a. New ACP session update handlers

The `session/update` notification is demuxed on its `update.sessionUpdate`
field. Register one handler per variant:

| sessionUpdate variant | Handler action |
|---|---|
| `agent_message_chunk` | Append text delta to current assistant message part; render |
| `agent_thought_chunk` | Append to reasoning part (if showing reasoning) |
| `user_message_chunk` | Create/append user message part (during session replay) |
| `tool_call` | Create new tool part in current message; render |
| `tool_call_update` | Update tool part status/output; render |
| `plan` | Update plan display (if we implement plan UI) |
| `usage_update` | Update header line with context/cost info |
| `available_commands_update` | Cache available commands on connection |
| `session_info_update` | Update session title in header |
| `current_mode_update` | Update current mode/agent display |
| `config_option_update` | (ignore for now) |

#### 4b. Agent-to-client request handlers

| ACP request | Handler action |
|---|---|
| `session/request_permission` | Prompt user via `completing-read` (Allow once / Allow always / Deny). Return result. Replaces current SSE `permission.asked` handler. |
| `fs/write_text_file` | Write file contents to disk. Revert any Emacs buffer visiting the file. Return success. Replaces `file.edited` SSE handler. |
| `fs/read_text_file` | Read file from disk, return contents. |

**Key insight**: `fs/write_text_file` replaces both the `file.edited` SSE event
AND the `permission.asked` -> "apply edit" flow. When ACP sends a
`request_permission` for an edit and the user approves, ACP then sends
`fs/write_text_file` with the new file content. The client writes it to
disk and can revert the Emacs buffer at that point. No SSE needed.

#### 4c. Handlers that become unnecessary

| Former SSE event | Why not needed |
|---|---|
| `session.created` | Session creation is synchronous via ACP `newSession` RPC |
| `session.updated` | Replaced by `session_info_update` ACP notification + HTTP poll on idle |
| `session.status` | Implicit from ACP `prompt()` request lifecycle |
| `session.idle` | Implicit — `prompt()` response means idle |
| `session.error` | Surfaces as JSON-RPC error on `prompt()` response |
| `message.updated` | **Gone entirely.** ACP uses fine-grained part updates. |
| `message.part.updated` | Replaced by `tool_call` / `tool_call_update` ACP notifications |
| `message.part.delta` | Replaced by `agent_message_chunk` / `agent_thought_chunk` ACP notifications |
| `permission.asked` | Replaced by `session/request_permission` ACP agent request |
| `file.edited` | Replaced by `fs/write_text_file` ACP agent request |
| `file.watcher.updated` | Use Emacs-native `auto-revert-mode` instead |

#### 4d. Question polling

ACP doesn't handle `question.asked`. We poll:

- While a prompt is active (between sending `session/prompt` and receiving
  response), run a timer that calls `GET /question/` every 500ms-1s
- When a new question ID appears: prompt user via `completing-read`
- Send reply via `POST /question/:id/reply` or reject via
  `POST /question/:id/reject`
- Stop polling timer when prompt response arrives

### Phase 5: Session mode updates (`emacs-opencode-session-mode.el`)

#### 5a. Prompt submission

Replace `opencode-session--send-input`:

```
Current:  opencode-client-session-prompt-async -> POST /session/:id/prompt_async
                                                  (fire-and-forget)

New:      opencode-acp-request "session/prompt" params
            :success -> prompt complete, update status to idle
            :error   -> display error, update status to idle
          (during execution, notifications arrive via process filter)
```

The key difference: the current HTTP prompt is fire-and-forget (status updates
come via SSE). The ACP prompt request stays open until completion. We need to:

1. Mark session as "busy" immediately when sending prompt
2. Start question polling timer
3. On prompt response:
   - Mark session as "idle"
   - Stop question polling timer
   - Refresh session metadata via HTTP `GET /session/:id` (for title update)

#### 5b. Spinner management

Currently driven by SSE `session.status` events. Replace with:

- Set busy when `session/prompt` request is sent
- Set idle when `session/prompt` response arrives
- The spinner timer logic in `emacs-opencode-session-header.el` stays the same,
  just driven by different trigger points

#### 5c. Abort

Replace `opencode-client-session-abort` (HTTP POST) with
`opencode-acp-cancel` (JSON-RPC notification). The notification has no
response; the pending `prompt` request will eventually return with
`stopReason: "cancelled"`.

#### 5d. Session creation

Replace `opencode-client-create-session` (HTTP POST) with ACP
`session/new` request:

```json
{
  "cwd": "/path/to/project",
  "mcpServers": []
}
```

Response includes `sessionId`, `models`, `modes`.

#### 5e. Session loading / history

Use ACP `session/resume` (NOT `session/load`) to attach to an existing
session. `resumeSession` registers the session in ACP's internal state
(so future prompts and event subscriptions work) but does NOT replay
message history. This avoids the message boundary detection problem
entirely.

Then fetch message history via HTTP `GET /session/:id/message` against
the embedded server. This returns full message objects with IDs, roles,
timestamps, and parts — exactly what we need to build `opencode-message`
structs. This is the same approach we use today, just fetching from the
ACP's embedded server instead of a separate `opencode serve` instance.

The flow:
1. ACP `session/resume` with `{ sessionId, cwd, mcpServers: [] }`
   - Registers session, returns models/modes metadata
   - Does NOT replay messages (unlike `session/load`)
2. HTTP `GET /session/:id/message` against `http://127.0.0.1:<port>`
   - Returns full message objects with all metadata
   - Build `opencode-message` structs from the response (existing code)
3. Render messages into the buffer (existing rendering code, unchanged)

For session listing (the `opencode-open-session` command): use ACP
`session/list` request.

#### 5f. Slash commands

ACP handles slash commands within `session/prompt` — if the prompt text
starts with `/`, ACP parses it as a command. No change needed in the
client; just send the `/command args` text as a regular prompt.

The current code dispatches `!shell` commands to a separate HTTP endpoint.
Keep that path using the embedded HTTP server (see Phase 3 note on shell
commands).

#### 5g. Agent / model selection

ACP provides:
- `session/set_mode` — set the agent/mode
- `session/set_model` (unstable) — set the model

These replace the current approach of passing agent/model as parameters to
each prompt call. The selection UI in `emacs-opencode-session-model.el`
stays the same but calls ACP methods instead of including the selection in
the HTTP prompt body.

### Phase 6: Entry point updates (`emacs-opencode.el`)

#### 6a. `opencode-run-server` -> `opencode-start-acp`

Replace server spawning:

```
Current:  opencode serve --hostname H --port P
          Watch stdout for "listening on" message
          Health check HTTP
          Open SSE connection

New:      Find free port
          opencode acp --port P --hostname 127.0.0.1
          Send initialize JSON-RPC request
          On response: verify HTTP server via GET /path
          Cache providers/commands via HTTP
          Invoke ready callback
```

#### 6b. `opencode` command

```
Current:  opencode-run-server -> on ready:
            POST /session (HTTP) -> session-id
            opencode-session-open with session-id

New:      opencode-start-acp -> on ready:
            session/new (ACP) -> session-id
            opencode-session-open with session-id
```

#### 6c. `opencode-open-session` command

```
Current:  opencode-run-server -> on ready:
            GET /session (HTTP) -> session list
            completing-read -> selected session-id
            opencode-session-open with session-id

New:      opencode-start-acp -> on ready:
            session/list (ACP) -> session list
            completing-read -> selected session-id
            opencode-session-open with session-id
```

#### 6d. `opencode-shutdown`

```
Current:  opencode-sse-close
          opencode-connection-stop (kill serve process)
          Unregister connection

New:      opencode-acp-stop (close stdin, kill process)
          Stop any polling timers
          Unregister connection
```

### Phase 7: Delete SSE module and simplify

#### 7a. Delete `emacs-opencode-sse.el`

The entire 396-line module is no longer needed:
- No curl process management
- No SSE line parser
- No chunk processor with skip/fast-path optimizations
- No event finalization
- No `opencode-sse-define-handler` macro

#### 7b. Simplify `emacs-opencode-client.el`

Remove all HTTP-only functions that have ACP equivalents. Keep only:
- Question polling endpoints (`GET /question/`, `POST /question/:id/reply`,
  `POST /question/:id/reject`)
- Provider/command/agent metadata fetching (`GET /provider`, `GET /command`,
  `GET /agent`)
- Session metadata refresh (`GET /session/:id`)
- Shell command (`POST /session/:id/shell`)
- Auth endpoints (OAuth flow, API key setting)
- Health check (`GET /global/health`)

The `request` library dependency stays for these HTTP calls, though they're
all small payloads and infrequent. Could potentially replace with built-in
`url-retrieve` later.

#### 7c. Remove SSE-related fields from connection struct

- Drop: `sse-process`, `sse-state`
- Drop: `hostname`, `port`, `base-url` (if fully replaced by `http-base-url`)

#### 7d. Remove SSE handler registrations

All `opencode-sse-define-handler` calls in `emacs-opencode-session-handlers.el`
get replaced with ACP notification handler registrations.

#### 7e. Enable auto-revert for session-adjacent files

Add to session mode setup or as a recommendation:

```elisp
(add-hook 'opencode-session-mode-hook
          (lambda ()
            (when (bound-and-true-p global-auto-revert-mode)
              nil)  ; already handled
            ;; or: enable auto-revert in project buffers
            ))
```

## Message Model Mapping

ACP session updates need to be mapped to the existing `opencode-message` /
`opencode-message-part` structs.

### Session history (loading existing sessions)

No mapping problem here. We use `session/resume` (no replay) + HTTP
`GET /session/:id/message` which returns full message objects with IDs,
roles, timestamps, and parts. This is the same data format we use today.
The existing `opencode-session--message-from-info` and
`opencode-session--message-part-from-info` functions work unchanged.

### During prompt execution (streaming)

ACP sends part-level updates without message-level metadata. The session
handler needs to manage a "current assistant message" that accumulates
parts during a single prompt turn:

1. When `session/prompt` request is sent: create a synthetic
   `opencode-message` for the user's input (we have the text locally).
   Create a new empty `opencode-message` with role "assistant" and a
   client-generated ID to serve as the streaming target.
2. On `agent_message_chunk`: create/update a text part on the current
   assistant message, append the delta text, re-render.
3. On `agent_thought_chunk`: create/update a reasoning part, append delta.
4. On `tool_call`: create a new tool part with pending status.
5. On `tool_call_update`: update the matching tool part's status/output.
6. On `session/prompt` response (turn complete): optionally fetch
   `GET /session/:id/message?limit=2` to get the server's canonical
   message objects with real IDs, timestamps, token counts, etc. Replace
   the synthetic client-side messages with the server's version.

The post-prompt HTTP fetch (step 6) is optional but recommended. It
ensures the client's message state matches the server exactly —
including fields like `finish`, `error`, `tokens`, `cost`, and
`summary` that ACP notifications don't carry. It also gives us the
real server-assigned message IDs.

## Dependencies

### Removed
- `curl` on `$PATH` (was needed for SSE streaming)

### Kept
- `request` MELPA package (for remaining HTTP calls)
- `opencode` CLI on `$PATH` (now invoked with `acp` subcommand instead of
  `serve`)

### New
- None

## Risk Assessment

| Risk | Severity | Mitigation |
|---|---|---|
| Port TOCTOU race | Low | Bind-and-release is reliable; retry on failure |
| ACP protocol changes | Medium | Pin to known OpenCode version; protocol is versioned |
| Question polling latency | Low | 500ms-1s delay is negligible for human interaction |
| Shell command HTTP fallback | Low | Embedded server is available; straightforward |
| `auto-revert-mode` not enabled | Low | Document requirement; consider auto-enabling for project |
| Streaming message state vs server state | Low | Post-prompt HTTP fetch reconciles canonical state |

## Implementation Order

Suggested order to enable incremental testing:

1. **Phase 1** (ACP transport) — can be tested standalone with manual JSON-RPC
2. **Phase 2** (Connection lifecycle) — spawn works, initialize handshake works
3. **Phase 4a** (Notification handlers) — can render streaming content
4. **Phase 4b** (Agent request handlers) — permissions and file writes work
5. **Phase 3** (Client rewrite) — prompt submission works end-to-end
6. **Phase 5** (Session mode) — full interactive flow works
7. **Phase 6** (Entry points) — user-facing commands work
8. **Phase 7** (Cleanup) — remove SSE, simplify
9. **Phase 4d** (Question polling) — handle edge case
