# AGENTS.md

This repository is a small Emacs Lisp client for OpenCode. Keep changes minimal
and consistent with existing code.

## Docs and References

- A local copy of OpenCode lives at `~/opencode` — reference it for HTTP
  endpoint interfaces or TUI behavior.
- OpenCode docs: https://opencode.ai/docs
- `ARCHITECTURE.org` describes the high-level design; `TODO.org` tracks the roadmap.

## Build, Lint, Test

There is no Makefile or CI. Use these commands manually:

Because `emacs -Q` skips user configuration, external dependencies
(currently just `request`, installed via `straight.el`) must be added to
the load path explicitly.

```sh
# Byte-compile all files (catches undefined variables, missing requires, etc.)
emacs -Q -L . -L ~/.emacs.d/straight/build/request -batch -f batch-byte-compile *.el

# Byte-compile a single file
emacs -Q -L . -L ~/.emacs.d/straight/build/request -batch -f batch-byte-compile emacs-opencode-client.el

# Run checkdoc on a single file (docstring style)
emacs -Q -L . -L ~/.emacs.d/straight/build/request -batch --eval '(checkdoc-file "emacs-opencode-client.el")'

# Run all ERT tests
emacs -Q -L . -L ~/.emacs.d/straight/build/request -batch -L tests -l tests/run-tests.el -f ert-run-tests-batch-and-exit

# Run a single test file
emacs -Q -L . -L ~/.emacs.d/straight/build/request -batch -L tests -l tests/emacs-opencode-sse-test.el -f ert-run-tests-batch-and-exit

# Run tests matching a pattern
emacs -Q -L . -L ~/.emacs.d/straight/build/request -batch -L tests -l tests/run-tests.el --eval '(ert-run-tests-batch-and-exit "test-opencode-acp")'
```

Tests live in `tests/` with one file per source module (e.g.,
`tests/emacs-opencode-acp-test.el` tests `emacs-opencode-acp.el`).
`tests/run-tests.el` loads all test files for batch execution.

`.dir-locals.el` adds the repo root to `load-path` so `require` works
during interactive development.

## Cursor / Copilot Rules

None found at the time of writing.

## External Dependencies

- **`request`** (MELPA) — the only external Emacs package. Used in the HTTP client module.
- Built-ins used: `cl-lib`, `subr-x`, `json`, `project`.

## Code Style Guidelines

### Formatting

- Lexical binding in every file: `-*- lexical-binding: t; -*-`.
- Standard file header, `(provide '...)`, and `;;; ... ends here` trailer.
- 2-space indentation (Emacs Lisp default). Wrap around 80–100 columns.

### Naming

- Public symbols: `opencode-` prefix (e.g., `opencode-run-server`).
- Internal helpers: double-dash prefix scoped to the module
  (e.g., `opencode--normalize-directory`, `opencode-session--render-message`).
- Buffer names: space-prefixed for hidden buffers (` *opencode-server<...>*`),
  user-visible buffers use `*OpenCode: <title>*`.
- Files: `emacs-opencode-<module>.el`.

### Imports and Requires

- `require` at top-level, grouped: built-ins first, then project modules.
- Prefer `cl-lib` for CL helpers and `subr-x` for `when-let`, `string-*`.
- Use `declare-function` for forward references that would cause circular
  `require` chains — this is a key pattern in the session sub-modules.

### Docstrings

- Provide docstrings for all public functions, defcustoms, and defvars.
- First line: a complete sentence ending with a period.
- Argument names in ALL CAPS (e.g., CONNECTION, DIRECTORY).

### Types and Data

- Use `cl-defstruct` with explicit `:constructor` for structured state
  (e.g., `opencode-connection-create`, `opencode-message-create`).
- JSON data from the server arrives as alists; access with `alist-get`.
- `defcustom` for user options, `defvar` for global state,
  `defvar-local` for per-buffer session state.

### Async and Callbacks

- All server communication is async and callback-based.
- API functions take `:success` and `:error` keyword callbacks:
  ```elisp
  (opencode-client-sessions conn
    :success (lambda (&rest args)
               (let ((data (plist-get args :data))) ...))
    :error (lambda (&rest _args)
             (error "Failed")))
  ```
- All HTTP calls go through `opencode-request` to centralize auth/timeout.
- Use `opencode-connection-base-url` to build URLs; never hardcode base URLs.

### Error Handling

- `error` for fatal user-visible failures.
- `condition-case` for recoverable errors (port binding, JSON parse, user quit).
- Guard optional data with `when-let`/`if-let`.
- Clean up processes and buffers on failure.

### Buffers and Rendering

- Process output goes in dedicated hidden buffers; never mix with UI buffers.
- Always check `buffer-live-p` before modifying a buffer from a callback or
  process filter. Use `with-current-buffer` to operate on a specific buffer.
- Messages use marker-based regions (`start-marker`/`end-marker`); updates
  replace text between markers rather than re-rendering the whole buffer.
- Let-bind `inhibit-read-only` when modifying read-only rendered regions.

### User Interaction

- Mark interactive commands with `;;;###autoload`.
- Use `read-directory-name` and `completing-read` for interactive arguments.
- Use `message` for status updates; keep normal flow quiet.

### Compatibility

- Target vanilla Emacs 29+ (`emacs -Q` should work with deps installed).
- Keep the dependency list minimal.

## Project Structure

The codebase is organized in layers:

- **Entry and commands** — top-level interactive commands and the connection
  registry (directory → connection hash-table).
- **Connection lifecycle** — ACP process spawn, JSON-RPC initialize handshake,
  HTTP health check, port allocation, shutdown, and the `opencode-connection`
  struct.
- **ACP transport** — JSON-RPC 2.0 over newline-delimited JSON (ndjson) via
  stdin/stdout of the `opencode acp` child process.  Handles requests,
  responses, notifications, and agent-to-client requests.
- **ACP handlers** — maps `session/update` notification variants to the
  rendering layer.  Handles agent-to-client requests for permissions and
  file operations.
- **HTTP client** — central `opencode-request` method plus per-endpoint API
  wrappers for supplementary operations (providers, questions, shell commands,
  auth, session metadata).
- **Session data models** — pure `cl-defstruct` definitions for sessions,
  messages, and message parts. No logic or side effects.
- **Session UI** — a `define-derived-mode` major mode with marker-delimited
  input area, incremental message rendering, header-line with spinner, agent/model
  selection, and completion-at-point.

Circular dependencies between session sub-modules are broken by a shared
variables module (`emacs-opencode-session-vars`) and `declare-function`.

## Change Checklist

- Update or add docstrings for new public APIs.
- Follow existing naming and registry patterns.
- Keep errors user-friendly and fail fast on invalid state.
- Prefer editing existing files over adding new modules.
- Byte-compile to catch issues: `emacs -Q -L . -L ~/.emacs.d/straight/build/request -batch -f batch-byte-compile *.el`

## Notes for Agents

- This repo is intentionally small; keep edits surgical.
- Prefer editing existing files over introducing new modules.
- When in doubt, follow patterns in `emacs-opencode.el`.
