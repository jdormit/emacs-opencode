# AGENTS.md

This repository is a small Emacs Lisp client for OpenCode. Keep changes minimal
and consistent with existing files (`emacs-opencode.el`, `emacs-opencode-connection.el`,
`emacs-opencode-client.el`).

## Build, Lint, Test

No explicit build, lint, or test scripts were found in the repo (no Makefile,
package metadata, or test files). Use the following Emacs-centric commands when
needed.

### Byte-compile (batch)

- Compile all Elisp in the repo:
  `emacs -Q --batch -L . -f batch-byte-compile emacs-opencode.el emacs-opencode-connection.el emacs-opencode-client.el`

### Lint (optional)

- If `package-lint` is installed locally:
  `emacs -Q --batch -L . -l package-lint -f package-lint-batch-and-exit emacs-opencode.el`
- If `elint` is available:
  `emacs -Q --batch -L . -f elint-batch emacs-opencode.el`

### Tests (template)

- No tests currently exist. If tests are added, prefer ERT.
- Run all ERT tests (example):
  `emacs -Q --batch -L . -l test/opencode-test.el -f ert-run-tests-batch-and-exit`
- Run a single test (example):
  `emacs -Q --batch -L . -l test/opencode-test.el -f ert-run-tests-batch-and-exit --eval '(ert-run-tests-batch "test-name")'`

## Cursor / Copilot Rules

No Cursor rules (`.cursor/rules/` or `.cursorrules`) or GitHub Copilot rules
(`.github/copilot-instructions.md`) were found at the time of writing.

## Code Style Guidelines

### Formatting

- Use Emacs Lisp conventions: 2-space indentation, align parens as per `lisp-mode`.
- Keep lines readable; wrap docstrings and long forms around ~80–100 columns.
- Prefer lexical binding; existing files use `-*- lexical-binding: t; -*-`.
- Use standard file headers and `provide`/`ends here` trailer comments.

### Naming

- Prefix public symbols with `opencode-` and internal helpers with `opencode--`.
- Use full, descriptive names (`opencode-connection-start`, not abbreviations).
- Keep buffer names consistent with existing patterns (e.g., ` *opencode-server<...>*`).
- File names should be prefixed with `emacs-opencode-`.

### Imports and Requires

- Use `require` at top-level, grouped by built-ins then project files.
- Prefer `cl-lib` for Common Lisp helpers and `subr-x` for `when-let`, `string-*`.
- Avoid `eval-when-compile` unless compile-time macros are needed.

### Docstrings

- Provide docstrings for all public functions, defcustoms, and defvars.
- First line should be a complete sentence ending with a period.
- Use ALL CAPS for argument names in docstrings (e.g., DIRECTORY, CONNECTION).

### Types and Data

- Favor `cl-defstruct` for structured state (see `opencode-connection`).
- Prefer alists and hash tables when matching existing patterns.
- Use `defcustom` for user-configurable options and `defvar` for internal state.

### Error Handling

- Use `error` for fatal user-visible failures (consistent with current code).
- Guard optional data with `when-let`/`if-let` or explicit `unless` checks.
- Clean up processes/buffers on failure when appropriate.
- Keep server interactions defensive: timeouts, health checks, and graceful shutdown.

### Functions and Control Flow

- Keep functions focused; prefer small helpers like `opencode--normalize-directory`.
- Use callbacks for async request handling (`:success`/`:error` on `request`).
- Avoid global mutable state outside the `opencode--connections` registry.

### Networking

- All HTTP calls should go through `opencode-request` to centralize auth/timeout.
- Use `opencode-connection-base-url` to build URLs; avoid manual base URL strings.

### Processes and Buffers

- Keep process output in dedicated buffers; do not mix with user UI buffers.
- Always check `buffer-live-p` before modifying buffers from a process filter.
- When stopping a process, delete it and clean up its buffer (as in
  `opencode-connection-stop`).

### User Interaction

- Provide interactive commands with `;;;###autoload` when appropriate.
- For interactive args, use `read-directory-name` and `completing-read` patterns
  used in `emacs-opencode.el`.
- Use `message` for status updates; avoid noisy logging in normal flow.

### Compatibility

- Target a vanilla Emacs environment (`emacs -Q` should work if dependencies are
  installed). Avoid heavy dependencies and keep `require` list minimal.

## Project Structure Notes

- `emacs-opencode.el`: entry points, connection registry, commands.
- `emacs-opencode-connection.el`: server process lifecycle, connection struct.
- `emacs-opencode-client.el`: HTTP request wrapper and API calls.
- `ARCHITECTURE.org` and `TODO.org` describe planned workflows and roadmap.

## Change Checklist

- Update or add docstrings for new public APIs.
- Use existing naming and registry patterns.
- Keep errors user-friendly and fail fast on invalid state.
- Avoid adding new files unless required by the task.
- If adding tests, document how to run a single test.

## Notes for Agents

- This repo is intentionally small; keep edits surgical.
- Prefer editing existing files over introducing new modules.
- When in doubt, follow patterns in `emacs-opencode.el`.
