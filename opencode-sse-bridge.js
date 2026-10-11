// opencode-sse-bridge.js — SSE bridge for emacs-opencode
//
// Connects to an OpenCode SSE endpoint, parses events, filters by type,
// strips known-large fields, and emits one JSON line per event to stdout.
//
// Usage:
//   node opencode-sse-bridge.js --url URL --events type1,type2,... [--auth BASE64]
//
// Runs under bun or node (18+). No external dependencies.

"use strict";

// ---------------------------------------------------------------------------
// Argument parsing
// ---------------------------------------------------------------------------

function parseArgs(argv) {
  const args = { url: null, events: null, auth: null };
  for (let i = 2; i < argv.length; i++) {
    switch (argv[i]) {
      case "--url":
        args.url = argv[++i];
        break;
      case "--events":
        args.events = argv[++i];
        break;
      case "--auth":
        args.auth = argv[++i];
        break;
      default:
        process.stderr.write("Unknown argument: " + argv[i] + "\n");
        process.exit(1);
    }
  }
  if (!args.url) {
    process.stderr.write("Missing required --url argument\n");
    process.exit(1);
  }
  if (!args.events) {
    process.stderr.write("Missing required --events argument\n");
    process.exit(1);
  }
  return args;
}

// ---------------------------------------------------------------------------
// Field stripping
// ---------------------------------------------------------------------------

// Strip fields from parsed event data that the Emacs client never reads but
// that can be very large.  The rules are derived from tracing through the
// Emacs handler code and confirming which fields are accessed.
//
// V2 events arrive as an envelope `{id, created, type, location, data,
// durable}`; the payload the client consumes is `event.data`.

function stripLargeFields(event) {
  const type = event.type;
  const data = event.data;
  if (!data || typeof data !== "object") return;

  // session.created / session.updated — strip summary.diffs and revert
  if (type === "session.created" || type === "session.updated") {
    if (data.summary) delete data.summary.diffs;
    if (data.revert) {
      delete data.revert.diff;
      delete data.revert.snapshot;
    }
  }

  // session.tool.success — strip full tool output from `content`.  The
  // Emacs client shows output in the per-tool drawer and refetches the
  // message on demand (the message endpoint returns full content).
  if (type === "session.tool.success") {
    delete data.content;
  }
}

// ---------------------------------------------------------------------------
// SSE parser
// ---------------------------------------------------------------------------

async function streamSSE(url, headers, allowedEvents, signal) {
  const response = await fetch(url, { headers, signal });

  if (!response.ok) {
    throw new Error("SSE connection failed: " + response.status + " " + response.statusText);
  }

  const body = response.body;
  if (!body) {
    throw new Error("SSE response has no body");
  }

  const reader = body.pipeThrough(new TextDecoderStream()).getReader();
  let buf = "";

  try {
    for (;;) {
      const { done, value } = await reader.read();
      if (done) break;

      // Normalize line endings and append to buffer
      buf += value.replace(/\r\n/g, "\n").replace(/\r/g, "\n");

      // Split on double-newline (SSE frame boundary)
      const frames = buf.split("\n\n");
      // Last element is either empty (clean boundary) or an incomplete frame
      buf = frames.pop();

      for (const frame of frames) {
        if (!frame) continue;

        // Extract data lines
        let data = "";
        for (const line of frame.split("\n")) {
          if (line.startsWith("data: ")) {
            data += (data ? "\n" : "") + line.slice(6);
          } else if (line.startsWith("data:")) {
            data += (data ? "\n" : "") + line.slice(5);
          }
          // Ignore event:, id:, retry:, and comment lines
        }

        if (!data) continue;

        let parsed;
        try {
          parsed = JSON.parse(data);
        } catch (_e) {
          // Skip malformed JSON
          continue;
        }

        const eventType = parsed.type;
        if (!eventType) continue;

        // Filter: only forward events the client cares about
        if (!allowedEvents.has(eventType)) continue;

        // Strip large fields the client doesn't need
        stripLargeFields(parsed);

        // Emit one JSON line to stdout
        process.stdout.write(JSON.stringify(parsed) + "\n");
      }
    }
  } finally {
    reader.releaseLock();
  }
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

async function main() {
  const args = parseArgs(process.argv);

  const allowedEvents = new Set(args.events.split(","));

  const headers = { Accept: "text/event-stream" };
  if (args.auth) {
    headers["Authorization"] = "Basic " + args.auth;
  }

  const controller = new AbortController();

  // Forward SIGTERM/SIGINT to abort the fetch
  process.on("SIGTERM", () => controller.abort());
  process.on("SIGINT", () => controller.abort());

  // Exit cleanly when stdout is closed (Emacs killed the process)
  process.stdout.on("error", (err) => {
    if (err.code === "EPIPE") {
      controller.abort();
      process.exit(0);
    }
  });

  try {
    await streamSSE(args.url, headers, allowedEvents, controller.signal);
    // Stream ended normally
    process.exit(0);
  } catch (err) {
    if (err.name === "AbortError") {
      process.exit(0);
    }
    process.stderr.write("SSE bridge error: " + err.message + "\n");
    process.exit(1);
  }
}

main();
