// Tests for opencode-sse-bridge.js
//
// Run with: node tests/opencode-sse-bridge.test.js
// or:       bun tests/opencode-sse-bridge.test.js
//
// Events are V2 envelopes: `{id, created, type, location, data, durable}`.
// The bridge reads the payload from `data` and strips a few large fields the
// Emacs client never uses (see `stripLargeFields` in opencode-sse-bridge.js).

"use strict";

const { createServer } = require("http");
const { spawn } = require("child_process");
const { resolve } = require("path");

const BRIDGE_SCRIPT = resolve(__dirname, "..", "opencode-sse-bridge.js");

// Use whichever runtime is running this test
const RUNTIME = process.argv[0];

let passed = 0;
let failed = 0;

function assert(condition, message) {
  if (!condition) {
    throw new Error("Assertion failed: " + message);
  }
}

function assertEqual(actual, expected, message) {
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    throw new Error(
      (message || "assertEqual") +
        ": expected " +
        JSON.stringify(expected) +
        ", got " +
        JSON.stringify(actual)
    );
  }
}

// Create a minimal SSE server that sends predefined events then closes
function createSSEServer(events) {
  return new Promise((resolve) => {
    const server = createServer((req, res) => {
      res.writeHead(200, {
        "Content-Type": "text/event-stream",
        "Cache-Control": "no-cache",
        Connection: "keep-alive",
      });

      for (const event of events) {
        res.write("data: " + JSON.stringify(event) + "\n\n");
      }

      // Small delay then close to let the bridge process the events
      setTimeout(() => {
        res.end();
      }, 100);
    });

    server.listen(0, "127.0.0.1", () => {
      resolve(server);
    });
  });
}

// Run the bridge and collect stdout lines
function runBridge(args, timeoutMs = 5000) {
  return new Promise((resolve, reject) => {
    const proc = spawn(RUNTIME, [BRIDGE_SCRIPT, ...args]);
    const lines = [];
    let stderr = "";
    let buf = "";

    proc.stdout.on("data", (chunk) => {
      buf += chunk.toString();
      const parts = buf.split("\n");
      buf = parts.pop();
      for (const line of parts) {
        if (line) lines.push(JSON.parse(line));
      }
    });

    proc.stderr.on("data", (chunk) => {
      stderr += chunk.toString();
    });

    const timer = setTimeout(() => {
      proc.kill("SIGTERM");
    }, timeoutMs);

    proc.on("close", (code) => {
      clearTimeout(timer);
      resolve({ lines, stderr, code });
    });

    proc.on("error", reject);
  });
}

async function test(name, fn) {
  try {
    await fn();
    console.log("  PASS: " + name);
    passed++;
  } catch (err) {
    console.log("  FAIL: " + name);
    console.log("    " + err.message);
    failed++;
  }
}

async function runTests() {
  console.log("Running bridge tests with " + RUNTIME + "...\n");

  await test("filters events by type", async () => {
    const events = [
      { type: "session.created", data: { id: "s1" } },
      { type: "session.diff", data: { sessionID: "s1", diff: "big" } },
      { type: "message.updated", data: { sessionID: "s1" } },
    ];
    const server = await createSSEServer(events);
    const port = server.address().port;

    try {
      const { lines } = await runBridge([
        "--url",
        `http://127.0.0.1:${port}/api/event`,
        "--events",
        "session.created,message.updated",
      ]);

      assertEqual(lines.length, 2, "should forward 2 of 3 events");
      assertEqual(lines[0].type, "session.created");
      assertEqual(lines[1].type, "message.updated");
    } finally {
      server.close();
    }
  });

  await test("strips summary.diffs from session.created", async () => {
    const events = [
      {
        type: "session.created",
        data: {
          id: "s1",
          title: "Test",
          summary: {
            additions: 10,
            deletions: 5,
            files: 2,
            diffs: [
              {
                file: "a.js",
                patch: "--- a/a.js\n+++ b/a.js\n@@ huge diff @@",
              },
            ],
          },
        },
      },
    ];
    const server = await createSSEServer(events);
    const port = server.address().port;

    try {
      const { lines } = await runBridge([
        "--url",
        `http://127.0.0.1:${port}/api/event`,
        "--events",
        "session.created",
      ]);

      assertEqual(lines.length, 1);
      const data = lines[0].data;
      // summary should exist but diffs should be stripped
      assert(data.summary, "summary should exist");
      assertEqual(data.summary.additions, 10, "additions preserved");
      assertEqual(data.summary.diffs, undefined, "diffs should be stripped");
    } finally {
      server.close();
    }
  });

  await test("strips summary.diffs and revert fields from session.updated", async () => {
    const events = [
      {
        type: "session.updated",
        data: {
          id: "s1",
          summary: {
            additions: 1,
            diffs: [{ file: "b.js", patch: "large patch" }],
          },
          revert: {
            diff: "revert diff text",
            snapshot: "abc123",
            other: "keep",
          },
        },
      },
    ];
    const server = await createSSEServer(events);
    const port = server.address().port;

    try {
      const { lines } = await runBridge([
        "--url",
        `http://127.0.0.1:${port}/api/event`,
        "--events",
        "session.updated",
      ]);

      assertEqual(lines.length, 1);
      const data = lines[0].data;
      assertEqual(data.summary.diffs, undefined, "summary.diffs stripped");
      assertEqual(data.summary.additions, 1, "additions preserved");
      assertEqual(data.revert.diff, undefined, "revert.diff stripped");
      assertEqual(data.revert.snapshot, undefined, "revert.snapshot stripped");
      assertEqual(data.revert.other, "keep", "other revert fields preserved");
    } finally {
      server.close();
    }
  });

  await test("strips content from session.tool.success", async () => {
    const events = [
      {
        type: "session.tool.success",
        data: {
          id: "t1",
          sessionID: "s1",
          assistantMessageID: "m1",
          content: "entire tool output here, very large ...",
          metadata: {
            input: { filePath: "/path/file.js" },
          },
        },
      },
    ];
    const server = await createSSEServer(events);
    const port = server.address().port;

    try {
      const { lines } = await runBridge([
        "--url",
        `http://127.0.0.1:${port}/api/event`,
        "--events",
        "session.tool.success",
      ]);

      assertEqual(lines.length, 1);
      const data = lines[0].data;
      assertEqual(data.content, undefined, "content stripped");
      // Preserved fields
      assertEqual(data.id, "t1", "tool id preserved");
      assertEqual(data.sessionID, "s1", "sessionID preserved");
      assertEqual(
        data.metadata.input.filePath,
        "/path/file.js",
        "metadata.input.filePath preserved"
      );
    } finally {
      server.close();
    }
  });

  await test("passes through session.text.delta unchanged", async () => {
    const events = [
      {
        type: "session.text.delta",
        data: {
          sessionID: "s1",
          assistantMessageID: "m1",
          ordinal: 0,
          delta: "hello world",
        },
      },
    ];
    const server = await createSSEServer(events);
    const port = server.address().port;

    try {
      const { lines } = await runBridge([
        "--url",
        `http://127.0.0.1:${port}/api/event`,
        "--events",
        "session.text.delta",
      ]);

      assertEqual(lines.length, 1);
      assertEqual(lines[0].data.delta, "hello world");
      assertEqual(lines[0].data.assistantMessageID, "m1");
    } finally {
      server.close();
    }
  });

  await test("passes through session.tool.called unchanged", async () => {
    const events = [
      {
        type: "session.tool.called",
        data: {
          id: "t1",
          sessionID: "s1",
          assistantMessageID: "m1",
          input: { command: "ls", raw: '{"command":"ls"}' },
        },
      },
    ];
    const server = await createSSEServer(events);
    const port = server.address().port;

    try {
      const { lines } = await runBridge([
        "--url",
        `http://127.0.0.1:${port}/api/event`,
        "--events",
        "session.tool.called",
      ]);

      assertEqual(lines.length, 1);
      const data = lines[0].data;
      assertEqual(data.input.command, "ls", "input.command preserved");
      assertEqual(data.input.raw, '{"command":"ls"}', "input.raw preserved");
    } finally {
      server.close();
    }
  });

  await test("handles auth header", async () => {
    let receivedAuth = null;
    const server = await new Promise((resolve) => {
      const s = createServer((req, res) => {
        receivedAuth = req.headers.authorization;
        res.writeHead(200, { "Content-Type": "text/event-stream" });
        res.write('data: {"type":"test.event","data":{}}\n\n');
        setTimeout(() => res.end(), 50);
      });
      s.listen(0, "127.0.0.1", () => resolve(s));
    });
    const port = server.address().port;

    try {
      await runBridge([
        "--url",
        `http://127.0.0.1:${port}/api/event`,
        "--events",
        "test.event",
        "--auth",
        "dXNlcjpwYXNz",
      ]);

      assertEqual(receivedAuth, "Basic dXNlcjpwYXNz", "auth header sent");
    } finally {
      server.close();
    }
  });

  await test("exits with error on connection failure", async () => {
    const { code, stderr } = await runBridge([
      "--url",
      "http://127.0.0.1:1/api/event",
      "--events",
      "test.event",
    ]);

    assert(code !== 0, "should exit with non-zero code");
    assert(stderr.length > 0, "should have stderr output");
  });

  await test("exits with error for missing --url", async () => {
    const { code, stderr } = await runBridge(["--events", "test.event"]);
    assert(code !== 0, "should exit with non-zero code");
    assert(stderr.includes("--url"), "stderr mentions --url");
  });

  await test("exits with error for missing --events", async () => {
    const { code, stderr } = await runBridge([
      "--url",
      "http://localhost:9999/api/event",
    ]);
    assert(code !== 0, "should exit with non-zero code");
    assert(stderr.includes("--events"), "stderr mentions --events");
  });

  console.log(
    "\n" + (passed + failed) + " tests, " + passed + " passed, " + failed + " failed"
  );
  process.exit(failed > 0 ? 1 : 0);
}

runTests().catch((err) => {
  console.error("Test runner error:", err);
  process.exit(1);
});
