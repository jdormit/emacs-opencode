// Run with node or bun tests/opencode-sse-bridge.test.js.
"use strict";

const assert = require("node:assert/strict");
const { createServer } = require("node:http");
const { spawn } = require("node:child_process");
const { resolve } = require("node:path");

function runBridge(args) {
  return new Promise((resolve, reject) => {
    const proc = spawn(process.execPath, [resolveScript(), ...args]);
    let stdout = "";
    let stderr = "";
    const timeout = setTimeout(() => proc.kill(), 5000);
    proc.stdout.on("data", (data) => { stdout += data; });
    proc.stderr.on("data", (data) => { stderr += data; });
    proc.on("error", reject);
    proc.on("close", (code) => {
      clearTimeout(timeout);
      resolve({ code, stderr, lines: stdout.trim().split("\n").filter(Boolean).map(JSON.parse) });
    });
  });
}

function resolveScript() {
  return resolve(__dirname, "..", "opencode-sse-bridge.js");
}

async function stream(events, types, verify, auth) {
  let receivedAuth;
  const server = createServer((req, res) => {
    receivedAuth = req.headers.authorization;
    res.writeHead(200, { "Content-Type": "text/event-stream" });
    for (const event of events) res.write(`data: ${JSON.stringify(event)}\n\n`);
    res.end();
  });
  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  try {
    const args = ["--url", `http://127.0.0.1:${server.address().port}/api/event`, "--events", types];
    if (auth) args.push("--auth", auth);
    const result = await runBridge(args);
    assert.equal(result.code, 0, result.stderr);
    verify(result.lines, receivedAuth);
  } finally {
    server.close();
  }
}

const calls = [
  { tool: "tasks.list", status: "completed", input: { directory: "/tmp/project" } },
  { tool: "tasks.remove", status: "error", input: { id: "missing" } },
];
const location = { directory: "/tmp/project" };
const cases = [
  ["filters events", async () => {
    await stream([{ type: "server.connected", data: {} }, { type: "unused", data: {} }],
      "server.connected", (lines) => assert.equal(lines.length, 1));
  }],
  ["preserves Code Mode calls and location on success and progress", async () => {
    const events = ["session.tool.success", "session.tool.progress"].map((type) => ({
      type, location, data: { id: "call", metadata: { toolCalls: calls },
        content: [{ type: "text", text: "large result" }], providerState: "opaque" },
    }));
    await stream(events, "session.tool.success,session.tool.progress", (lines) => {
      for (const line of lines) {
        assert.deepEqual(line.location, location);
        assert.deepEqual(line.data.metadata.toolCalls, calls);
        assert.equal(line.data.providerState, undefined);
      }
      assert.equal(lines[0].data.content, undefined);
    });
  }],
  ["strips failed tool content but preserves the error", async () => {
    await stream([{ type: "session.tool.failed", data: {
      content: [{ type: "text", text: "large" }], error: { message: "failed" },
    } }], "session.tool.failed", ([line]) => {
      assert.equal(line.data.content, undefined);
      assert.equal(line.data.error.message, "failed");
    });
  }],
  ["strips shell output but preserves exit status", async () => {
    await stream([{ type: "session.shell.ended", data: { output: { output: "large", exit: 1 } } }],
      "session.shell.ended", ([line]) => assert.deepEqual(line.data.output, { exit: 1 }));
  }],
  ["preserves text deltas", async () => {
    const event = { type: "session.text.delta", location, data: { delta: "hello", ordinal: 0 } };
    await stream([event], event.type, ([line]) => assert.deepEqual(line, event));
  }],
  ["strips synthetic text and attachment data", async () => {
    await stream([{ type: "session.inbox.enqueued", data: { item: {
      type: "synthetic", payload: { text: "large", files: [{ data: "large", mime: "image/png" }] },
    } } }], "session.inbox.enqueued", ([line]) => {
      assert.equal(line.data.item.payload.text, undefined);
      assert.deepEqual(line.data.item.payload.files, [{ mime: "image/png" }]);
    });
  }],
  ["sends Basic auth", async () => {
    await stream([{ type: "server.connected", data: {} }], "server.connected",
      (_lines, auth) => assert.equal(auth, "Basic dXNlcjpwYXNz"), "dXNlcjpwYXNz");
  }],
  ["fails on connection refusal", async () => {
    const result = await runBridge(["--url", "http://127.0.0.1:1/api/event", "--events", "server.connected"]);
    assert.notEqual(result.code, 0);
  }],
  ["requires URL and events", async () => {
    assert.notEqual((await runBridge(["--events", "test"])).code, 0);
    assert.notEqual((await runBridge(["--url", "http://localhost"])).code, 0);
  }],
];

(async () => {
  let failed = 0;
  for (const [name, test] of cases) {
    try {
      await test();
      console.log(`PASS: ${name}`);
    } catch (error) {
      failed++;
      console.error(`FAIL: ${name}`, error);
    }
  }
  console.log(`${cases.length - failed}/${cases.length} passed`);
  process.exitCode = failed ? 1 : 0;
})();
