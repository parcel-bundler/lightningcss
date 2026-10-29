import { test } from "node:test";
import assert from "node:assert/strict";
import { capture } from "../extract.mjs";
import { classify } from "../run.mjs";
import { Adapter } from "../adapter.mjs";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const document = (code) => ({
  scripts: [
    { attrs: { src: "/css/support/parsing-testcommon.js" } },
    { attrs: { src: "/css/support/shorthand-testcommon.js" } },
    { attrs: {}, code, line: 12 },
  ],
  variants: [],
  contextFree: true,
});

test("extractor option typos fail before opening the checkout", () => {
  for (const options of [["--discovry"], ["--output"], ["--output", "--discovery"]]) {
    const result = spawnSync(process.execPath,
      [fileURLToPath(new URL("../extract.mjs", import.meta.url)), "nonexistent-wpt-checkout", ...options],
      { encoding: "utf8" });
    assert.equal(result.status, 1);
    assert.match(result.stderr, /Unknown or incomplete option/);
    assert.ok(!result.stderr.includes("fatal:"));
  }
});

test("captures generated cases, omitted expectations, alternatives, and stable IDs", () => {
  const source = document(`for (const n of [1, 2]) test_valid_value('width', n + 'px');
    test_valid_value('color', 'red', ['red', 'rgb(255, 0, 0)']);`);
  const first = capture("css/example.html", source);
  assert.equal(first.status, "extracted");
  assert.equal(first.cases.length, 3);
  assert.equal(first.cases[0].expected, "1px");
  assert.equal(first.cases[0].scriptLine, 12);
  assert.deepEqual(
    first.cases.map((c) => c.id),
    capture("css/example.html", source).cases.map((c) => c.id),
  );
  assert.deepEqual(Array.from(first.cases[2].expected), ["red", "rgb(255, 0, 0)"]);
});

test("resolves relative helper scripts and preserves execution order", () => {
  const input = document("");
  input.scripts = [
    { attrs: { src: "../support/parsing-testcommon.js" } },
    { attrs: {}, code: `const value = 'red';`, line: 2 },
    { attrs: {}, code: `test_valid_value('color', value);`, line: 4 },
  ];
  assert.equal(capture("css/example/test.html", input).cases[0].value, "red");
});

test("unsupported setup discards all partial cases", () => {
  for (const suffix of [
    "document.createElement('div')",
    "test_valid_selector('a')",
    "test_valid_value('color', 'red', 'red', {comparisonFunction() {}})",
    "test_valid_value('color', '\\ud800')",
    "while (true) {}",
  ]) {
    const result = capture(
      "css/example.html",
      document(`test_valid_value('color', 'red'); ${suffix}`),
    );
    assert.equal(result.status, "unsupported", suffix);
    assert.equal(result.cases.length, 0);
  }
});

test("promise callbacks register cases before capture returns", () => {
  const result = capture("css/example.html", document(`
    Promise.resolve().then(() => test_valid_value('color', 'red'));
    (async () => { await 0; test_valid_value('color', 'blue'); })();
  `));
  assert.equal(result.status, "extracted");
  assert.deepEqual(result.cases.map(c => c.value), ["red", "blue"]);
});

test("promise loops remain inside the evaluation deadline", () => {
  // Keep a process-level deadline so a broken VM timeout cannot hang the suite.
  const source = document(`Promise.resolve().then(() => { while (true) {} });`);
  const result = spawnSync(process.execPath, ["--input-type=module", "-e", `
    import {capture} from ${JSON.stringify(new URL("../extract.mjs", import.meta.url).href)};
    console.log(JSON.stringify(capture('css/example.html', ${JSON.stringify(source)})));
  `], {encoding: "utf8", timeout: 5000});
  assert.equal(result.status, 0, result.stderr || result.error?.message);
  const captured = JSON.parse(result.stdout);
  assert.equal(captured.status, "unsupported");
  assert.match(captured.reason, /timed out/);
  assert.deepEqual(captured.cases, []);
});

test("caught and asynchronous unsupported helper calls discard partial cases", () => {
  for (const code of [
    `try { test_valid_value('color', 'red', 'red', {comparisonFunction() {}}); } catch {}
     test_valid_value('color', 'blue');`,
    `test_valid_value('color', 'blue');
     Promise.resolve().then(() => test_valid_value('color', 'red', 'red', {comparisonFunction() {}}));`,
  ]) {
    const result = capture("css/example.html", document(code));
    assert.equal(result.status, "unsupported");
    assert.match(result.reason, /custom comparison/);
    assert.deepEqual(result.cases, []);
  }
});

test("empty pages, variants and unsupported dependencies stay visible", () => {
  assert.equal(capture("css/example.html", document("")).status, "empty");
  assert.equal(
    capture("css/example.html", { ...document(""), variants: ["?one"] }).status,
    "unsupported",
  );
  const input = document("");
  input.scripts.push({ attrs: { src: "https://example.org/helpers.js" } });
  assert.equal(capture("css/example.html", input).status, "unsupported");
});

test("valid preserved values are coverage gaps, not recognition passes", () => {
  assert.equal(classify({ kind: "valid" }, { recognition: "unparsed" }), "value-coverage-gap");
  assert.equal(classify({ kind: "invalid" }, { recognition: "unparsed" }), "preserved-invalid");
  assert.equal(classify({ kind: "invalid" }, { recognition: "typed" }), "invalid-typed");
  assert.equal(classify({ kind: "invalid" }, { recognition: "panic" }), "robustness-failure");
});

test("adapter restarts after an abort and attributes timeout to the request", async () => {
  const script = `require('readline').createInterface({input: process.stdin}).on('line', line => {
    const v = JSON.parse(line);
    if (v.crash) process.exit(7);
    else if (!v.hang) console.log(JSON.stringify({requestId:v.requestId, recognition: 'typed', code:'color:red'}));
  });`;
  const adapter = new Adapter(process.execPath, 1000, ["-e", script]);
  try {
    assert.equal((await adapter.request({ crash: true })).recognition, "crash");
    assert.equal((await adapter.request({})).recognition, "typed");
    assert.equal((await adapter.request({ hang: true })).recognition, "timeout");
    assert.equal((await adapter.request({})).recognition, "typed");
  } finally {
    adapter.close();
  }
});

test("helper availability follows script loading order and execution attributes", () => {
  const code = "test_valid_value('color', 'red')";
  const inline = { attrs: {}, code, line: 1 };
  const helper = { attrs: { src: "/css/support/parsing-testcommon.js" } };
  for (const scripts of [
    [inline, helper],
    [{ attrs: { ...helper.attrs, type: "module" } }, inline],
    [{ attrs: { ...helper.attrs, async: null } }, inline],
    [{ attrs: { ...helper.attrs, defer: null } }, inline],
    [{ attrs: { src: "" }, code }, inline],
  ]) {
    const result = capture("css/example.html", { ...document(""), scripts });
    assert.equal(result.status, "unsupported");
    assert.equal(result.cases.length, 0);
  }
  const skipped = capture("css/example.html", {
    ...document(""), scripts: [helper, { ...inline, attrs: { nomodule: null } }],
  });
  assert.equal(skipped.status, "empty");
});

test("HTML reader ignores inert scripts and identifies fixture setup", () => {
  const source = `<template><script>inert()</script></template>
    <noscript><script>alsoInert()</script></noscript>
    <link rel="alternate STYLESHEET" href="test.css"><link rel>
    <body onload="setup()"><script>executed()</script>`;
  const result = spawnSync("python3", [fileURLToPath(new URL("../read-html.py", import.meta.url))], {
    input: JSON.stringify(source) + "\n", encoding: "utf8",
  });
  assert.equal(result.status, 0, result.stderr);
  const parsed = JSON.parse(result.stdout);
  assert.equal(parsed.scripts.length, 1);
  assert.equal(parsed.scripts[0].code, "executed()");
  assert.equal(parsed.contextFree, false);
});

test("declarative shadow-root scripts are deferred rather than treated as inert", () => {
  const source = `<script src="/css/support/parsing-testcommon.js"></script>
    <script>test_valid_value('color', 'red')</script>
    <div><template shadowrootmode="open"><script>test_valid_value('color', 'blue')</script></template></div>`;
  const result = spawnSync("python3", [fileURLToPath(new URL("../read-html.py", import.meta.url))], {
    input: JSON.stringify(source) + "\n", encoding: "utf8",
  });
  assert.equal(result.status, 0, result.stderr);
  const parsed = JSON.parse(result.stdout);
  const captured = capture("css/example.html", parsed);
  assert.equal(captured.status, "unsupported");
  assert.match(captured.reason, /declarative shadow roots/);
  assert.equal(captured.cases.length, 0);
});

test("HTML reader preserves raw script text and detects fixture dependencies", () => {
  const html = `<!doctype html>\n<style>div {color:red}</style>\n<script>test_valid_value("content", '"&amp;"');</script>`;
  const result = spawnSync(
    "python3",
    [fileURLToPath(new URL("../read-html.py", import.meta.url))],
    {
      input: JSON.stringify(html) + "\n",
      encoding: "utf8",
    },
  );
  assert.equal(result.status, 0, result.stderr);
  const parsed = JSON.parse(result.stdout);
  assert.equal(parsed.contextFree, false);
  assert.equal(parsed.scripts[0].line, 3);
  assert.ok(parsed.scripts[0].code.includes("&amp;"));
});

test("valid surrogate pairs and literal CSS escapes are preserved", () => {
  const values = ["😀", String.raw`\ud800`];
  const result = capture(
    "css/example.html",
    document(
      values.map((value) => `test_valid_value('content', ${JSON.stringify(value)});`).join("\n"),
    ),
  );
  assert.equal(result.status, "extracted");
  assert.deepEqual(
    result.cases.map((c) => c.value),
    values,
  );
});
