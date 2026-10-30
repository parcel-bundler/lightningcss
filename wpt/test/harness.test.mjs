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
    Promise.resolve().then(test_valid_value.bind(null, 'color', 'green', 'green'));
    Promise.resolve('yellow').then(test_valid_value.bind(null, 'color'));
  `));
  assert.equal(result.status, "extracted");
  assert.deepEqual(result.cases.map(c => c.value), ["red", "blue", "green", "yellow"]);
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
    `test_valid_value('color', 'blue');
     Promise.resolve().then(test_valid_value.bind(null, 'color', 'red', 'red', {comparisonFunction() {}}));`,
  ]) {
    const result = capture("css/example.html", document(code));
    assert.equal(result.status, "unsupported");
    assert.match(result.reason, /custom comparison/);
    assert.deepEqual(result.cases, []);
  }
});

test("non-Error throws remain atomic page errors with nonempty reasons", () => {
  for (const value of ["null", "undefined", "false", "0", '""', '"failure"',
    "{get message() {throw null}, toString() {throw null}}"]) {
    const result = capture("css/example.html", document(`
      test_valid_value('color', 'red'); throw ${value};
    `));
    assert.equal(result.status, "unsupported", value);
    assert.deepEqual(result.cases, []);
    assert.equal(typeof result.reason, "string");
    assert.ok(result.reason.length > 0);
  }
});

test("falsey exceptions during helper validation cannot hide extraction gaps", () => {
  for (const value of ["null", "undefined", "false", "0", '""']) {
    const result = capture("css/example.html", document(`
      test_valid_value('color', 'red');
      test_shorthand_value('margin', '0px', {get 'margin-top'() {throw ${value};}});
      test_valid_value('color', 'blue');
    `));
    assert.equal(result.status, "unsupported", value);
    assert.deepEqual(result.cases, []);
    assert.equal(typeof result.reason, "string");
    assert.ok(result.reason.length > 0);
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

test("HTML text elements do not expose inert script calls", () => {
  const tags = ["textarea", "title", "xmp", "iframe", "noembed", "noframes"];
  const source = `<script src="/css/support/parsing-testcommon.js"></script>` +
    tags.map(tag => `<${tag}><script>test_valid_value('color', 'blue')</script></${tag}>`).join("\n") +
    `<script>test_valid_value('color', 'red')</script>`;
  const result = spawnSync("python3", [fileURLToPath(new URL("../read-html.py", import.meta.url))], {
    input: JSON.stringify(source) + "\n", encoding: "utf8",
  });
  assert.equal(result.status, 0, result.stderr);
  const captured = capture("css/example.html", JSON.parse(result.stdout));
  assert.equal(captured.status, "extracted");
  assert.deepEqual(captured.cases.map(c => c.value), ["red"]);
});

test("duplicate HTML attributes keep their first value for dependency checks", () => {
  const source = `<script src="/unexpected.js" src="/css/support/parsing-testcommon.js"></script>
    <script>test_valid_value('color', 'red')</script>`;
  const result = spawnSync("python3", [fileURLToPath(new URL("../read-html.py", import.meta.url))], {
    input: JSON.stringify(source) + "\n", encoding: "utf8",
  });
  assert.equal(result.status, 0, result.stderr);
  const captured = capture("css/example.html", JSON.parse(result.stdout));
  assert.equal(captured.status, "unsupported");
  assert.match(captured.reason, /unsupported dependency: \/unexpected.js/);
  assert.deepEqual(captured.cases, []);
});

test("unsupported HTML text modes cannot silently expose subsequent scripts", () => {
  for (const prefix of ["<plaintext></plaintext>", "<template/>", "<script/>", "<iframe/>"]) {
    const source = `<script src="/css/support/parsing-testcommon.js"></script>${prefix}
      <script>test_valid_value('color', 'red')</script>`;
    const result = spawnSync("python3", [fileURLToPath(new URL("../read-html.py", import.meta.url))], {
      input: JSON.stringify(source) + "\n", encoding: "utf8",
    });
    assert.equal(result.status, 0, result.stderr);
    const captured = capture("css/example.html", JSON.parse(result.stdout));
    assert.equal(captured.status, "unsupported", prefix);
    assert.match(captured.reason, /requires? a browser adapter/);
    assert.deepEqual(captured.cases, []);
  }
});

test("expectations are snapshots of each synchronous WPT assertion", () => {
  const result = capture("css/example.html", document(`
    const alternatives = ['red'];
    test_valid_value('color', 'red', alternatives);
    alternatives[0] = 'blue';
    test_valid_value('color', 'blue', alternatives);
    const longhands = {'margin-top':'1px'};
    test_shorthand_value('margin', '1px', longhands);
    longhands['margin-top'] = '2px';
    test_shorthand_value('margin', '2px', longhands);
  `));
  assert.equal(result.status, "extracted");
  assert.deepEqual(result.cases.map(c => c.expected), [
    ['red'], ['blue'], {'margin-top':'1px'}, {'margin-top':'2px'},
  ]);
});

test("effective custom comparators and null options cannot lose their semantics", () => {
  for (const options of [
    "Object.create({comparisonFunction() {}})",
    "Object.defineProperty({}, 'comparisonFunction', {value() {}})",
    "null",
  ]) {
    const result = capture("css/example.html", document(`test_valid_value('color', 'red', 'red', ${options});`));
    assert.equal(result.status, "unsupported", options);
    assert.match(result.reason, /custom comparison/);
    assert.deepEqual(result.cases, []);
  }
});

test("legacy execution attributes do not install helpers or collect inert calls", () => {
  for (const attrs of [{language:'vbscript'}, {for:'other', event:'onclick'}]) {
    const input = document("test_valid_value('color', 'red')");
    input.scripts.at(-1).attrs = attrs;
    const result = capture("css/example.html", input);
    assert.equal(result.status, "unsupported");
    assert.match(result.reason, /legacy script attributes/);
    assert.deepEqual(result.cases, []);
  }
});

test("foreign namespaces and base URLs cannot silently change script execution", () => {
  for (const source of [
    `<svg><template><script>test_valid_value('color', 'red')</script></template></svg>`,
    `<svg><title><script>test_valid_value('color', 'red')</script></title></svg>`,
    `<math><script>test_valid_value('color', 'red')</script></math>`,
    `<base href="https://different-origin.invalid/">`,
  ]) {
    const result = spawnSync("python3", [fileURLToPath(new URL("../read-html.py", import.meta.url))], {
      input: JSON.stringify(source) + "\n", encoding: "utf8",
    });
    assert.equal(result.status, 0, result.stderr);
    const captured = capture("css/example.html", JSON.parse(result.stdout));
    assert.equal(captured.status, "unsupported", source);
    assert.match(captured.reason, /elements require a browser adapter/);
    assert.deepEqual(captured.cases, []);
  }
});

test("source error getters and coercions cannot escape the evaluation deadline", () => {
  for (const expression of [
    "{get message() {while(true) {}}}",
    "{toString() {while(true) {}}}",
    "Object.defineProperty(Error('x'), 'message', {get() {while(true) {}}})",
    "new Proxy({}, {getOwnPropertyDescriptor() {while(true) {}}, get() {while(true) {}}})",
  ]) {
    const result = spawnSync(process.execPath, ["--input-type=module", "-e", `
      import {capture} from ${JSON.stringify(new URL("../extract.mjs", import.meta.url).href)};
      console.log(JSON.stringify(capture('css/example.html', ${JSON.stringify(document('throw '+expression))})));
    `], {encoding:'utf8', timeout:5000});
    assert.equal(result.status, 0, result.stderr || result.error?.message);
    const captured = JSON.parse(result.stdout);
    assert.equal(captured.status, "unsupported");
    assert.equal(captured.reason, "Unknown extraction error");
    assert.deepEqual(captured.cases, []);
  }
});

test("isolated capture records Promise rejections atomically and continues", async () => {
  const {CaptureWorker} = await import('../capture.mjs');
  const worker = new CaptureWorker();
  try {
    for (const suffix of [
      "Promise.resolve().then(() => {throw Error('async setup failed')})",
      "(async () => {await 0; throw Error('async setup failed')})()",
    ]) {
      const result = await worker.request('css/error.html', document(`test_valid_value('color','red'); ${suffix}`));
      assert.equal(result.status, 'unsupported');
      assert.equal(result.reason, 'async setup failed');
      assert.deepEqual(result.cases, []);
    }
    const valid = await worker.request('css/valid.html', document("test_valid_value('color','blue')"));
    assert.equal(valid.status, 'extracted');
    assert.equal(valid.cases[0].value, 'blue');
  } finally { await worker.close(); }
});

test("strict unhandled rejection mode cannot kill the corpus extractor", () => {
  const bad = document("test_valid_value('color','red'); Promise.reject(Error('strict async failure'))");
  const good = document("test_valid_value('color','blue')");
  const result = spawnSync(process.execPath, ['--unhandled-rejections=strict', '--input-type=module', '-e', `
    import {CaptureWorker} from ${JSON.stringify(new URL('../capture.mjs', import.meta.url).href)};
    const worker = new CaptureWorker();
    try {
      const bad = await worker.request('css/bad.html', ${JSON.stringify(bad)});
      const good = await worker.request('css/good.html', ${JSON.stringify(good)});
      console.log(JSON.stringify({bad, good}));
    } finally { await worker.close(); }
  `], {encoding:'utf8', timeout:10000});
  assert.equal(result.status, 0, result.stderr || result.error?.message);
  const {bad: failed, good: passed} = JSON.parse(result.stdout);
  assert.equal(failed.status, 'unsupported');
  assert.match(failed.reason, /strict async failure/);
  assert.deepEqual(failed.cases, []);
  assert.equal(passed.status, 'extracted');
});

test("page deadlines replace a stalled capture worker and preserve later pages", async () => {
  const {CaptureWorker} = await import('../capture.mjs');
  const worker = new CaptureWorker();
  try {
    const input = document("test_valid_value('color','red')");
    input.scripts.splice(2, 0, ...Array.from({length:8}, () => ({
      attrs:{}, line:1, code:'{const start=Date.now(); while(Date.now()-start<300) {}}',
    })));
    const failed = await worker.request('css/slow.html', input);
    assert.equal(failed.status, 'unsupported');
    assert.equal(failed.file, 'css/slow.html');
    assert.match(failed.reason, /Page execution timed out after 1500ms/);
    assert.deepEqual(failed.cases, []);
    const passed = await worker.request('css/next.html', document("test_valid_value('color','blue')"));
    assert.equal(passed.status, 'extracted');
    assert.equal(passed.cases[0].value, 'blue');
  } finally { await worker.close(); }
});

test("HTML execution policies and frameset mode cannot produce phantom cases", () => {
  for (const prefix of [
    `<meta http-equiv="Content-Security-Policy" content="script-src 'none'">`,
    `<meta HTTP-EQUIV="CONTENT-SECURITY-POLICY" content="script-src 'none'">`,
    `<frameset>`,
  ]) {
    const source = `<!doctype html>${prefix}
      <script src="/css/support/parsing-testcommon.js"></script>
      <script>test_valid_value('color','red')</script>`;
    const parsed = spawnSync('python3', [fileURLToPath(new URL('../read-html.py', import.meta.url))], {
      input:JSON.stringify(source)+'\n', encoding:'utf8',
    });
    assert.equal(parsed.status, 0, parsed.stderr);
    const result = capture('css/example.html', JSON.parse(parsed.stdout));
    assert.equal(result.status, 'unsupported');
    assert.match(result.reason, /(?:CSP|frameset)/);
    assert.deepEqual(result.cases, []);
  }
});

test("integrity constraints cannot be bypassed by replacing helper scripts", () => {
  const input = document("test_valid_value('color','red')");
  input.scripts[0].attrs.integrity = 'sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=';
  const result = capture('css/example.html', input);
  assert.equal(result.status, 'unsupported');
  assert.match(result.reason, /script integrity/);
  assert.deepEqual(result.cases, []);
  for (const integrity of ['', ' ', null]) {
    input.scripts[0].attrs.integrity = integrity;
    assert.equal(capture('css/example.html', input).status, 'extracted');
  }
});

test("HTML double-escaped script text is deferred instead of truncated", () => {
  for (const script of [
    `<!-- <script>\n// </script>\ntest_valid_value('color','red');\n// -->\n</script>`,
    `<!--<SCRIPT>\ntest_valid_value('color','red');\n</script>\n--></script>`,
  ]) {
    const source = `<!doctype html><script src="/css/support/parsing-testcommon.js"></script><script>${script}`;
    const parsed = spawnSync('python3', [fileURLToPath(new URL('../read-html.py', import.meta.url))], {
      input:JSON.stringify(source)+'\n', encoding:'utf8',
    });
    assert.equal(parsed.status, 0, parsed.stderr);
    const result = capture('css/example.html', JSON.parse(parsed.stdout));
    assert.equal(result.status, 'unsupported');
    assert.match(result.reason, /double-escaped scripts/);
    assert.deepEqual(result.cases, []);
  }
});

test("only recognized standards-mode documents use the context-free fixture", () => {
  const body = `<script src="/css/support/parsing-testcommon.js"></script>
    <script>test_valid_value('width','10','10px')</script>`;
  for (const [prefix, expected] of [
    ['', false], ['<!doctype html>', true], ['<!-- comment -->\n<!DOCTYPE HTML>', true],
    ['text<!doctype html>', false], ['<html><!doctype html>', false],
    ['&nbsp;<!doctype html>', false], ['</p><!doctype html>', false],
  ]) {
    const parsed = spawnSync('python3', [fileURLToPath(new URL('../read-html.py', import.meta.url))], {
      input:JSON.stringify(prefix+body)+'\n', encoding:'utf8',
    });
    assert.equal(parsed.status, 0, parsed.stderr);
    const document = JSON.parse(parsed.stdout);
    assert.equal(document.contextFree, expected, prefix);
    const result = capture('css/example.html', document);
    assert.equal(result.status, 'extracted');
    assert.equal(result.cases[0].contextFree, expected);
  }
});
