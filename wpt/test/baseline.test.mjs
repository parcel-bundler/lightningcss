import { test } from "node:test";
import assert from "node:assert/strict";
import { compareBaseline, exitCode, outcome } from "../baseline.mjs";

function fixture() {
  const results = [
    { id: "invalid-width", mode: "print", status: "invalid-typed", recognition: "typed", code: "width: 1px" },
    { id: "valid-color", mode: "print", status: "recognized", recognition: "typed", code: "color: red" },
  ];
  const report = {
    complete: true, wptRevision: "wpt", corpusHash: "corpus", loweringTargets: { chrome: 80 << 16 },
    modes: ["print"], filter: "", results,
    pages: [{ file: "unsupported.html", status: "unsupported", reason: "document is not defined" },
      { file: "extracted.html", status: "extracted" }],
  };
  const baseline = {
    schemaVersion: 1, wptRevision: "wpt", loweringTargets: { chrome: 80 << 16 },
    corpora: [{ name: "test", hash: "corpus" }], reviews: { reviewed: "Reviewed exact input and output" },
    failures: [{ id: "invalid-width", lane: "parser", modes: ["print"], corpora: ["test"],
      outcome: { status: "invalid-typed", recognition: "typed", code: "width: 1px" }, review: "reviewed" }],
    pages: [{ file: "unsupported.html", status: "unsupported", reason: "document is not defined", review: "reviewed", corpora: ["test"] }],
  };
  const check = () => compareBaseline(baseline, report, report.results);
  return { baseline, report, check };
}

test("exact reviewed failures pass while raw diagnostics still fail", () => {
  const { report, check } = fixture();
  assert.equal(exitCode(report), 1);
  report.baseline = check();
  assert.deepEqual(report.baseline, { expectedFailures: 1, expectedPages: 1, issues: [] });
  assert.equal(exitCode(report), 0);
  assert.equal(report.results[0].status, "invalid-typed");
});

test("a new failure cannot replace a fixed one just because counts match", () => {
  const { report, check } = fixture();
  report.results[0].status = "rejected";
  report.results[0].recognition = "error";
  report.results[1].status = "valid-rejected";
  report.results[1].recognition = "error";
  report.results[1].error = "new parse error";
  report.baseline = check();
  assert.deepEqual(report.baseline.issues.map((issue) => issue.type), ["obsolete-expectation", "new-failure"]);
  assert.equal(exitCode(report), 1);
});

test("changed output, error, recognition or failure kind needs review", () => {
  for (const changes of [
    { code: "width: 2px" }, { error: "new error" }, { recognition: "panic" },
    { status: "robustness-failure", recognition: "panic", error: "new panic" },
  ]) {
    const { report, check } = fixture();
    Object.assign(report.results[0], changes);
    assert.equal(check().issues[0].type, "changed-failure");
  }
});

test("a fixed failure requires removal of its obsolete expectation", () => {
  const { report, check } = fixture();
  report.results[0].status = "rejected";
  report.baseline = check();
  assert.equal(report.baseline.issues[0].type, "obsolete-expectation");
  assert.equal(exitCode(report), 1);
});

test("filters and mode subsets compare only executions that were requested", () => {
  const { baseline, report } = fixture();
  baseline.failures[0].modes.push("minify");
  baseline.failures.push({ ...baseline.failures[0], id: "outside-filter" });
  report.filter = "width";
  report.results = [report.results[0]];
  assert.equal(compareBaseline(baseline, report, report.results).issues.length, 0);
  report.results[0].mode = "minify";
  report.modes = ["minify"];
  assert.equal(compareBaseline(baseline, report, report.results).issues.length, 0);
  baseline.failures[0].modes = ["print"];
  assert.equal(compareBaseline(baseline, report, report.results).issues[0].type, "new-failure");
});

test("new, changed and newly adapted pages are detected independently", () => {
  for (const [pages, type] of [
    [[{ file: "new.html", status: "empty" }], "new-page"],
    [[{ file: "unsupported.html", status: "unsupported", reason: "new dependency" }], "changed-page"],
    [[{ file: "unsupported.html", status: "extracted" }], "obsolete-page"],
  ]) {
    const { report, check } = fixture();
    report.pages = pages;
    assert.equal(check().issues[0].type, type);
  }
});

test("browser failures require their own exact expectations", () => {
  const { baseline, report, check } = fixture();
  const result = report.results[1];
  report.browser = { version: "test-browser" };
  report.results[0].browser = { status: "pass" };
  result.browser = { status: "computed-difference", code: "browser serialization",
    computed: [{ property: "color", original: "red", transformed: "blue" }] };
  assert.equal(check().issues[0].lane, "browser");
  baseline.failures.push({ id: result.id, lane: "browser", modes: ["print"], corpora: ["test"],
    outcome: outcome(result, "browser"), review: "reviewed" });
  assert.equal(check().issues.length, 0);
  assert.equal(baseline.failures.at(-1).outcome.code, "color: red");
  assert.equal(baseline.failures.at(-1).outcome.comparison.code, "browser serialization");
  result.browser.code = "changed browser serialization";
  assert.equal(check().issues[0].type, "changed-failure");
  result.browser.code = "browser serialization";
  result.browser.computed[0].transformed = "green";
  assert.equal(check().issues[0].type, "changed-failure");
  result.browser = { status: "pass" };
  assert.equal(check().issues[0].type, "obsolete-expectation");
  delete report.browser;
  assert.equal(check().issues.length, 0);
});

test("baseline mismatches, wildcard modes, missing reviews and duplicate entries fail closed", () => {
  for (const mutate of [
    (b) => { b.schemaVersion = 2; },
    (b) => { b.wptRevision = "different"; },
    (b) => { b.corpora[0].hash = "different"; },
    (b) => { b.loweringTargets.chrome = 1; },
    (b) => { b.failures[0].modes = ["*"]; },
    (b) => { b.failures[0].review = "missing"; },
    (b) => { b.failures[0].corpora = ["unknown-corpus"]; },
    (b) => { b.failures.push(b.failures[0]); },
    (b) => { b.pages.push(b.pages[0]); },
  ]) {
    const { baseline, check } = fixture();
    mutate(baseline);
    assert.throws(check);
  }
});

test("removed cases and pages are obsolete, while other corpus scopes are ignored", () => {
  const { baseline, report, check } = fixture();
  baseline.failures.push({ ...baseline.failures[0], id: "removed-case" });
  report.pages = report.pages.filter((page) => page.file !== "unsupported.html");
  assert.deepEqual(check().issues.map((issue) => issue.type), ["obsolete-expectation", "obsolete-page"]);
  baseline.corpora.push({ name: "other", hash: "other-hash" });
  baseline.failures.at(-1).corpora = ["other"];
  baseline.pages[0].corpora = ["other"];
  assert.equal(check().issues.length, 0);
});

test("only reviewed panics can be baselined; infrastructure failures always exit 2", () => {
  const { baseline, report, check } = fixture();
  const result = report.results[0];
  Object.assign(result, { status: "robustness-failure", recognition: "panic", error: "known panic" });
  delete result.code;
  baseline.failures[0].outcome = outcome(result, "parser");
  report.baseline = check();
  assert.equal(exitCode(report), 0);
  for (const recognition of ["timeout", "crash", "protocol-error"]) {
    result.recognition = recognition;
    assert.equal(exitCode(report), 2);
    baseline.failures[0].outcome = outcome(result, "parser");
    assert.throws(check, /Invalid baseline failure/);
  }
  result.status = "harness-error";
  result.recognition = "unknown";
  assert.equal(exitCode(report), 2);
  baseline.failures[0].outcome = outcome(result, "parser");
  assert.throws(check, /Invalid baseline failure/);
});

test("incomplete runs and missing or duplicate executions cannot pass", () => {
  const { report, check } = fixture();
  report.complete = false;
  assert.throws(check, /incomplete/);
  assert.equal(exitCode(report), 2);
  report.complete = true;
  report.harnessError = "browser crashed";
  assert.throws(check, /incomplete/);
  delete report.harnessError;
  report.modes.push("minify");
  assert.throws(check, /Missing WPT execution/);
  report.modes.pop();
  report.results.push(report.results[0]);
  assert.throws(check, /duplicate WPT execution/);
});
