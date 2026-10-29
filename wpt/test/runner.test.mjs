import { test } from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { runCases } from "../run.mjs";
import { exitCode } from "../baseline.mjs";
import { revision } from "../extract.mjs";

const cases = [{ id: "color-red", property: "color", value: "red", kind: "valid" }];
const report = () => ({ modes: ["print"], results: [], pages: [] });

test("CLI output cannot overwrite its fixture or baseline inputs", () => {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), "wpt-output-test-"));
  try {
    const fixtures = path.join(directory, "fixtures.json");
    const baseline = path.join(directory, "baseline.json");
    const content = JSON.stringify({ schemaVersion: revision.schemaVersion,
      wptRevision: revision.commit, cases, pages: [] });
    fs.writeFileSync(fixtures, content);
    fs.writeFileSync(baseline, "{}");
    for (const output of [fixtures, baseline]) {
      const result = spawnSync(process.execPath, [fileURLToPath(new URL("../run.mjs", import.meta.url)),
        "--fixtures", fixtures, "--baseline", baseline, "--output", output], { encoding: "utf8" });
      assert.equal(result.status, 2);
      assert.match(result.stderr, /must not overwrite/);
      assert.equal(fs.readFileSync(fixtures, "utf8"), content);
      assert.equal(fs.readFileSync(baseline, "utf8"), "{}");
    }
  } finally { fs.rmSync(directory, { recursive: true, force: true }); }
});

test("falsey JSON baselines cannot silently disable checking in the CLI", () => {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), "wpt-baseline-test-"));
  try {
    const fixtures = path.join(directory, "fixtures.json");
    const baseline = path.join(directory, "baseline.json");
    fs.writeFileSync(fixtures, JSON.stringify({ schemaVersion: revision.schemaVersion,
      wptRevision: revision.commit, cases, pages: [] }));
    for (const invalid of ["null", "false", "0", '""']) {
      fs.writeFileSync(baseline, invalid);
      const result = spawnSync(process.execPath, [fileURLToPath(new URL("../run.mjs", import.meta.url)),
        "--fixtures", fixtures, "--baseline", baseline, "--output", path.join(directory, "results.json")],
        { encoding: "utf8" });
      assert.equal(result.status, 2, result.stderr);
      assert.match(result.stderr, /Invalid WPT baseline schema/);
    }
  } finally { fs.rmSync(directory, { recursive: true, force: true }); }
});

test("browser errors preserve the responsible case and its parser output", async () => {
  const result = report();
  let adapterClosed = false, browserClosed = false;
  await runCases(result, cases, {
    request: async () => ({ recognition: "typed", code: "color:red" }),
    close() { adapterClosed = true; },
  }, async () => ({
    check: async () => { throw Error("page crashed"); },
    close: async () => { browserClosed = true; },
  }));
  assert.match(result.harnessError, /page crashed/);
  assert.equal(result.results[0].id, "color-red");
  assert.equal(result.results[0].code, "color:red");
  assert.equal(result.results[0].browser.status, "harness-error");
  assert.deepEqual(result.activeCase, { id: "color-red", mode: "print" });
  assert.ok(adapterClosed && browserClosed);
  assert.equal(exitCode(result), 2);
});

test("cleanup errors are reported without discarding completed results", async () => {
  const result = report();
  await runCases(result, cases, {
    request: async () => ({ recognition: "typed", code: "color:red" }), close() {},
  }, async () => ({ check: async () => ({ status: "pass" }),
    close: async () => { throw Error("browser close failed"); } }));
  assert.match(result.harnessError, /browser close failed/);
  assert.equal(result.results.length, 1);
  assert.equal(result.activeCase, undefined);
  assert.equal(exitCode(result), 2);
});

test("an empty thrown cleanup value cannot make the run appear successful", async () => {
  const result = report();
  await runCases(result, cases, {
    request: async () => ({ recognition: "typed", code: "color:red" }),
    close() { throw ""; },
  });
  result.complete = !result.harnessError && result.results.length === cases.length;
  assert.ok(result.harnessError);
  assert.equal(exitCode(result), 2);
});

test("adapter and cleanup failures retain the first error and still close the browser", async () => {
  const result = report();
  let browserClosed = false;
  await runCases(result, cases, {
    request: async () => { throw Error("write failed"); },
    close() { throw Error("adapter cleanup failed"); },
  }, async () => ({ close: async () => { browserClosed = true; } }));
  assert.match(result.harnessError, /write failed/);
  assert.ok(browserClosed);
  assert.deepEqual(result.activeCase, { id: "color-red", mode: "print" });
});

test("CLI runs Cargo's reported artifact and rejects missing or ambiguous executables", {
  skip: process.platform === "win32", // Fake executable scripts use POSIX shebangs.
}, () => {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), "wpt-artifact-test-"));
  try {
    const target = path.join(directory, "target");
    const executable = path.join(target, "configured-target", "debug", "examples", "wpt_adapter");
    const stale = path.join(target, "debug", "examples", "wpt_adapter");
    for (const [file, code] of [[executable, "color:red"], [stale, "color:blue"]]) {
      fs.mkdirSync(path.dirname(file), {recursive: true});
      fs.writeFileSync(file, `#!${process.execPath}
        require('readline').createInterface({input:process.stdin}).on('line',line=>{
          const input=JSON.parse(line);
          console.log(JSON.stringify({requestId:input.requestId,recognition:'typed',code:${JSON.stringify(code)}}));
        });`, {mode: 0o755});
    }
    const fixtures = path.join(directory, "fixtures.json");
    const output = path.join(directory, "results.json");
    fs.writeFileSync(fixtures, JSON.stringify({schemaVersion: revision.schemaVersion,
      wptRevision: revision.commit, cases, pages: []}));
    const cargo = path.join(directory, "cargo");
    for (const executables of [[executable], [], [executable, executable + "-other"]]) {
      fs.writeFileSync(cargo, `#!${process.execPath}
        if(process.argv[2]==='metadata')console.log(JSON.stringify({target_directory:${JSON.stringify(target)}}));
        else {
          if(!process.argv.includes('--message-format=json-render-diagnostics'))process.exit(3);
          for(const executable of ${JSON.stringify(executables)})
            console.log(JSON.stringify({reason:'compiler-artifact',target:{name:'wpt_adapter',kind:['example']},executable}));
          console.log(JSON.stringify({reason:'build-finished',success:true}));
        }`, {mode: 0o755});
      fs.rmSync(output, {force: true});
      const result = spawnSync(process.execPath, [fileURLToPath(new URL("../run.mjs", import.meta.url)),
        "--fixtures", fixtures, "--modes", "print", "--output", output],
        {encoding: "utf8", env: {...process.env, PATH: directory + path.delimiter + process.env.PATH}});
      if (executables.length === 1) {
        assert.equal(result.status, 0, result.stderr);
        assert.equal(JSON.parse(fs.readFileSync(output, "utf8")).results[0].code, "color:red");
      } else {
        assert.equal(result.status, 2, result.stderr);
        assert.match(result.stderr, /exactly one wpt_adapter executable/);
        assert.equal(fs.existsSync(output), false);
      }
    }
  } finally { fs.rmSync(directory, {recursive: true, force: true}); }
});
