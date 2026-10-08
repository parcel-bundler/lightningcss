import fs from "node:fs";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { directory, revision, hash } from "./extract.mjs";
import { Adapter } from "./adapter.mjs";
import { startBrowser } from "./browser.mjs";

export function classify(test, result) {
  if (["panic", "crash", "timeout", "protocol-error"].includes(result.recognition))
    return "robustness-failure";
  if (test.kind === "invalid") {
    if (result.recognition === "typed") return "invalid-typed";
    return result.recognition === "error" ? "rejected" : "preserved-invalid";
  }
  return (
    {
      typed: "recognized",
      "css-wide": "generic",
      deferred: "deferred",
      "custom-property": "custom-property",
      unparsed: "value-coverage-gap",
      "unknown-property": "property-coverage-gap",
      error: "valid-rejected",
    }[result.recognition] ?? "harness-error"
  );
}
export const failureStatuses = new Set([
  "robustness-failure",
  "invalid-typed",
  "valid-rejected",
  "harness-error",
]);
export const browserFailures = new Set([
  "invalid-became-valid",
  "valid-became-invalid",
  "fallback-changed",
  "shorthand-reset-changed",
  "computed-difference",
]);

async function main() {
  const args = process.argv.slice(2);
  const option = (name, fallback) =>
    args.includes(name) ? args[args.indexOf(name) + 1] : fallback;
  for (let i = 0; i < args.length; i++) {
    if (["--browser"].includes(args[i])) continue;
    if (
      ["--fixtures", "--output", "--chrome", "--modes", "--filter"].includes(args[i]) &&
      args[i + 1] &&
      !args[i + 1].startsWith("--")
    ) {
      i++;
      continue;
    }
    throw Error(`Unknown or incomplete option: ${args[i]}`);
  }
  const root = path.dirname(directory);
  const corpusText = fs.readFileSync(
    option("--fixtures", path.join(directory, "fixtures.json")),
    "utf8",
  );
  const corpus = JSON.parse(corpusText);
  if (corpus.schemaVersion !== revision.schemaVersion || corpus.wptRevision !== revision.commit)
    throw Error("Fixture schema/revision mismatch");
  const filter = option("--filter", "");
  const selectedCases = corpus.cases.filter((c) =>
    [c.id, c.property, c.value].some((v) => v.includes(filter)),
  );
  if (!selectedCases.length) throw Error("No matching cases");
  const modes = option("--modes", "print,minify,lower,lower-minify").split(",");
  if (modes.some((m) => !["print", "minify", "lower", "lower-minify"].includes(m)))
    throw Error("Unknown transform mode");
  // Always compile this checkout. Never load a possibly stale installed Node binary.
  execFileSync("cargo", ["build", "--locked", "--example", "wpt_adapter"], {
    cwd: root,
    stdio: "inherit",
  });
  const metadata = JSON.parse(
    execFileSync("cargo", ["metadata", "--locked", "--format-version", "1", "--no-deps"], {
      cwd: root,
      encoding: "utf8",
    }),
  );
  const adapter = new Adapter(
    path.join(
      metadata.target_directory,
      "debug",
      "examples",
      `wpt_adapter${process.platform === "win32" ? ".exe" : ""}`,
    ),
  );
  let browser;
  const results = [];
  const report = {
    schemaVersion: 1,
    wptRevision: corpus.wptRevision,
    corpusHash: hash(corpusText),
    lightningcssRevision: execFileSync("git", ["rev-parse", "HEAD"], {
      cwd: root,
      encoding: "utf8",
    }).trim(),
    workingTreeStatus: execFileSync("git", ["status", "--porcelain"], {
      cwd: root,
      encoding: "utf8",
    }),
    trackedDiffHash: hash(execFileSync("git", ["diff", "HEAD"], { cwd: root })),
    adapterHash: hash(fs.readFileSync(path.join(root, "examples/wpt_adapter.rs"))),
    node: process.version,
    platform: `${process.platform}-${process.arch}`,
    filter,
    modes,
    loweringTargets: { chrome: 80 << 16 },
    pages: corpus.pages,
    results,
  };
  try {
    if (args.includes("--browser")) {
      browser = await startBrowser(option("--chrome", process.env.WPT_CHROME));
      report.browser = { version: browser.version, executable: browser.executable };
    }
    for (const test of selectedCases) {
      for (const mode of modes) {
        const output = await adapter.request({ ...test, mode });
        const result = { ...test, mode, ...output, status: classify(test, output) };
        if (browser && output.code !== undefined)
          result.browser = await browser.check(test, output.code);
        results.push(result);
      }
    }
  } catch (error) {
    report.harnessError = error.stack;
  } finally {
    adapter.close();
    await browser?.close();
  }
  const count = (values) =>
    Object.fromEntries(
      [...new Set(values)].sort().map((v) => [v, values.filter((x) => x === v).length]),
    );
  report.complete = !report.harnessError && results.length === selectedCases.length * modes.length;
  report.summary = {
    cases: selectedCases.length,
    executions: results.length,
    parser: count(results.map((r) => r.status)),
    browser: count(results.map((r) => r.browser?.status ?? "not-run")),
    unadaptedPages: corpus.pages.filter((p) => p.status !== "extracted").length,
  };
  const outputPath = option("--output", path.join(directory, "results.json"));
  fs.writeFileSync(outputPath, JSON.stringify(report, null, 2) + "\n");
  console.log(JSON.stringify(report.summary, null, 2));
  console.log(`Full report: ${outputPath}`);
  // Until failures have been reviewed, do not silently baseline them away.
  if (report.harnessError) {
    console.error(report.harnessError);
    process.exitCode = 2;
  } else if (
    results.some((r) => failureStatuses.has(r.status) || browserFailures.has(r.browser?.status)) ||
    report.summary.unadaptedPages
  )
    process.exitCode = 1;
}
if (process.argv[1] === fileURLToPath(import.meta.url))
  main().catch((error) => {
    console.error(error);
    process.exitCode = 2;
  });
