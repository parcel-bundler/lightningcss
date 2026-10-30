import fs from "node:fs";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { directory, revision, hash } from "./extract.mjs";
import { Adapter } from "./adapter.mjs";
import { sameFile } from "./paths.mjs";
import { startBrowser } from "./browser.mjs";
import { compareBaseline, validateBaseline, exitCode } from "./baseline.mjs";

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

export async function runCases(report, selectedCases, adapter, browserFactory) {
  let browser;
  const errorText = (error) => error?.stack || String(error) || "Unknown execution error";
  const recordError = (error) => { report.harnessError ??= errorText(error); };
  try {
    browser = await browserFactory?.();
    if (browser) report.browser = { version: browser.version, executable: browser.executable };
    for (const test of selectedCases) {
      for (const mode of report.modes) {
        report.activeCase = { id: test.id, mode };
        const output = await adapter.request({ ...test, mode });
        const result = { ...test, mode, ...output, status: classify(test, output) };
        report.results.push(result);
        if (browser && output.code !== undefined) {
          try {
            result.browser = await browser.check(test, output.code);
          } catch (error) {
            result.browser = { status: "harness-error", error: errorText(error) };
            throw error;
          }
        }
        delete report.activeCase;
      }
    }
  } catch (error) {
    recordError(error);
  } finally {
    try { adapter.close(); } catch (error) { recordError(error); }
    try { await browser?.close(); } catch (error) { recordError(error); }
  }
}

async function main() {
  const args = process.argv.slice(2);
  const option = (name, fallback) =>
    args.includes(name) ? args[args.indexOf(name) + 1] : fallback;
  for (let i = 0; i < args.length; i++) {
    if (["--browser"].includes(args[i])) continue;
    if (
      ["--fixtures", "--output", "--chrome", "--modes", "--filter", "--baseline", "--baseline-corpus"].includes(args[i]) &&
      args[i + 1] &&
      !args[i + 1].startsWith("--")
    ) {
      i++;
      continue;
    }
    throw Error(`Unknown or incomplete option: ${args[i]}`);
  }
  const root = path.dirname(directory);
  const fixturesPath = option("--fixtures", path.join(directory, "fixtures.json"));
  const baselinePath = option("--baseline");
  const baselineCorpus = option("--baseline-corpus");
  const outputPath = option("--output", path.join(directory, "results.json"));
  if (baselineCorpus !== undefined && baselinePath === undefined)
    throw Error("--baseline-corpus requires --baseline");
  if ([fixturesPath, baselinePath].some((file) => file !== undefined && sameFile(file, outputPath)))
    throw Error("Output must not overwrite fixtures or baseline");
  const corpusText = fs.readFileSync(fixturesPath, "utf8");
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
  if (new Set(modes).size !== modes.length) throw Error("Duplicate transform mode");
  const baseline = baselinePath && JSON.parse(fs.readFileSync(baselinePath, "utf8"));
  const context = {
    wptRevision: corpus.wptRevision,
    corpusHash: hash(corpusText),
    loweringTargets: { chrome: 80 << 16 },
    ...(baselineCorpus !== undefined && { baselineCorpus }),
  };
  if (baselinePath !== undefined) validateBaseline(baseline, context);
  // Always compile this checkout. Never load a possibly stale installed Node binary.
  const build = execFileSync("cargo", ["build", "--locked", "--example", "wpt_adapter",
    "--message-format=json-render-diagnostics"], {
    cwd: root,
    encoding: "utf8",
    stdio: ["ignore", "pipe", "inherit"],
    maxBuffer: 20e6,
  });
  const executables = new Set(build.trim().split("\n").filter(Boolean)
    .map((line) => JSON.parse(line))
    .filter((message) => message.reason === "compiler-artifact" &&
      message.target?.name === "wpt_adapter" && message.target.kind.includes("example") &&
      typeof message.executable === "string")
    .map((message) => message.executable));
  if (executables.size !== 1)
    throw Error("Cargo did not report exactly one wpt_adapter executable");
  const adapter = new Adapter([...executables][0]);
  const results = [];
  const report = {
    schemaVersion: 1,
    ...context,
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
    pages: corpus.pages,
    results,
  };
  await runCases(report, selectedCases, adapter, args.includes("--browser")
    ? () => startBrowser(option("--chrome", process.env.WPT_CHROME)) : undefined);
  const count = (values) =>
    Object.fromEntries(
      [...new Set(values)].sort().map((v) => [v, values.filter((x) => x === v).length]),
    );
  report.complete = !report.harnessError && results.length === selectedCases.length * modes.length;
  if (baseline && report.complete) {
    try {
      report.baseline = { path: baselinePath, ...compareBaseline(baseline, report, selectedCases) };
    } catch (error) {
      report.harnessError = error.stack;
      report.complete = false;
    }
  }
  report.summary = {
    cases: selectedCases.length,
    executions: results.length,
    parser: count(results.map((r) => r.status)),
    browser: count(results.map((r) => r.browser?.status ?? "not-run")),
    unadaptedPages: corpus.pages.filter((p) => p.status !== "extracted").length,
    ...(report.baseline && { baseline: {
      expectedFailures: report.baseline.expectedFailures,
      expectedPages: report.baseline.expectedPages,
      unexpected: report.baseline.issues.length,
    } }),
  };
  fs.writeFileSync(outputPath, JSON.stringify(report, null, 2) + "\n");
  console.log(JSON.stringify(report.summary, null, 2));
  console.log(`Full report: ${outputPath}`);
  if (report.harnessError) console.error(report.harnessError);
  if (report.baseline?.issues.length)
    console.error("Unexpected WPT outcomes (first 20):", JSON.stringify(report.baseline.issues.slice(0, 20), null, 2));
  process.exitCode = exitCode(report);
}
if (process.argv[1] === fileURLToPath(import.meta.url))
  main().catch((error) => {
    console.error(error);
    process.exitCode = 2;
  });
