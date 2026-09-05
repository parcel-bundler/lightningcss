import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const parserFailures = new Set([
  "invalid-typed",
  "valid-rejected",
  "robustness-failure",
  "harness-error",
]);
const browserFailures = new Set([
  "invalid-became-valid",
  "valid-became-invalid",
  "fallback-changed",
  "shorthand-reset-changed",
  "computed-difference",
]);
const coverage = new Set(["property-coverage-gap", "value-coverage-gap"]);
const browserNormal = new Set(["pass", "contextual-match", "not-run"]);

export function category(result) {
  if (parserFailures.has(result.status) || browserFailures.has(result.browser?.status))
    return "failure";
  if (coverage.has(result.status)) return "coverage";
  if (result.browser?.status && !browserNormal.has(result.browser.status)) return "unresolved";
  return "other";
}

export function summarize(report) {
  if (!Array.isArray(report.results))
    throw Error("Expected a results.json object with a results array");
  const groups = new Map();
  const classifications = new Map();
  const totals = { failure: 0, coverage: 0, unresolved: 0, other: 0 };
  for (const [index, result] of report.results.entries()) {
    const property = result.property ?? "(unknown property)";
    if (!groups.has(property)) groups.set(property, new Map());
    const cases = groups.get(property);
    const id = result.id ?? `row-${index}`;
    if (!cases.has(id))
      cases.set(id, {
        id,
        file: result.file,
        value: result.value,
        expected: result.expected,
        kind: result.kind,
        scriptLine: result.scriptLine,
        runs: [],
      });
    const run = {
      mode: result.mode,
      status: result.status ?? "unknown",
      recognition: result.recognition,
      code: result.code,
      error: result.error,
      stderr: result.stderr,
      browser: result.browser,
      category: category(result),
    };
    cases.get(id).runs.push(run);
    totals[run.category]++;
    for (const [lane, status] of [
      ["parser", run.status],
      ["browser", run.browser?.status ?? "not-run"],
    ]) {
      const key = `${lane}:${status}`;
      if (!classifications.has(key))
        classifications.set(key, { key, lane, status, executions: 0, cases: new Set() });
      const classification = classifications.get(key);
      classification.executions++;
      classification.cases.add(`${property}:${id}`);
    }
  }
  const { results, pages, summary, ...metadata } = report;
  return {
    metadata,
    totals,
    executions: report.results.length,
    properties: [...groups]
      .map(([property, cases]) => ({ property, cases: [...cases.values()] }))
      .sort((a, b) => a.property.localeCompare(b.property)),
    classifications: [...classifications.values()]
      .map((c) => ({ ...c, cases: c.cases.size }))
      .sort((a, b) => a.key.localeCompare(b.key)),
    pages: (pages ?? []).filter((p) => p.status !== "extracted"),
  };
}

const escapeHTML = (value) =>
  String(value).replace(
    /[&<>"']/g,
    (ch) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[ch],
  );
// Escaping '<' is essential: CSS inputs may contain a literal closing script tag.
const scriptJSON = (value) =>
  JSON.stringify(value).replace(
    /[<>&\u2028\u2029]/g,
    (ch) => `\\u${ch.charCodeAt(0).toString(16).padStart(4, "0")}`,
  );

function interactiveReport() {
  const data = JSON.parse(document.getElementById("report-data").textContent);
  const $ = (id) => document.getElementById(id);
  const number = (n) => n.toLocaleString();
  const make = (tag, text, className) => {
    const node = document.createElement(tag);
    if (text !== undefined) node.textContent = text;
    if (className) node.className = className;
    return node;
  };
  const pre = (value) =>
    make("pre", typeof value === "string" ? value : JSON.stringify(value, null, 2));
  const addField = (parent, label, value) => {
    if (value === undefined) return;
    parent.append(make("h4", label), pre(value));
  };
  const badge = (label, kind) => make("span", label, `badge ${kind ?? ""}`);
  const modes = [
    ...new Set(data.properties.flatMap((p) => p.cases.flatMap((c) => c.runs.map((r) => r.mode)))),
  ];
  for (const mode of modes) $("mode").append(new Option(mode ?? "(unspecified)", mode ?? ""));
  for (const item of data.classifications) {
    $("classification").append(new Option(`${item.lane}: ${item.status}`, item.key));
    const button = make("button", undefined, "classification");
    button.type = "button";
    button.append(
      make("span", item.status),
      make("strong", number(item.executions)),
      make("small", `${number(item.cases)} cases`),
    );
    button.onclick = () => {
      $("classification").value = item.key;
      $("scope").value = "all";
      render();
      $("properties").scrollIntoView({ behavior: "smooth", block: "start" });
    };
    $(item.lane + "-counts").append(button);
  }
  $("metadata-body").append(pre(data.metadata));
  const pageList = $("pages-body");
  $("pages-summary").textContent = `${number(data.pages.length)} unadapted or empty source files`;
  // Also defer the extraction diagnostic list until requested.
  $("pages").addEventListener("toggle", () => {
    if (!$("pages").open || pageList.childElementCount) return;
    for (const page of data.pages) {
      const row = make("div", undefined, "page-row");
      row.append(
        make("code", page.file),
        make("p", `${page.status}: ${page.reason ?? "No helper calls captured"}`),
      );
      pageList.append(row);
    }
  });
  function runMatches(run) {
    const scope = $("scope").value;
    if (scope === "findings" && run.category === "other") return false;
    if (!["findings", "all"].includes(scope) && run.category !== scope) return false;
    if ($("mode").value && run.mode !== $("mode").value) return false;
    const classification = $("classification").value;
    return (
      !classification ||
      classification === `parser:${run.status}` ||
      classification === `browser:${run.browser?.status ?? "not-run"}`
    );
  }
  function caseRow(test, runs) {
    const details = make("details", undefined, "case");
    const summary = make("summary");
    summary.append(
      make("code", test.value ?? "(no input)", "input-preview"),
      badge(`${runs.length} mode${runs.length === 1 ? "" : "s"}`),
      ...[...new Set(runs.map((r) => r.status))].map((s) => badge(s)),
    );
    details.append(summary);
    details.addEventListener("toggle", () => {
      if (!details.open || details.childElementCount > 1) return;
      const body = make("div", undefined, "case-body");
      if (
        test.file &&
        /^[a-f0-9]{40}$/i.test(data.metadata.wptRevision ?? "") &&
        test.file.startsWith("css/") &&
        !test.file.split("/").includes("..")
      ) {
        const link = make("a", test.file);
        link.href = `https://github.com/web-platform-tests/wpt/blob/${data.metadata.wptRevision}/${test.file.split("/").map(encodeURIComponent).join("/")}`;
        if (Number.isInteger(test.scriptLine) && test.scriptLine > 0)
          link.href += `#L${test.scriptLine}`;
        link.target = "_blank";
        link.rel = "noopener noreferrer";
        body.append(link);
      } else body.append(make("code", test.file ?? "Unknown source"));
      body.append(make("p", `${test.kind ?? "Unknown kind"} · ${test.id}`, "muted"));
      addField(body, "Input value", test.value);
      addField(body, "WPT expectation", test.expected);
      for (const run of runs) {
        const panel = make("section", undefined, "run");
        panel.append(
          make("h4", run.mode ?? "(unspecified mode)"),
          badge(run.status, run.category),
          badge(`browser: ${run.browser?.status ?? "not-run"}`),
        );
        addField(panel, "Recognition", run.recognition);
        addField(panel, "Emitted CSS", run.code);
        addField(panel, "Error", run.error);
        addField(panel, "Process diagnostics", run.stderr);
        addField(panel, "Browser comparison", run.browser);
        body.append(panel);
      }
      details.append(body);
    });
    return details;
  }
  function render() {
    const query = $("search").value.toLowerCase().trim();
    const container = $("properties");
    container.replaceChildren();
    let caseCount = 0,
      runCount = 0,
      propertyCount = 0;
    const groups = [];
    for (const group of data.properties) {
      const matches = [];
      for (const test of group.cases) {
        if (
          query &&
          !`${group.property}\n${test.value}\n${test.file}\n${test.id}`
            .toLowerCase()
            .includes(query)
        )
          continue;
        const runs = test.runs.filter(runMatches);
        if (runs.length) matches.push({ test, runs });
      }
      if (matches.length)
        groups.push({
          group,
          matches,
          failures: matches.reduce(
            (n, m) => n + m.runs.filter((r) => r.category === "failure").length,
            0,
          ),
        });
    }
    groups.sort(
      (a, b) => b.failures - a.failures || a.group.property.localeCompare(b.group.property),
    );
    for (const { group, matches, failures } of groups) {
      const executions = matches.reduce((n, m) => n + m.runs.length, 0);
      caseCount += matches.length;
      runCount += executions;
      propertyCount++;
      const details = make("details", undefined, "property");
      details.hidden = propertyCount > 20;
      const summary = make("summary");
      summary.append(
        make("strong", group.property),
        make("span", `${number(matches.length)} cases · ${number(executions)} executions`, "muted"),
      );
      if (failures) summary.append(badge(`${number(failures)} failure signals`, "failure"));
      details.append(summary);
      details.addEventListener("toggle", () => {
        if (!details.open || details.childElementCount > 1) return;
        const body = make("div", undefined, "property-body");
        const more = make("button", "Load next 50 cases", "more");
        let offset = 0;
        const load = () => {
          for (const match of matches.slice(offset, offset + 50))
            body.insertBefore(caseRow(match.test, match.runs), more);
          offset += 50;
          more.hidden = offset >= matches.length;
        };
        more.onclick = load;
        body.append(more);
        details.append(body);
        load();
      });
      container.append(details);
    }
    if (propertyCount > 20) {
      const more = make("button", undefined, "more-properties");
      let shown = 20;
      const label = () => {
        more.textContent = `Show next 20 properties (${number(propertyCount - shown)} remaining)`;
      };
      more.onclick = () => {
        for (const node of [...container.querySelectorAll(".property")].slice(shown, shown + 20))
          node.hidden = false;
        shown = Math.min(propertyCount, shown + 20);
        more.hidden = shown === propertyCount;
        label();
      };
      label();
      container.append(more);
    }
    $("matched").textContent =
      `${number(caseCount)} cases · ${number(runCount)} executions · ${number(propertyCount)} properties`;
    if (!groups.length) container.append(make("p", "No results match these filters.", "empty"));
  }
  let timer;
  $("search").addEventListener("input", () => {
    clearTimeout(timer);
    timer = setTimeout(render, 150);
  });
  for (const id of ["scope", "mode", "classification"]) $(id).addEventListener("change", render);
  $("reset").onclick = () => {
    $("search").value = "";
    $("scope").value = "findings";
    $("mode").value = "";
    $("classification").value = "";
    render();
  };
  $("collapse").onclick = () => {
    for (const node of document.querySelectorAll("#properties details[open]")) node.open = false;
  };
  render();
}

export function renderReport(report, title = "Lightning CSS · WPT report") {
  const data = summarize(report);
  const cases = data.properties.reduce((n, p) => n + p.cases.length, 0);
  const fmt = (n) => n.toLocaleString("en-US");
  return `<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>${escapeHTML(title)}</title>
<style>
:root{font-family:system-ui,-apple-system,sans-serif;color:#202b39;background:#f5f7fa;font-size:15px;color-scheme:light}
*{box-sizing:border-box}body{margin:0}main{max-width:1180px;margin:auto;padding:36px 24px 64px}h1{font-size:30px;letter-spacing:-.8px;margin:6px 0 12px}h2{font-size:18px;margin:0 0 14px}h4{font-size:13px;margin:16px 0 6px}.eyebrow{font-size:12px;font-weight:700;letter-spacing:1.5px;color:#436883}.muted,small{color:#627184}p{line-height:1.6}header>p{max-width:850px}
.stats{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin:24px 0}.stat,.panel{background:white;border:1px solid #dce2e9;border-radius:10px;padding:18px}.stat strong{display:block;font-size:28px}.stat span{font-size:13px;color:#627184}.lanes{display:grid;grid-template-columns:1fr 1fr;gap:16px;margin:20px 0}.classification{width:100%;display:grid;grid-template-columns:1fr auto 90px;text-align:left;align-items:center;border:0;border-top:1px solid #edf0f4;border-radius:0;padding:9px 0;font-size:12px;gap:8px}.classification small{text-align:right}.classification:hover{background:#f1f6fb}
button,select,input{font:inherit;border:1px solid #ccd5df;border-radius:6px;background:white;padding:8px 10px;color:inherit}button{cursor:pointer}button:hover{background:#eef3f8}button:focus-visible,summary:focus-visible,a:focus-visible{outline:3px solid #4783bc;outline-offset:2px}.filters{display:flex;flex-wrap:wrap;gap:12px;align-items:end;margin:16px 0}.filters label{font-size:12px;display:flex;flex-direction:column;gap:5px}.filters label:first-child{flex:1;min-width:220px}#matched{font-size:13px}.property{background:white;border:1px solid #dce2e9;border-radius:8px;margin:8px 0;overflow:hidden}summary{cursor:pointer;padding:14px;overflow-wrap:anywhere}summary>strong{display:inline-block;min-width:210px}summary>.muted{font-size:12px;margin-right:10px}.badge{display:inline-block;font-size:11px;border-radius:4px;padding:3px 6px;background:#edf1f6;color:#526177;margin:2px 5px 2px 0}.failure{background:#fce8e5;color:#a43525}.coverage{background:#fff1d6;color:#88610b}.unresolved{background:#eae8fa;color:#615496}.property-body{border-top:1px solid #e0e6ed;padding:6px 14px 14px}.case{border-bottom:1px solid #e5eaf0}.case summary{padding:10px 0}.input-preview{display:inline-block;max-width:100%;vertical-align:middle;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;margin-right:8px}.case-body{padding:6px 10px 20px}.case-body>p{font-size:12px;overflow-wrap:anywhere}.run{border-left:3px solid #dce5ef;padding:0 14px;margin:18px 0}pre{background:#f5f7fa;border:1px solid #e4e9ef;border-radius:5px;padding:10px;white-space:pre-wrap;overflow-wrap:anywhere;font-size:12px;max-height:420px;overflow:auto}code{font-family:ui-monospace,SFMono-Regular,Consolas,monospace;font-size:12px}a{color:#2566a2}.more{margin-top:14px}.aux{margin:18px 0;border-top:1px solid #dce2e9}.page-row{padding:8px 14px;border-bottom:1px solid #e0e6ed}.page-row p{margin:4px 0;font-size:12px}.warning{border:1px solid #d7a754;background:#fff3dc;padding:14px;border-radius:8px}.empty{padding:24px;text-align:center}noscript{display:block;padding:20px;background:#fff3dc}
@media(max-width:750px){main{padding:22px 12px}.lanes{grid-template-columns:1fr}.stats{grid-template-columns:1fr 1fr}summary>strong{min-width:0;display:block}.classification{grid-template-columns:1fr auto 70px}.filters select{max-width:100%}}
</style></head><body><main>
<header><div class="eyebrow">LIGHTNING CSS / WEB PLATFORM TESTS</div><h1>WPT results</h1>
<p class="muted">${fmt(cases)} distinct cases across ${fmt(data.executions)} case × mode executions. Counts below describe this results file, not the entire WPT suite. Classification totals can overlap between parser and browser.</p></header>
${report.complete === false || report.harnessError ? '<p class="warning">Incomplete run — results below are partial. Expand run metadata for the harness error and configuration.</p>' : ""}
<div class="stats"><div class="stat"><strong>${fmt(data.totals.failure)}</strong><span>Failure signals / executions</span></div><div class="stat"><strong>${fmt(data.totals.coverage)}</strong><span>Coverage gaps / executions</span></div><div class="stat"><strong>${fmt(data.totals.unresolved)}</strong><span>Unresolved browser checks</span></div><div class="stat"><strong>${fmt(data.properties.length)}</strong><span>Properties in this run</span></div></div>
<p class="muted">Failure signals include parser validation candidates, not only confirmed behavior changes. Browser baseline failures are unresolved checks. “Not run” is not a browser pass. A contextual match applies only to the fixture tested.</p>
<div class="lanes"><section class="panel"><h2>Parser classifications</h2><div id="parser-counts"></div></section><section class="panel"><h2>Browser classifications</h2><div id="browser-counts"></div></section></div>
<h2>Explore by property</h2><p class="muted">Click a classification above to filter. Properties and cases stay collapsed until requested; properties appear 20 at a time and large groups load 50 cases at a time.</p>
<div class="filters"><label>Search property, input, source or case ID<input id="search" type="search" placeholder="e.g. color, calc(infinity), css-grid"></label><label>Show<select id="scope"><option value="findings">All findings</option><option value="failure">Failure signals</option><option value="coverage">Coverage gaps</option><option value="unresolved">Unresolved browser checks</option><option value="all">All results</option></select></label><label>Mode<select id="mode"><option value="">All modes</option></select></label><label>Classification<select id="classification"><option value="">All classifications</option></select></label><button id="reset">Reset</button><button id="collapse">Collapse all</button></div>
<p id="matched" aria-live="polite"></p><div id="properties"></div>
<details class="aux" id="pages"><summary id="pages-summary">Unadapted source files</summary><div id="pages-body"></div></details>
<details class="aux" id="metadata"><summary>Run metadata and configuration</summary><div id="metadata-body"></div></details>
<noscript>Enable JavaScript to browse classifications and property details. This report is self-contained and makes no network requests.</noscript>
</main><script type="application/json" id="report-data">${scriptJSON(data)}</script><script>(${interactiveReport.toString()})();</script></body></html>`;
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  try {
    const args = process.argv.slice(2);
    const input =
      args[0] && args[0] !== "--output"
        ? args.shift()
        : path.join(path.dirname(fileURLToPath(import.meta.url)), "results.json");
    if (args.length && (args.length !== 2 || args[0] !== "--output" || !args[1]))
      throw Error("Usage: node wpt/report.mjs [results.json] [--output report.html]");
    const output =
      args[1] ??
      path.join(path.dirname(input), `${path.basename(input, path.extname(input))}.html`);
    if (path.resolve(input) === path.resolve(output))
      throw Error("Output must not overwrite the input JSON");
    const report = JSON.parse(fs.readFileSync(input, "utf8"));
    fs.writeFileSync(output, renderReport(report));
    console.log(`HTML report: ${path.resolve(output)}`);
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}
