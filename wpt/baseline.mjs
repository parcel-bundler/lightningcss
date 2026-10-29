// Baselines accept exact, reviewed diagnostic outcomes, never infrastructure errors.
export const modes = ["print", "minify", "lower", "lower-minify"];
export const failureStatuses = new Set([
  "robustness-failure", "invalid-typed", "valid-rejected", "harness-error",
]);
export const browserFailures = new Set([
  "invalid-became-valid", "valid-became-invalid", "fallback-changed",
  "shorthand-reset-changed", "computed-difference",
]);

export function infrastructureFailure(result) {
  return result.status === "harness-error" ||
    ["crash", "timeout", "protocol-error"].includes(result.recognition);
}

function canonical(value) {
  if (Array.isArray(value)) return value.map(canonical);
  if (value && typeof value === "object")
    return Object.fromEntries(Object.keys(value).sort()
      .filter((key) => value[key] !== undefined)
      .map((key) => [key, canonical(value[key])]));
  return value;
}
const same = (a, b) => JSON.stringify(canonical(a)) === JSON.stringify(canonical(b));
const key = (id, lane, mode) => JSON.stringify([id, lane, mode]);

export function outcome(result, lane) {
  return lane === "parser"
    ? canonical({ status: result.status, recognition: result.recognition,
        code: result.code, error: result.error })
    : canonical({ status: result.browser?.status, code: result.code, comparison: result.browser });
}

function isFailure(value, lane) {
  return (lane === "parser" ? failureStatuses : browserFailures).has(value?.status);
}

export function validateBaseline(baseline, report) {
  if (baseline.schemaVersion !== 1 || !Array.isArray(baseline.corpora) ||
      !Array.isArray(baseline.failures) || !Array.isArray(baseline.pages) ||
      !baseline.reviews || typeof baseline.reviews !== "object")
    throw Error("Invalid WPT baseline schema");
  if (baseline.wptRevision !== report.wptRevision)
    throw Error("Baseline WPT revision mismatch");
  const corpusNames = new Set();
  const corpusHashes = new Set();
  for (const corpus of baseline.corpora) {
    if (typeof corpus.name !== "string" || typeof corpus.hash !== "string" ||
        corpusNames.has(corpus.name) || corpusHashes.has(corpus.hash))
      throw Error("Invalid or duplicate baseline corpus");
    corpusNames.add(corpus.name);
    corpusHashes.add(corpus.hash);
  }
  const corpus = baseline.corpora.find((corpus) => corpus.hash === report.corpusHash);
  if (!corpus)
    throw Error("Baseline corpus hash mismatch; review extraction changes before updating it");
  if (!same(baseline.loweringTargets, report.loweringTargets))
    throw Error("Baseline lowering targets mismatch");

  const failures = new Map();
  const pages = new Map();
  const allFailures = new Set();
  const allPages = new Set();
  const reviewed = (entry) => {
    if (typeof baseline.reviews[entry.review] !== "string" ||
        !baseline.reviews[entry.review].trim())
      throw Error(`Missing review reason for ${entry.id ?? entry.file}`);
    if (!Array.isArray(entry.corpora) || !entry.corpora.length ||
        new Set(entry.corpora).size !== entry.corpora.length ||
        entry.corpora.some((name) => !corpusNames.has(name)))
      throw Error(`Invalid baseline corpus scope for ${entry.id ?? entry.file}`);
  };
  for (const entry of baseline.failures) {
    reviewed(entry);
    if (typeof entry.id !== "string" || !["parser", "browser"].includes(entry.lane) ||
        !Array.isArray(entry.modes) || !entry.modes.length ||
        entry.modes.some((mode) => !modes.includes(mode)) ||
        !isFailure(entry.outcome, entry.lane) ||
        infrastructureFailure(entry.outcome) ||
        (entry.lane === "parser" && entry.outcome.status === "robustness-failure" &&
          entry.outcome.recognition !== "panic"))
      throw Error(`Invalid baseline failure: ${entry.id}`);
    for (const mode of entry.modes) {
      const id = key(entry.id, entry.lane, mode);
      if (allFailures.has(id)) throw Error(`Duplicate baseline failure: ${id}`);
      allFailures.add(id);
      if (entry.corpora.includes(corpus.name)) failures.set(id, entry);
    }
  }
  for (const entry of baseline.pages) {
    reviewed(entry);
    if (typeof entry.file !== "string" || !["unsupported", "empty"].includes(entry.status) ||
        (entry.status === "unsupported" && typeof entry.reason !== "string"))
      throw Error(`Invalid baseline page: ${entry.file}`);
    if (allPages.has(entry.file)) throw Error(`Duplicate baseline page: ${entry.file}`);
    allPages.add(entry.file);
    if (entry.corpora.includes(corpus.name)) pages.set(entry.file, entry);
  }
  return { failures, pages };
}

export function compareBaseline(baseline, report, selectedCases) {
  const expected = validateBaseline(baseline, report);
  if (!report.complete || report.harnessError)
    throw Error("Cannot compare an incomplete WPT run with a baseline");
  const selected = new Set(selectedCases.map((test) => test.id));
  const observed = new Set();
  const issues = [];
  let expectedFailures = 0;
  let expectedPages = 0;
  for (const result of report.results) {
    for (const lane of report.browser ? ["parser", "browser"] : ["parser"]) {
      const id = key(result.id, lane, result.mode);
      if (!selected.has(result.id) || !report.modes.includes(result.mode) || observed.has(id))
        throw Error(`Unexpected or duplicate WPT execution: ${id}`);
      observed.add(id);
      const entry = expected.failures.get(id);
      const actual = outcome(result, lane);
      if (entry && same(entry.outcome, actual)) {
        expectedFailures++;
      } else if (entry || isFailure(actual, lane)) {
        issues.push({ type: entry ? (isFailure(actual, lane) ? "changed-failure" : "obsolete-expectation")
          : "new-failure", id: result.id, lane, mode: result.mode,
          expected: entry?.outcome, actual });
      }
    }
  }
  // A missing execution must never look like an improvement or an accepted failure.
  const lanes = report.browser ? ["parser", "browser"] : ["parser"];
  for (const id of selected)
    for (const mode of report.modes)
      for (const lane of lanes)
        if (!observed.has(key(id, lane, mode)))
          throw Error(`Missing WPT execution: ${key(id, lane, mode)}`);
  if (!report.filter) {
    for (const entry of new Set(expected.failures.values())) {
      if (lanes.includes(entry.lane) && entry.modes.some((mode) => report.modes.includes(mode)) &&
          !selected.has(entry.id))
        issues.push({ type: "obsolete-expectation", id: entry.id, lane: entry.lane,
          expected: entry.outcome, actual: { status: "case-not-present" } });
    }
  }

  const observedPages = new Set();
  for (const page of report.pages) {
    if (observedPages.has(page.file)) throw Error(`Duplicate WPT page: ${page.file}`);
    observedPages.add(page.file);
    const entry = expected.pages.get(page.file);
    const actual = canonical({ status: page.status, reason: page.reason });
    if (entry && same(actual, { status: entry.status, reason: entry.reason })) {
      expectedPages++;
    } else if (entry || page.status !== "extracted") {
      issues.push({ type: entry ? (page.status === "extracted" ? "obsolete-page" : "changed-page")
        : "new-page", file: page.file, expected: entry && { status: entry.status, reason: entry.reason }, actual });
    }
  }
  for (const [file, entry] of expected.pages)
    if (!observedPages.has(file))
      issues.push({ type: "obsolete-page", file,
        expected: { status: entry.status, reason: entry.reason }, actual: { status: "not-discovered" } });
  return { expectedFailures, expectedPages, issues };
}

export function exitCode(report) {
  if (!report.complete || report.harnessError || report.results.some(infrastructureFailure)) return 2;
  if (report.baseline) return report.baseline.issues.length ? 1 : 0;
  return report.results.some((result) => failureStatuses.has(result.status) ||
    browserFailures.has(result.browser?.status)) ||
    report.pages.some((page) => page.status !== "extracted") ? 1 : 0;
}
