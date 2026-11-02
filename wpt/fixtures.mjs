// Validate the fields interpreted by the runner before building or executing CSS.
export function validateCorpus(corpus) {
  const invalid = (message) => { throw Error(`Invalid WPT fixture: ${message}`); };
  if (!Array.isArray(corpus.cases) || !Array.isArray(corpus.pages))
    invalid("cases and pages must be arrays");
  const fields = new Set(["id", "identity", "file", "kind", "property", "value", "expected", "scriptLine", "contextFree"]);
  const ids = new Set();
  for (const test of corpus.cases) {
    if (!test || typeof test !== "object" || Array.isArray(test)) invalid("case must be an object");
    if (Object.keys(test).some(name => !fields.has(name))) invalid("unknown case field");
    if (typeof test.id !== "string" || !test.id || ids.has(test.id)) invalid("missing or duplicate case ID");
    ids.add(test.id);
    if (!["valid", "invalid", "shorthand"].includes(test.kind)) invalid(`unknown case kind: ${test.kind}`);
    if (typeof test.property !== "string" || typeof test.value !== "string") invalid("property/value must be strings");
    if (test.kind === "valid" && typeof test.expected !== "string" &&
        !(Array.isArray(test.expected) && test.expected.every(value => typeof value === "string")))
      invalid("valid case requires string expectations");
    if (test.kind === "shorthand" && (!test.expected || typeof test.expected !== "object" ||
        Array.isArray(test.expected) || Object.values(test.expected).some(value => typeof value !== "string")))
      invalid("shorthand case requires longhand expectations");
    if (test.kind === "invalid" && Object.hasOwn(test, "expected")) invalid("invalid case has an expectation");
    if (test.contextFree !== undefined && typeof test.contextFree !== "boolean") invalid("contextFree must be boolean");
    for (const name of ["file", "identity"])
      if (test[name] !== undefined && typeof test[name] !== "string") invalid(`${name} must be a string`);
    if (test.scriptLine !== undefined && (!Number.isInteger(test.scriptLine) || test.scriptLine < 1))
      invalid("scriptLine must be a positive integer");
  }
  const files = new Set();
  for (const page of corpus.pages) {
    if (!page || typeof page !== "object" || typeof page.file !== "string" || !page.file || files.has(page.file))
      invalid("missing or duplicate source page");
    files.add(page.file);
    if (!["extracted", "unsupported", "empty"].includes(page.status)) invalid("unknown source page status");
    if (page.status === "unsupported" && (typeof page.reason !== "string" || !page.reason))
      invalid("unsupported page requires a reason");
    if (page.caseCount !== undefined && (!Number.isInteger(page.caseCount) || page.caseCount < 0))
      invalid("invalid page case count");
  }
}
