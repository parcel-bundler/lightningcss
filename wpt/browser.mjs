import puppeteer from "puppeteer";

// Phase 2 adapts only context-free parsing helpers. Fixture-dependent computed
// tests will need the WPT server and are explicitly skipped here.
export async function startBrowser(executablePath) {
  const browser = await puppeteer.launch({
    headless: true,
    ...(executablePath ? { executablePath } : {}),
  });
  const page = await browser.newPage();
  await page.setContent('<!doctype html><meta charset="utf-8"><body>');
  return {
    version: await browser.version(),
    executable: browser.process()?.spawnfile,
    async check(test, output) {
      if (!test.contextFree) return { status: "fixture-required" };
      if (/(?:url\(|var\(|env\()/i.test(test.value)) return { status: "context-required" };
      return page.evaluate(
        ({ test, output }) => {
          const original = document.createElement("div").style;
          const transformed = document.createElement("div").style;
          const property = test.property;
          original.setProperty(property, test.value);
          const read = original.getPropertyValue(property);
          const matches = (actual, expected) =>
            Array.isArray(expected) ? expected.includes(actual) : actual === expected;
          if (test.kind === "invalid") {
            if (read !== "") return { status: "baseline-failure", actual: read, expected: "" };
          } else if (!read) {
            return { status: "baseline-failure", actual: "", expected: test.expected };
          } else if (test.kind === "valid" && !matches(read, test.expected)) {
            return { status: "baseline-failure", actual: read, expected: test.expected };
          } else if (test.kind === "shorthand") {
            const differences = Object.entries(test.expected).filter(
              ([name, value]) => original.getPropertyValue(name) !== value,
            );
            if (differences.length)
              return {
                status: "baseline-failure",
                differences: differences.map(([name, expected]) => ({
                  property: name,
                  expected,
                  actual: original.getPropertyValue(name),
                })),
              };
          }
          if (test.kind === "shorthand") {
            const check = document.createElement("div").style;
            check.setProperty(property, test.value);
            for (const name of Object.keys(test.expected)) check.removeProperty(name);
            if (check.length)
              return {
                status: "baseline-failure",
                reason: "shorthand set unrelated longhands",
                actual: check.cssText,
              };
          }
          if (test.kind === "valid") {
            const check = document.createElement("div").style;
            check.setProperty(property, read);
            if (check.getPropertyValue(property) !== read)
              return { status: "baseline-failure", reason: "serialization did not round-trip" };
          }
          transformed.cssText = output;
          if (test.kind === "invalid") {
            if (transformed.length)
              return { status: "invalid-became-valid", code: transformed.cssText };
            // A valid declaration preceding invalid input must keep winning.
            original.cssText = "";
            original.setProperty(property, "initial");
            if (!original.length) return { status: "baseline-unsupported-property" };
            const fallback = original.cssText;
            original.setProperty(property, test.value);
            transformed.cssText = fallback + ";" + output;
            return original.cssText === transformed.cssText
              ? { status: "pass" }
              : {
                  status: "fallback-changed",
                  original: original.cssText,
                  transformed: transformed.cssText,
                };
          }
          if (!transformed.length) return { status: "valid-became-invalid" };
          const names = new Set([
            ...Array.from(original),
            ...Array.from(transformed),
            ...(test.kind === "shorthand" ? Object.keys(test.expected) : [property]),
          ]);
          const differences = [...names]
            .map((name) => ({
              property: name,
              original: original.getPropertyValue(name),
              transformed: transformed.getPropertyValue(name),
            }))
            .filter((x) => x.original !== x.transformed);
          if (test.kind === "shorthand") {
            const seededOriginal = document.createElement("div").style;
            const seededOutput = document.createElement("div").style;
            for (const name of Object.keys(test.expected))
              seededOriginal.setProperty(name, "inherit");
            const before = seededOriginal.cssText;
            seededOriginal.setProperty(property, test.value);
            seededOutput.cssText = before + ";" + output;
            const missingResets = Object.keys(test.expected).filter(
              (name) =>
                seededOriginal.getPropertyValue(name) !== "inherit" &&
                seededOutput.getPropertyValue(name) === "inherit",
            );
            if (missingResets.length)
              return { status: "shorthand-reset-changed", properties: missingResets };
          }
          // CSSOM differences may be equivalent at computed/used-value time. Keep
          // these visible for later contextual checks; don't call them passes.
          if (differences.length) {
            if (/(?:url\(|var\(|env\()/i.test(output))
              return { status: "context-required", differences };
            // A derived, fixed-size fixture resolves simple equivalent spellings.
            // Preserve the CSSOM difference in the report; this establishes only
            // equivalence in this context, not general computed-value conformance.
            const containers = [original, transformed].map((style) => {
              const container = document.createElement("div");
              container.style.cssText = "width:800px;height:600px;font:16px serif;color:black";
              const element = document.createElement("div");
              element.style.cssText = style.cssText;
              container.append(element);
              document.body.append(container);
              return { container, element };
            });
            try {
              const styles = containers.map(({ element }) => getComputedStyle(element));
              const computed = differences.map(({ property }) => ({
                property,
                original: styles[0].getPropertyValue(property),
                transformed: styles[1].getPropertyValue(property),
              }));
              return {
                status: computed.some((x) => x.original !== x.transformed)
                  ? "computed-difference"
                  : "contextual-match",
                context: "derived-800x600-16px-serif",
                differences,
                computed,
              };
            } finally {
              containers.forEach(({ container }) => container.remove());
            }
          }
          return { status: "pass" };
        },
        { test, output },
      );
    },
    close: () => browser.close(),
  };
}
