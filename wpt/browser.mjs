import puppeteer from "puppeteer";

// Phase 2 adapts only context-free parsing helpers. Fixture-dependent computed
// tests will need the WPT server and are explicitly skipped here.
export async function startBrowser(executablePath, timeout = 5000) {
  const browser = await puppeteer.launch({
    headless: true,
    ...(executablePath ? { executablePath } : {}),
  });
  let page, version, executable;
  try {
    page = await browser.newPage();
    await page.setContent('<!doctype html><meta charset="utf-8"><body>');
    version = await browser.version();
    executable = browser.process()?.spawnfile;
  } catch (error) {
    await browser.close().catch(() => {});
    throw error;
  }
  return {
    version,
    executable,
    async check(test, output) {
      if (!test.contextFree) return { status: "fixture-required" };
      if (/(?:url\(|var\(|env\()/i.test(test.value)) return { status: "context-required" };
      const evaluation = page.evaluate(
        (input) => {
          "use strict";
          const {test, output} = input;
          const original = document.createElement("div").style;
          const transformed = document.createElement("div").style;
          const property = test.property;
          const contextDependent = (name, value) => {
            if (/(?:url\(|var\(|env\()/i.test(value)) return true;
            // CSSOM can preserve escaped function names. Typed OM identifies
            // pending substitutions without relying on their source spelling.
            // Custom properties also use CSSUnparsedValue for plain token lists.
            try {
              return !name.startsWith("--") &&
                CSSStyleValue.parse(name, value) instanceof CSSUnparsedValue;
            } catch { return false; }
          };
          if (contextDependent(property, test.value)) return { status: "context-required" };
          let read;
          try {
            original[property] = test.value;
            read = original.getPropertyValue(property);
          } catch (error) {
            return {status: "baseline-failure", reason: "original setter threw", error: error.message};
          }
          // CSS escapes and image-set() strings can introduce URLs even when
          // the source text contains no literal url() function.
          if (contextDependent(property, read)) return { status: "context-required" };
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
              ([name, value]) => original[name] !== value,
            );
            if (differences.length)
              return {
                status: "baseline-failure",
                differences: differences.map(([name, expected]) => ({
                  property: name,
                  expected,
                  actual: original[name],
                })),
              };
          }
          if (test.kind === "shorthand") {
            if (!CSS.supports(property, test.value))
              return { status: "baseline-failure", reason: "CSS.supports rejected shorthand" };
            for (const name of Object.keys(test.expected)) {
              const check = document.createElement("div").style;
              const value = original[name];
              check[name] = value;
              if (check[name] !== value)
                return {
                  status: "baseline-failure",
                  reason: "longhand serialization did not round-trip",
                  property: name,
                };
            }
            const check = document.createElement("div").style;
            check[property] = test.value;
            for (const name of Object.keys(test.expected)) check[name] = "";
            if (check.length)
              return {
                status: "baseline-failure",
                reason: "shorthand set unrelated longhands",
                actual: check.cssText,
              };
          }
          if (test.kind === "valid") {
            const check = document.createElement("div").style;
            check[property] = read;
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
            // An invalid CSS declaration leaves the fallback intact. Calling
            // setProperty again would instead remove it for the empty string.
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
            ...(test.kind === "shorthand" ? Object.keys(test.expected) : []),
          ]);
          const differences = [...names]
            .map((name) => ({
              property: name,
              original: original.getPropertyValue(name),
              transformed: transformed.getPropertyValue(name),
            }))
            .filter((x) => x.original !== x.transformed);
          const priorities = [...names].filter(
            (name) => original.getPropertyPriority(name) !== transformed.getPropertyPriority(name),
          );
          if (priorities.length) return { status: "priority-changed", properties: priorities };
          // `all` interacts with declaration order beyond this isolated probe.
          if (names.has("all") && differences.length)
            return { status: "context-required", differences };
          // Valid-value helpers also cover shorthands. Check the affected
          // declaration set before comparing computed defaults: an omitted
          // longhand can leave a preceding declaration in the cascade intact.
          {
            const originalNames = new Set(original);
            const transformedNames = new Set(transformed);
            const declaredNames = new Set([...originalNames, ...transformedNames]);
            const missingResets = [...declaredNames].filter(
              (name) => originalNames.has(name) && !transformedNames.has(name) &&
                transformed.getPropertyValue(name) === "",
            );
            if (missingResets.length)
              return { status: "shorthand-reset-changed", properties: missingResets };
            const extraResets = [...declaredNames].filter(
              (name) => transformedNames.has(name) && !originalNames.has(name) &&
                original.getPropertyValue(name) === "",
            );
            if (extraResets.length) {
              // Compatibility lowering can add helper variables or prefixed
              // fallbacks. Their surrounding cascade is outside this fixture.
              if (extraResets.every((name) => name.startsWith("--lightningcss-") ||
                  (/^-(?:webkit|moz|ms|o)-/.test(name) &&
                    name.replace(/^-(?:webkit|moz|ms|o)-/, "") === property)))
                return { status: "context-required", properties: extraResets, differences };
              return { status: "cascade-changed", properties: extraResets };
            }
          }
          // CSSOM differences may be equivalent at computed/used-value time. Keep
          // these visible for later contextual checks; don't call them passes.
          if (differences.length) {
            if (/(?:url\(|var\(|env\()/i.test(output) || [...transformed].some(
              (name) => contextDependent(name, transformed.getPropertyValue(name)),
            ))
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
              // CSS Easing 1 defines start/end as aliases of jump-start/jump-end.
              // Normalize only parsed timing-function values, retaining raw
              // strings in the report. https://drafts.csswg.org/css-easing-1/#step-easing-functions
              const comparable = (property, value) =>
                /^(?:-(?:webkit|moz|o)-)?(?:animation|transition)-timing-function$/.test(property)
                  ? value.replace(/\bstep-start\b/g, "steps(1,start)")
                    .replace(/\bstep-end\b/g, "steps(1,end)")
                    .replace(/\bsteps\(\s*(\d+)\s*(?:,\s*(jump-start|jump-end|start|end|jump-none|jump-both))?\s*\)/g,
                      (_match, count, position = "end") => `steps(${count},${position.replace(/^jump-(start|end)$/, "$1")})`)
                  : value;
              return {
                status: computed.some((x) => comparable(x.property, x.original) !== comparable(x.property, x.transformed))
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
      let timer;
      try {
        return await Promise.race([
          evaluation,
          new Promise((_resolve, reject) => {
            timer = setTimeout(() => reject(Error(`Browser check timed out after ${timeout}ms`)), timeout);
          }),
        ]);
      } finally { clearTimeout(timer); }
    },
    close: () => browser.close(),
  };
}
