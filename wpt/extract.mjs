import fs from "node:fs";
import path from "node:path";
import vm from "node:vm";
import { createHash } from "node:crypto";
import { execFileSync, spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

export const directory = path.dirname(fileURLToPath(import.meta.url));
export const revision = JSON.parse(fs.readFileSync(path.join(directory, "revision.json")));
export const hash = (value) => createHash("sha256").update(value).digest("hex");
const helpers = new Map([
  ["test_valid_value", "valid"],
  ["test_invalid_value", "invalid"],
  ["test_shorthand_value", "shorthand"],
]);
const scripts = new Set([
  "/resources/testharness.js",
  "/resources/testharnessreport.js",
  "/css/support/parsing-testcommon.js",
  "/css/support/shorthand-testcommon.js",
]);

export function capture(file, document) {
  const cases = [];
  let scriptLine;
  try {
    if (document.variants.length) throw Error("variants require a browser adapter");
    const sandbox = Object.fromEntries(
      [...helpers].map(([helper, kind]) => [
        helper,
        (...args) => {
          if (cases.length >= 100000) throw Error("too many generated cases");
          const [property, value, expected, options] = args;
          if (typeof property !== "string" || typeof value !== "string")
            throw Error("non-string property/value");
          if (options && Object.keys(options).length)
            throw Error("custom comparison/options require a browser adapter");
          if (args.length > (kind === "invalid" ? 2 : 4))
            throw Error("unsupported helper arguments");
          if (
            kind === "shorthand" &&
            (!expected ||
              typeof expected !== "object" ||
              Array.isArray(expected) ||
              Object.values(expected).some((v) => typeof v !== "string"))
          )
            throw Error("invalid longhand expectations");
          if (
            kind === "valid" &&
            args.length >= 3 &&
            typeof expected !== "string" &&
            !(Array.isArray(expected) && expected.every((v) => typeof v === "string"))
          )
            throw Error("unsupported expectation");
          // JSON cannot carry unpaired UTF-16 surrogates into Rust strings losslessly.
          const data = {
            kind,
            property,
            value,
            ...(kind === "invalid" ? {} : { expected: args.length >= 3 ? expected : value }),
          };
          JSON.stringify(data, (_key, value) => {
            if (
              typeof value === "string" &&
              [...value].some((ch) => {
                const code = ch.codePointAt(0);
                return code >= 0xd800 && code <= 0xdfff;
              })
            )
              throw Error("unpaired surrogate requires an encoding adapter");
            return value;
          });
          const identity = hash(JSON.stringify({ kind, property, value })).slice(0, 16);
          const duplicate = cases.filter((c) => c.identity === identity).length;
          cases.push({
            id: `${file}#${identity}:${duplicate}`,
            identity,
            file,
            ...data,
            scriptLine,
            contextFree: document.contextFree,
          });
        },
      ]),
    );
    const context = vm.createContext(sandbox, { codeGeneration: { strings: false, wasm: false } });
    for (const script of document.scripts) {
      if (script.attrs.src) {
        const url = new URL(script.attrs.src, `https://wpt.invalid/${file}`);
        if (url.origin !== "https://wpt.invalid" || url.search || !scripts.has(url.pathname)) {
          throw Error(`unsupported dependency: ${script.attrs.src}`);
        }
      } else {
        if (
          script.attrs.type &&
          !["text/javascript", "application/javascript"].includes(script.attrs.type)
        ) {
          throw Error(`unsupported script type: ${script.attrs.type}`);
        }
        scriptLine = script.line;
        vm.runInContext(script.code, context, {
          filename: file,
          lineOffset: script.line - 1,
          timeout: 500,
        });
      }
    }
    return { file, status: cases.length ? "extracted" : "empty", cases };
  } catch (error) {
    return { file, status: "unsupported", reason: error.message, cases: [] };
  }
}

export function extract(root, discovery = false) {
  const commit = execFileSync("git", ["-C", root, "rev-parse", "HEAD"], {
    encoding: "utf8",
  }).trim();
  if (commit !== revision.commit)
    throw Error(`WPT revision mismatch: expected ${revision.commit}, got ${commit}`);
  if (
    execFileSync("git", ["-C", root, "status", "--porcelain", "--", "css"], {
      encoding: "utf8",
    }).trim()
  ) {
    throw Error("WPT css checkout is modified; restore it before extracting");
  }
  let files = JSON.parse(fs.readFileSync(path.join(directory, "selection.json"))).files;
  if (discovery) {
    files = execFileSync("git", ["-C", root, "ls-files", "css"], {
      encoding: "utf8",
      maxBuffer: 20e6,
    })
      .split("\n")
      .filter(
        (f) =>
          f.endsWith(".html") &&
          /(?:parsing|shorthand)-testcommon\.js/.test(fs.readFileSync(path.join(root, f), "utf8")),
      );
  }
  files.sort();
  const sources = files.map((f) => fs.readFileSync(path.join(root, f), "utf8"));
  const parsed = spawnSync("python3", [path.join(directory, "read-html.py")], {
    input: sources.map((s) => JSON.stringify(s)).join("\n") + "\n",
    encoding: "utf8",
    maxBuffer: 100e6,
  });
  if (parsed.status !== 0) throw Error(parsed.stderr || String(parsed.error));
  const documents = parsed.stdout
    .trim()
    .split("\n")
    .map((s) => JSON.parse(s));
  if (documents.length !== files.length) throw Error("HTML reader returned incomplete results");
  const pages = files.map((file, i) => ({
    ...capture(file, documents[i]),
    sourceHash: hash(sources[i]),
  }));
  return {
    schemaVersion: revision.schemaVersion,
    wptRevision: commit,
    pages: pages.map(({ cases, ...page }) => ({ ...page, caseCount: cases.length })),
    cases: pages.flatMap((p) => p.cases),
  };
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const args = process.argv.slice(2);
  if (!args[0])
    throw Error("Usage: node wpt/extract.mjs WPT_CHECKOUT [--discovery] [--output FILE]");
  const corpus = extract(path.resolve(args[0]), args.includes("--discovery"));
  const outputIndex = args.indexOf("--output");
  const output = outputIndex < 0 ? path.join(directory, "fixtures.json") : args[outputIndex + 1];
  fs.writeFileSync(output, JSON.stringify(corpus, null, 2) + "\n");
  console.log(
    `${corpus.cases.length} cases; ${corpus.pages.filter((p) => p.status !== "extracted").length} unadapted pages; ${output}`,
  );
}
