import fs from "node:fs";
import path from "node:path";
import vm from "node:vm";
import { types } from "node:util";
import { CaptureWorker } from "./capture.mjs";
import { createHash } from "node:crypto";
import { execFileSync, spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

export const directory = path.dirname(fileURLToPath(import.meta.url));
export const revision = JSON.parse(fs.readFileSync(path.join(directory, "revision.json")));
export const hash = (value) => createHash("sha256").update(value).digest("hex");
const helperExports = JSON.parse(fs.readFileSync(path.join(directory, "helper-exports.json")));
const helpers = new Map([
  ["test_valid_value", "valid"],
  ["test_invalid_value", "invalid"],
  ["test_shorthand_value", "shorthand"],
]);
const trimASCII = (value) => value.replace(/^[\t\n\f\r ]+|[\t\n\f\r ]+$/g, "");
const scripts = new Set([
  "/resources/testharness.js",
  "/resources/testharnessreport.js",
  "/css/support/parsing-testcommon.js",
  "/css/support/shorthand-testcommon.js",
]);

// JavaScript can throw any value, including null and objects with failing getters.
export function errorMessage(error) {
  if (error === null || !["object", "function"].includes(typeof error))
    return String(error) || "Unknown extraction error";
  // isNativeError does not invoke Proxy traps. Only read a native Error's own
  // data property; source-defined getters and coercions must never run here.
  if (types.isNativeError(error)) {
    const message = Object.getOwnPropertyDescriptor(error, "message");
    if (typeof message?.value === "string" && message.value) return message.value;
  }
  return "Unknown extraction error";
}

// Synchronous collection stage. CaptureWorker commits the page after handling rejections.
export function capture(file, document) {
  const cases = [];
  let scriptLine;
  let collectionError;
  try {
    if (document.unsupportedReason) throw Error(document.unsupportedReason);
    if (document.variants.length) throw Error("variants require a browser adapter");
    const collectors = Object.fromEntries(
      [...helpers].map(([helper, kind]) => [
        helper,
        (...args) => {
          if (collectionError) return;
          try {
            if (cases.length >= 100000) throw Error("too many generated cases");
            const [property, value, expected, options] = args;
            if (typeof property !== "string" || typeof value !== "string")
              throw Error("non-string property/value");
            if (options === null || (options &&
                (Object.keys(options).length || options.comparisonFunction)))
              throw Error("custom comparison/options require a browser adapter");
            if (args.length > (kind === "invalid" ? 2 : 4))
              throw Error("unsupported helper arguments");
            // WPT's test() runs assertions immediately. Later source mutations
            // must not alter an earlier helper call's expectation.
            const snapshot = Array.isArray(expected)
              ? Array.from({length: expected.length}, (_, i) => expected[i])
              : kind === "shorthand" && expected && typeof expected === "object"
                ? Object.fromEntries(Object.entries(expected)) : expected;
            if (
              kind === "shorthand" &&
              (!expected ||
                typeof expected !== "object" ||
                Array.isArray(expected) ||
                Object.values(snapshot).some((v) => typeof v !== "string"))
            )
              throw Error("invalid longhand expectations");
            if (
              kind === "valid" &&
              args.length >= 3 &&
              typeof expected !== "string" &&
              !(Array.isArray(expected) && snapshot.every((v) => typeof v === "string"))
            )
              throw Error("unsupported expectation");
            // JSON cannot carry unpaired UTF-16 surrogates into Rust strings losslessly.
            const data = {
              kind,
              property,
              value,
              ...(kind === "invalid" ? {} : { expected: args.length >= 3 ? snapshot : value }),
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
          } catch (error) {
            // Do not let source-level catch blocks or rejected promises hide a
            // helper call that this adapter could not represent.
            collectionError = errorMessage(error);
          }
        },
      ]),
    );
    // Known helper APIs outside this adapter must reject the entire page even
    // when source code catches their exception. Keep the existing diagnostics.
    const reject = (name) => {
      const message = `${name} is not defined`;
      collectionError ??= message;
      throw new ReferenceError(message);
    };
    for (const name of new Set(Object.values(helperExports).flat())) {
      if (!Object.hasOwn(collectors, name)) collectors[name] = () => reject(name);
    }
    const sandbox = {};
    const context = vm.createContext(sandbox, {
      codeGeneration: { strings: false, wasm: false },
      microtaskMode: "afterEvaluate",
    });
    // Promise handlers use the callback's realm to select their microtask queue.
    // Expose VM functions so direct or bound helpers also drain before returning.
    for (const name of Object.keys(collectors)) {
      collectors[name] = vm.compileFunction("return function(...args) { return collect(...args); }", ["collect"],
        {parsingContext: context})(collectors[name]);
    }
    // A caught namespace/DOM error must not hide a qualified helper call either.
    for (const name of ["window", "self", "document", "CSS", "location", "navigator"]) {
      const get = vm.compileFunction("return function() { return reject(); }", ["reject"],
        {parsingContext: context})(() => reject(name));
      Object.defineProperty(sandbox, name, {get});
    }
    for (const script of document.scripts) {
      if (script.attrs.type &&
          !["text/javascript", "application/javascript"].includes(trimASCII(script.attrs.type).toLowerCase()))
        throw Error(`unsupported script type: ${script.attrs.type}`);
      if (["language", "for", "event"].some((name) => name in script.attrs))
        throw Error("legacy script attributes require a browser adapter");
      // Classic nomodule scripts do not execute in the browser used by this harness.
      if ("nomodule" in script.attrs) continue;
      if ("src" in script.attrs) {
        if (trimASCII(script.attrs.charset ?? ""))
          throw Error("script charset requires an encoding adapter");
        if ((script.attrs.integrity ?? "").trim())
          throw Error("script integrity requires a browser adapter");
        if ("async" in script.attrs || "defer" in script.attrs)
          throw Error("asynchronous helper scripts require a browser adapter");
        if (!script.attrs.src) throw Error("empty external script URL");
        const url = new URL(script.attrs.src, `https://wpt.invalid/${file}`);
        if (url.origin !== "https://wpt.invalid" || url.username || url.password ||
            url.search || !scripts.has(url.pathname)) {
          throw Error(`unsupported dependency: ${script.attrs.src}`);
        }
        for (const name of helperExports[url.pathname] ?? []) {
          // Parsing/shorthand helpers use global function declarations, whose
          // bindings cannot be deleted. Testharness exposes APIs by assignment.
          if (url.pathname === "/resources/testharness.js") {
            sandbox[name] = collectors[name];
          } else {
            Object.defineProperty(sandbox, name, {
              value: collectors[name], writable: true, enumerable: true, configurable: false,
            });
          }
        }
      } else {
        scriptLine = script.line;
        vm.runInContext(script.code, context, {
          filename: file,
          lineOffset: script.line - 1,
          timeout: 500,
          displayErrors: false,
        });
        if (collectionError) throw Error(collectionError);
        // These globals can register callbacks that execute after inline scripts.
        // Do not accept a partial page without running its event lifecycle.
        if (Object.getOwnPropertyNames(sandbox).some((name) => /^on[a-z]+$/i.test(name)))
          throw Error("event-handler globals require a browser adapter");
      }
    }
    return { file, status: cases.length ? "extracted" : "empty", cases };
  } catch (error) {
    return { file, status: "unsupported", reason: errorMessage(error), cases: [] };
  }
}

export async function extract(root, discovery = false) {
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
  const worker = new CaptureWorker();
  const pages = [];
  try {
    for (const [i, file] of files.entries())
      pages.push({...await worker.request(file, documents[i]), sourceHash: hash(sources[i])});
  } finally { await worker.close(); }
  return {
    schemaVersion: revision.schemaVersion,
    wptRevision: commit,
    pages: pages.map(({ cases, ...page }) => ({ ...page, caseCount: cases.length })),
    cases: pages.flatMap((p) => p.cases),
  };
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const args = process.argv.slice(2);
  if (!args[0] || args[0].startsWith("--"))
    throw Error("Usage: node wpt/extract.mjs WPT_CHECKOUT [--discovery] [--output FILE]");
  for (let i = 1; i < args.length; i++) {
    if (args[i] === "--discovery") continue;
    if (args[i] === "--output" && args[i + 1] && !args[i + 1].startsWith("--")) {
      i++;
      continue;
    }
    throw Error(`Unknown or incomplete option: ${args[i]}`);
  }
  const corpus = await extract(path.resolve(args[0]), args.includes("--discovery"));
  const outputIndex = args.indexOf("--output");
  const output = outputIndex < 0 ? path.join(directory, "fixtures.json") : args[outputIndex + 1];
  fs.writeFileSync(output, JSON.stringify(corpus, null, 2) + "\n");
  console.log(
    `${corpus.cases.length} cases; ${corpus.pages.filter((p) => p.status !== "extracted").length} unadapted pages; ${output}`,
  );
}
