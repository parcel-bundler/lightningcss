#!/usr/bin/env node
// Run a file of CSS edge cases through one or two Lightning CSS builds under
// several option sets, show where outputs differ, and check every output with
// the browser equivalence oracle (scripts/equivalence.mjs).
//
// This is the main tool for reviewing a PR (base build vs PR build) and for
// reproducing issues (a single build).
//
// Usage:
//   node scripts/cases.mjs CASES.css [--bin PATH] [--base-bin PATH]
//        [--config ARGS]... [--html FILE] [--repeat N] [--no-browser]
//        [--verbose] [--json]
//
// CASES.css holds several cases, each starting with a marker comment:
//   /* case: shorthand after longhand */
//   .a { margin-left: 1px; margin: 0 }
//   /* case: ... */
// Per-case directives (comments inside the case):
//   /* html: <div class="a"></div> */   DOM fixture instead of generated ones
//   /* configs: --minify | --targets 'safari 12' */
//                                        replaces the --config list for this case
//   /* expect: error */                  the build under test should reject it
//
// --config may be repeated; each value is a CLI argument string. Defaults:
//   "--minify", "--minify --targets 'safari 12' --targets 'chrome 90'",
//   "--targets 'chrome 60'"
// Targets always include a Chromium version, because the oracle runs in
// Chromium: output for a Safari-only target may legitimately not work there.
//
// --bin is the build under test (default target/release/lightningcss).
// --base-bin, if given, is the build to compare against (e.g. master). Every
// run is then classified:
//   regression   base output equivalent (or ok), new output not (or errors)
//   fixed        base output not equivalent (or errors), new output equivalent
//   still-broken neither output is equivalent
//   changed      both equivalent but different output (check size/intent)
//   ok           unchanged and equivalent
// --repeat N runs each CLI invocation N times and flags nondeterministic output.
//
// Exit code: 1 if there are regressions (with --base-bin), or any
// non-equivalent / unexpectedly failing output (without), else 0.

import fs from 'fs';
import os from 'os';
import path from 'path';
import {execFileSync} from 'child_process';
import {checkEquivalence, findBinary} from './equivalence.mjs';

const DEFAULT_CONFIGS = ['--minify', "--minify --targets 'safari 12' --targets 'chrome 90'", "--targets 'chrome 60'"];

// Split "--targets 'safari 12' --minify" into argv like a shell would.
function splitArgs(str) {
  return [...str.matchAll(/'([^']*)'|"([^"]*)"|(\S+)/g)].map(m => m[1] ?? m[2] ?? m[3]);
}

function parseCases(text) {
  let parts = text.split(/^\/\*\s*case:\s*(.*?)\s*\*\/[ \t]*$/m);
  let cases = [];
  let directive = (body, name) => body.match(new RegExp(`/\\*\\s*${name}:\\s*([\\s\\S]*?)\\*/`))?.[1]?.trim() ?? null;
  for (let i = 1; i < parts.length; i += 2) {
    let body = parts[i + 1];
    let css = body.replace(/\/\*\s*(?:html|configs|args|expect):[\s\S]*?\*\//g, '').trim();
    let configs = directive(body, 'configs') ?? directive(body, 'args');
    cases.push({
      name: parts[i],
      css,
      html: directive(body, 'html'),
      configs: configs ? configs.split('|').map(s => s.trim()).filter(Boolean) : null,
      expectError: directive(body, 'expect') === 'error',
    });
  }
  if (!cases.length && text.trim()) cases.push({name: 'input', css: text.trim(), html: null, configs: null, expectError: false});
  return cases;
}

// Keep only the meaningful part of an error (the CLI panics with a backtrace).
function shortError(stderr) {
  let lines = stderr.split('\n').filter(l => l.trim());
  let msg = lines.find(l => /Error|error|panicked/.test(l)) ?? lines[0] ?? 'error';
  return msg.replace(/^thread 'main' panicked at [^:]+:\d+:\d+:\s*/, '').slice(0, 300);
}

function run(bin, file, args, repeat) {
  let outputs = new Set();
  let result;
  for (let i = 0; i < repeat; i++) {
    try {
      result = {ok: true, out: execFileSync(bin, [...args, file], {encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe']}).trim()};
    } catch (err) {
      result = {ok: false, out: shortError(err.stderr || err.message)};
    }
    outputs.add(result.out);
  }
  result.nondeterministic = outputs.size > 1 ? outputs.size : 0;
  return result;
}

async function main() {
  let argv = process.argv.slice(2);
  let opts = {file: null, bin: null, baseBin: null, configs: [], html: null, browser: true, json: false, verbose: false, repeat: 1};
  for (let i = 0; i < argv.length; i++) {
    let a = argv[i];
    switch (a) {
      case '--bin': opts.bin = argv[++i]; break;
      case '--base-bin': opts.baseBin = argv[++i]; break;
      case '--config': opts.configs.push(argv[++i]); break;
      case '--html': opts.html = fs.readFileSync(argv[++i], 'utf8'); break;
      case '--repeat': opts.repeat = Number(argv[++i]); break;
      case '--no-browser': opts.browser = false; break;
      case '--verbose': opts.verbose = true; break;
      case '--json': opts.json = true; break;
      case '-h': case '--help':
        console.log(fs.readFileSync(new URL(import.meta.url), 'utf8').split('\n\n')[1].replace(/^\/\/ ?/gm, ''));
        process.exit(0);
      default:
        if (a.startsWith('--')) throw new Error(`Unknown option ${a}`);
        opts.file = a;
    }
  }
  if (!opts.file) throw new Error('Usage: node scripts/cases.mjs CASES.css [--bin PATH] [--base-bin PATH] [--config ARGS]... (see --help)');
  let bin = opts.bin ?? findBinary();
  let configs = opts.configs.length ? opts.configs : DEFAULT_CONFIGS;
  let cases = parseCases(fs.readFileSync(opts.file, 'utf8'));
  let tmp = fs.mkdtempSync(path.join(os.tmpdir(), 'lcss-cases-'));
  let results = [];

  let check = async (c, css) => {
    let r = await checkEquivalence({originalCss: c.css, transformedCss: css, html: c.html ?? opts.html, seeds: 3});
    let seen = new Set();
    let sample = [];
    for (let d of r.significant) {
      let key = d.property + d.original + d.transformed;
      if (seen.has(key) || sample.length >= 3) continue;
      seen.add(key);
      sample.push(`${d.property}: ${JSON.stringify(d.original)} -> ${JSON.stringify(d.transformed)} (${d.context})`);
    }
    let notes = [...new Set(r.diffs.filter(d => d.kind === 'color-space' || d.kind === 'prefixed-fallback').map(d => d.kind))];
    return {equivalent: r.equivalent, diffs: r.significant.length, sample, notes};
  };

  for (let c of cases) {
    let file = path.join(tmp, 'case.css');
    fs.writeFileSync(file, c.css);
    for (let cfg of c.configs ?? configs) {
      let args = splitArgs(cfg);
      let pr = run(bin, file, args, opts.repeat);
      let base = opts.baseBin ? run(opts.baseBin, file, args, opts.repeat) : null;
      let entry = {case: c.name, config: cfg, input: c.css, output: pr.out, ok: pr.ok, nondeterministic: pr.nondeterministic};
      if (base) {
        entry.baseOutput = base.out;
        entry.baseOk = base.ok;
        entry.baseNondeterministic = base.nondeterministic;
        entry.changed = base.out !== pr.out;
      }
      if (opts.browser) {
        if (pr.ok) entry.browser = await check(c, pr.out);
        if (base && base.ok) entry.baseBrowser = entry.changed ? await check(c, base.out) : entry.browser;
      }
      // Classify.
      let good = (ok, browser) => ok && (!browser || browser.equivalent);
      let prGood = c.expectError ? !pr.ok : good(pr.ok, entry.browser) && !pr.nondeterministic;
      if (base) {
        let baseGood = c.expectError ? !base.ok : good(base.ok, entry.baseBrowser) && !base.nondeterministic;
        entry.status = baseGood && !prGood ? 'regression'
          : !baseGood && prGood ? 'fixed'
          : !baseGood && !prGood ? 'still-broken'
          : entry.changed ? 'changed' : 'ok';
      } else {
        entry.status = prGood ? 'ok' : c.expectError ? 'unexpected-success' : !pr.ok ? 'error' : pr.nondeterministic ? 'nondeterministic' : 'not-equivalent';
      }
      results.push(entry);
    }
  }
  fs.rmSync(tmp, {recursive: true, force: true});

  if (opts.json) {
    console.log(JSON.stringify(results, null, 2));
  } else {
    let lastCase = null;
    let one = (s) => s.replace(/\s*\n\s*/g, ' ');
    let fmtBrowser = (b, ok) => !ok ? 'error' : !b ? '-' : b.equivalent ? 'equiv' : `NOT EQUIV (${b.diffs})`;
    for (let r of results) {
      let interesting = opts.verbose || r.status !== 'ok';
      if (!interesting) continue;
      if (r.case !== lastCase) {
        console.log(`\n=== ${r.case} ===\n${r.input}`);
        lastCase = r.case;
      }
      console.log(`\n  [${r.config}] ${r.status.toUpperCase()}`);
      if (r.baseOutput !== undefined && r.changed) {
        console.log(`    base: ${one(r.baseOutput)}   [${fmtBrowser(r.baseBrowser, r.baseOk)}]`);
        console.log(`    new:  ${one(r.output)}   [${fmtBrowser(r.browser, r.ok)}]`);
      } else {
        console.log(`    ${r.baseOutput !== undefined ? '(unchanged) ' : ''}${one(r.output)}   [${fmtBrowser(r.browser, r.ok)}]`);
      }
      if (r.nondeterministic) console.log(`      ! nondeterministic: ${r.nondeterministic} distinct outputs in ${opts.repeat} runs`);
      if (r.baseNondeterministic) console.log(`      ! base nondeterministic: ${r.baseNondeterministic} distinct outputs in ${opts.repeat} runs`);
      for (let s of r.browser?.sample ?? []) console.log(`      ! ${s}`);
      if (r.browser?.notes?.length) console.log(`      (not counted: ${r.browser.notes.join(', ')})`);
    }

    // Summary table.
    console.log('\n| case | config | status | base | new |\n|---|---|---|---|---|');
    for (let r of results) {
      console.log(`| ${r.case} | \`${r.config}\` | ${r.status} | ${r.baseOutput !== undefined ? fmtBrowser(r.baseBrowser, r.baseOk) : ''} | ${fmtBrowser(r.browser, r.ok)}${r.changed ? ' (changed)' : ''} |`);
    }
    let counts = results.reduce((acc, r) => ((acc[r.status] = (acc[r.status] || 0) + 1), acc), {});
    console.log(`\n${results.length} runs, ${cases.length} cases: ${Object.entries(counts).map(([k, n]) => `${n} ${k}`).join(', ')}.` +
      (opts.verbose ? '' : ' Unchanged equivalent runs are hidden above (use --verbose).'));
  }
  let failing = opts.baseBin ? results.some(r => r.status === 'regression') : results.some(r => r.status !== 'ok');
  process.exit(failing ? 1 : 0);
}

main().catch(err => {
  console.error(err.stack || err.message);
  process.exit(2);
});
