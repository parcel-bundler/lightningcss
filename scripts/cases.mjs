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
//        [--config ARGS]... [--html FILE] [--no-browser] [--json]
//
// CASES.css holds several cases, each starting with a marker comment:
//   /* case: shorthand after longhand */
//   .a { margin-left: 1px; margin: 0 }
//   /* case: ... */
// A case may set its own fixture with `/* html: <div class="a"></div> */`
// and extra CLI arguments with `/* args: --targets 'safari 12' */`.
//
// --config may be repeated; each value is a CLI argument string. The default
// set is: "--minify", "--minify --targets 'safari 12'" and "--targets 'chrome 60'".
// --bin is the build under test (default target/release/lightningcss);
// --base-bin, if given, is the build to compare against (e.g. master).
//
// Exit code is 1 if any output from --bin is not browser-equivalent to its
// input, or if a build errors; 0 otherwise.

import fs from 'fs';
import os from 'os';
import path from 'path';
import {execFileSync} from 'child_process';
import {compare, buildFixtures, autoMatrix, findBinary} from './equivalence.mjs';

const DEFAULT_CONFIGS = ['--minify', "--minify --targets 'safari 12'", "--targets 'chrome 60'"];

// Split "--targets 'safari 12' --minify" into argv like a shell would.
function splitArgs(str) {
  return [...str.matchAll(/'([^']*)'|"([^"]*)"|(\S+)/g)].map(m => m[1] ?? m[2] ?? m[3]);
}

function parseCases(text) {
  let parts = text.split(/^\/\*\s*case:\s*(.*?)\s*\*\/[ \t]*$/m);
  let cases = [];
  for (let i = 1; i < parts.length; i += 2) {
    let body = parts[i + 1];
    let html = body.match(/\/\*\s*html:\s*([\s\S]*?)\*\//)?.[1]?.trim() ?? null;
    let args = body.match(/\/\*\s*args:\s*([\s\S]*?)\*\//)?.[1]?.trim() ?? null;
    let css = body.replace(/\/\*\s*(?:html|args):[\s\S]*?\*\//g, '').trim();
    cases.push({name: parts[i], css, html, args});
  }
  if (!cases.length && text.trim()) cases.push({name: 'input', css: text.trim(), html: null, args: null});
  return cases;
}

function run(bin, file, args) {
  try {
    return {ok: true, out: execFileSync(bin, [...args, file], {encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe']}).trim()};
  } catch (err) {
    return {ok: false, out: (err.stderr || err.message).trim()};
  }
}

async function main() {
  let argv = process.argv.slice(2);
  let opts = {file: null, bin: null, baseBin: null, configs: [], html: null, browser: true, json: false};
  for (let i = 0; i < argv.length; i++) {
    let a = argv[i];
    switch (a) {
      case '--bin': opts.bin = argv[++i]; break;
      case '--base-bin': opts.baseBin = argv[++i]; break;
      case '--config': opts.configs.push(argv[++i]); break;
      case '--html': opts.html = fs.readFileSync(argv[++i], 'utf8'); break;
      case '--no-browser': opts.browser = false; break;
      case '--json': opts.json = true; break;
      default:
        if (a.startsWith('--')) throw new Error(`Unknown option ${a}`);
        opts.file = a;
    }
  }
  if (!opts.file) throw new Error('Usage: node scripts/cases.mjs CASES.css [--bin PATH] [--base-bin PATH] [--config ARGS]...');
  let bin = opts.bin ?? findBinary();
  let configs = opts.configs.length ? opts.configs : DEFAULT_CONFIGS;
  let cases = parseCases(fs.readFileSync(opts.file, 'utf8'));
  let tmp = fs.mkdtempSync(path.join(os.tmpdir(), 'lcss-cases-'));
  let results = [];
  let failed = false;

  for (let c of cases) {
    let file = path.join(tmp, 'case.css');
    fs.writeFileSync(file, c.css);
    let caseConfigs = c.args ? configs.map(cfg => `${cfg} ${c.args}`) : configs;
    let fixtures = opts.browser ? await buildFixtures({originalCss: c.css, html: c.html ?? opts.html, seeds: 3}) : null;
    let auto = autoMatrix(c.css);
    for (let cfg of caseConfigs) {
      let args = splitArgs(cfg);
      let pr = run(bin, file, args);
      let base = opts.baseBin ? run(opts.baseBin, file, args) : null;
      let entry = {case: c.name, config: cfg, input: c.css, output: pr.out, ok: pr.ok};
      if (base) {
        entry.baseOutput = base.out;
        entry.changed = base.out !== pr.out;
      }
      if (!pr.ok) failed = true;
      if (opts.browser && pr.ok) {
        let check = async (css) => {
          let r = await compare({
            originalCss: c.css,
            transformedCss: css,
            fixtures,
            viewports: auto.viewports.split(',').map(v => ({width: +v.split('x')[0], height: +v.split('x')[1]})),
            states: auto.states.split(',').map(s => (s === 'none' ? [] : s.split('+'))),
            animations: true,
          });
          let sig = r.diffs.filter(d => d.kind === 'computed');
          let seen = new Set();
          let sample = [];
          for (let d of sig) {
            let key = d.property + d.original + d.transformed;
            if (seen.has(key) || sample.length >= 5) continue;
            seen.add(key);
            sample.push(`${d.property}: ${JSON.stringify(d.original)} -> ${JSON.stringify(d.transformed)} at ${d.path} (${d.context})`);
          }
          return {equivalent: sig.length === 0, diffs: sig.length, sample};
        };
        entry.browser = await check(pr.out);
        if (!entry.browser.equivalent) failed = true;
        if (base && base.ok && entry.changed) entry.baseBrowser = await check(base.out);
      }
      results.push(entry);
    }
  }
  fs.rmSync(tmp, {recursive: true, force: true});

  if (opts.json) {
    console.log(JSON.stringify(results, null, 2));
  } else {
    let lastCase = null;
    for (let r of results) {
      if (r.case !== lastCase) {
        console.log(`\n=== ${r.case} ===\n${r.input}`);
        lastCase = r.case;
      }
      let status = !r.ok ? 'ERROR' : !r.browser ? '' : r.browser.equivalent ? 'browser: equivalent' : `browser: NOT EQUIVALENT (${r.browser.diffs} diffs)`;
      console.log(`\n  [${r.config}] ${status}`);
      if (r.baseOutput !== undefined) {
        if (r.changed) {
          let baseStatus = r.baseBrowser ? (r.baseBrowser.equivalent ? 'equivalent' : `NOT EQUIVALENT (${r.baseBrowser.diffs} diffs)`) : '';
          console.log(`    base: ${r.baseOutput}${baseStatus ? `   [${baseStatus}]` : ''}`);
          console.log(`    new:  ${r.output}`);
        } else {
          console.log(`    (unchanged) ${r.output}`);
        }
      } else {
        console.log(`    ${r.output}`);
      }
      for (let s of r.browser?.sample ?? []) console.log(`      ! ${s}`);
    }
    let changed = results.filter(r => r.changed).length;
    let bad = results.filter(r => r.browser && !r.browser.equivalent).length;
    let errors = results.filter(r => !r.ok).length;
    console.log(`\n${results.length} runs, ${cases.length} cases` +
      (opts.baseBin ? `, ${changed} changed vs base` : '') +
      (opts.browser ? `, ${bad} not browser-equivalent` : '') +
      `, ${errors} errors.`);
  }
  process.exit(failed ? 1 : 0);
}

main().catch(err => {
  console.error(err.stack || err.message);
  process.exit(2);
});
