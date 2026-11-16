#!/usr/bin/env node
// Browser equivalence oracle.
//
// Loads an original stylesheet and Lightning CSS's output for it into a real
// browser against the same DOM, then compares the computed style of every
// element and pseudo-element. Any difference means the transform changed what
// the CSS does, at least in this browser.
//
// The comparison runs across several viewports, forced interaction states
// (:hover, :focus, ...), dir/lang variations and sampled animation times, so
// @media, state-dependent selectors and @keyframes are covered too.
//
// Usage:
//   node scripts/equivalence.mjs input.css [options] [-- lightningcss-cli-args]
//
// Options:
//   --html FILE          HTML fixture (body content or full document). When
//                        omitted, random DOM trees are generated from the
//                        classes, ids, tags and attributes in the selectors.
//   --transformed FILE   Compare against this CSS instead of running the CLI.
//   --bin PATH           Lightning CSS CLI binary (default target/release or
//                        target/debug lightningcss). Build it with
//                        `cargo build --release --features cli`.
//   --seeds N            Number of generated fixtures (default 4).
//   --viewports LIST     e.g. 1280x800,375x667. Default: 1280x800, plus
//                        narrower ones when the CSS uses @media/@container or
//                        viewport/container units.
//   --states LIST        Forced pseudo-class sets, comma separated; join
//                        several in one set with "+". "none" = no forcing.
//                        Default: "none" plus each of hover, focus(-*),
//                        active, checked, visited that the CSS mentions.
//   --concurrency N      Parallel browser pages (default 4).
//   --no-animations      Skip sampling animations at several times.
//   --chrome PATH        Chrome/Chromium executable (or CHROME_PATH env).
//   --max-diffs N        Maximum differences to print (default 40).
//   --json               Print a machine-readable report.
//
// Everything after `--` is passed to the CLI (default: --minify). Examples:
//   node scripts/equivalence.mjs test.css
//   node scripts/equivalence.mjs test.css -- --minify --targets 'safari 12'
//   node scripts/equivalence.mjs test.css --transformed out.css --html page.html
//
// Exit codes: 0 equivalent, 1 differences found, 2 error.
//
// Limitations: only one browser engine (the one launched). Lowered output for
// old targets is only checked for meaning-preservation in this browser, not
// for working in the old browser. Fonts, images and @import URLs are not
// loaded. A pass means "no difference observed for these fixtures", not proof.

import puppeteer from 'puppeteer';
import {execFileSync} from 'child_process';
import fs from 'fs';
import path from 'path';
import {fileURLToPath} from 'url';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const PSEUDOS = ['::before', '::after', '::marker', '::placeholder', '::first-letter', '::first-line', '::selection', '::backdrop', '::file-selector-button'];
const DEFAULT_ANIMATION_SAMPLES = [0, 0.3, 0.5, 0.99];

// Pick a test matrix based on what the CSS uses, so trivial inputs stay fast.
export function autoMatrix(css) {
  let viewports = /@media|@container|@custom-media|\d(?:[sdl]?v[hwib]|vmin|vmax|cq[whib]|cqmin|cqmax)\b/i.test(css)
    ? '1280x800,500x800,320x900' : '1280x800';
  let states = ['none'];
  if (/:hover\b/i.test(css)) states.push('hover');
  if (/:focus/i.test(css)) states.push('focus+focus-visible+focus-within');
  if (/:active\b/i.test(css)) states.push('active');
  if (/:(?:checked|indeterminate)\b/i.test(css)) states.push('checked');
  if (/:visited\b/i.test(css)) states.push('visited');
  return {viewports, states: states.join(',')};
}

function parseArgs(argv) {
  let dash = argv.indexOf('--');
  let cliArgs = dash >= 0 ? argv.slice(dash + 1) : ['--minify'];
  let args = dash >= 0 ? argv.slice(0, dash) : argv;
  let opts = {
    input: null,
    html: null,
    transformed: null,
    bin: null,
    seeds: 4,
    viewports: null,
    states: null,
    concurrency: 4,
    animations: true,
    chrome: process.env.CHROME_PATH || null,
    maxDiffs: 40,
    json: false,
    cliArgs,
  };
  for (let i = 0; i < args.length; i++) {
    let a = args[i];
    let next = () => {
      if (i + 1 >= args.length) throw new Error(`Missing value for ${a}`);
      return args[++i];
    };
    switch (a) {
      case '--html': opts.html = next(); break;
      case '--transformed': opts.transformed = next(); break;
      case '--bin': opts.bin = next(); break;
      case '--seeds': opts.seeds = Number(next()); break;
      case '--viewports': opts.viewports = next(); break;
      case '--states': opts.states = next(); break;
      case '--no-animations': opts.animations = false; break;
      case '--chrome': opts.chrome = next(); break;
      case '--max-diffs': opts.maxDiffs = Number(next()); break;
      case '--concurrency': opts.concurrency = Number(next()); break;
      case '--json': opts.json = true; break;
      case '-h': case '--help':
        console.log(fs.readFileSync(fileURLToPath(import.meta.url), 'utf8').split('\n\n')[1].replace(/^\/\/ ?/gm, ''));
        process.exit(0);
      default:
        if (a.startsWith('--')) throw new Error(`Unknown option ${a}`);
        if (opts.input) throw new Error('Only one input file is supported');
        opts.input = a;
    }
  }
  if (!opts.input) throw new Error('Missing input CSS file');
  return opts;
}

export function findBinary() {
  for (let p of ['target/release/lightningcss', 'target/debug/lightningcss']) {
    let full = path.join(ROOT, p);
    if (fs.existsSync(full)) return full;
  }
  throw new Error('No lightningcss CLI binary found. Run `cargo build --release --features cli` or pass --bin.');
}

export function findChrome() {
  for (let p of [process.env.CHROME_PATH, process.env.WPT_CHROME, '/opt/pw-browsers/chromium', '/usr/bin/google-chrome', '/usr/bin/chromium']) {
    if (p && fs.existsSync(p)) return p;
  }
  return undefined; // fall back to puppeteer's bundled browser
}

export function runCli(bin, inputFile, cliArgs) {
  return execFileSync(bin, [...cliArgs, inputFile], {encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe']});
}

// Deterministic PRNG so fixtures are reproducible.
function mulberry32(seed) {
  return function () {
    seed |= 0; seed = seed + 0x6D2B79F5 | 0;
    let t = Math.imul(seed ^ seed >>> 15, 1 | seed);
    t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
    return ((t ^ t >>> 14) >>> 0) / 4294967296;
  };
}

// Collect the vocabulary used in selectors, using the browser's own parser so
// values like `#fff` are never mistaken for ids.
async function collectVocabulary(page, css) {
  return page.evaluate((css) => {
    let sheet = new CSSStyleSheet();
    sheet.replaceSync(css);
    let selectors = [];
    let visit = (rules) => {
      for (let rule of rules) {
        if (rule.selectorText) selectors.push(rule.selectorText);
        if (rule.cssRules) visit(rule.cssRules);
      }
    };
    visit(sheet.cssRules);
    let text = selectors.join(' ');
    let unique = (re, group = 1) => [...new Set([...text.matchAll(re)].map(m => m[group]))];
    let classes = unique(/\.((?:\\.|[\w-])+)/g).map(c => c.replace(/\\(.)/g, '$1'));
    let ids = unique(/#((?:\\.|[\w-])+)/g).map(c => c.replace(/\\(.)/g, '$1'));
    let tags = unique(/(?:^|[\s>+~(,|])([a-zA-Z][\w-]*)(?![\w-]*\()/g).filter(t => t !== 'n' && !/^(of|odd|even)$/.test(t));
    let attrs = [...text.matchAll(/\[\s*([\w-]+)\s*(?:[~|^$*]?=\s*(?:"([^"]*)"|'([^']*)'|([^\]\s]+)))?/g)]
      .map(m => [m[1], m[2] ?? m[3] ?? m[4] ?? '']);
    return {classes, ids, tags, attrs, usesDirLang: /:dir\(|:lang\(/.test(text)};
  }, css);
}

const VOID = new Set(['input', 'img', 'br', 'hr', 'meta', 'link', 'area', 'base', 'col', 'embed', 'source', 'track', 'wbr']);
const SKIP_TAGS = new Set(['html', 'head', 'body', 'style', 'script', 'title', 'meta', 'link', 'base', 'template']);

function generateFixture(vocab, seed) {
  let rand = mulberry32(seed);
  let pick = (arr) => arr[Math.floor(rand() * arr.length)];
  let tags = ['div', 'span', 'p', ...vocab.tags.filter(t => !SKIP_TAGS.has(t.toLowerCase()))];
  let esc = (s) => s.replace(/&/g, '&amp;').replace(/"/g, '&quot;').replace(/</g, '&lt;');
  let count = 0;
  let node = (depth) => {
    count++;
    let tag = depth === 0 ? 'div' : pick(tags);
    let attrs = [];
    let cls = vocab.classes.filter(() => rand() < 0.35);
    if (cls.length) attrs.push(`class="${esc(cls.join(' '))}"`);
    if (vocab.ids.length && rand() < 0.15) attrs.push(`id="${esc(pick(vocab.ids))}"`);
    for (let [name, value] of vocab.attrs) {
      if (rand() < 0.3) attrs.push(`${name}="${esc(value)}"`);
    }
    if (rand() < 0.12) attrs.push('dir="rtl"');
    if (vocab.usesDirLang && rand() < 0.2) attrs.push(`lang="${pick(['en', 'fr', 'de-CH'])}"`);
    if (rand() < 0.2) attrs.push('tabindex="0"');
    let open = `<${tag}${attrs.length ? ' ' + attrs.join(' ') : ''}>`;
    if (VOID.has(tag.toLowerCase())) return open;
    let children = '';
    let n = depth >= 4 || count > 120 ? 0 : Math.floor(rand() * (depth === 0 ? 5 : 4));
    for (let i = 0; i < n; i++) {
      if (rand() < 0.3) children += 'Text ';
      children += node(depth + 1);
    }
    if (!children || rand() < 0.3) children += 'Lorem ipsum';
    return `${open}${children}</${tag}>`;
  };
  // Also include one element with every class and a nested chain of them, so
  // compound and descendant selectors always have something to match.
  let all = vocab.classes.length ? `<div class="${esc(vocab.classes.join(' '))}">All</div>` : '';
  let chain = vocab.classes.reduceRight((inner, c) => `<div class="${esc(c)}">${inner}</div>`, 'Chain');
  return node(0) + node(0) + all + chain;
}

function documentFor(body, css) {
  if (/<html[\s>]/i.test(body)) {
    // Full document fixture: inject the stylesheet at the end of <head>.
    let style = `<style id="__equiv">${css.replace(/<\/style/gi, '<\\/style')}</style>`;
    return /<\/head>/i.test(body) ? body.replace(/<\/head>/i, style + '</head>') : style + body;
  }
  return `<!doctype html><html><head><meta charset="utf-8"><style id="__equiv">${css.replace(/<\/style/gi, '<\\/style')}</style></head><body>${body}</body></html>`;
}

async function forceStates(page, states) {
  let client = await page.target().createCDPSession();
  await client.send('DOM.enable');
  await client.send('CSS.enable');
  let {root} = await client.send('DOM.getDocument', {depth: -1});
  let {nodeIds} = await client.send('DOM.querySelectorAll', {nodeId: root.nodeId, selector: '*'});
  await Promise.all(nodeIds.map(nodeId => client.send('CSS.forcePseudoState', {nodeId, forcedPseudoClasses: states})));
  return client;
}

// Snapshot computed styles of every element and pseudo-element.
async function snapshot(page, pseudos, customProps, animationFraction) {
  return page.evaluate((PSEUDOS, customProps, animationFraction) => {
    // Finish transitions started by forced states; pause and seek animations
    // so results never depend on wall-clock timing.
    for (let a of document.getAnimations()) {
      if (typeof CSSTransition !== 'undefined' && a instanceof CSSTransition) {
        a.finish();
      } else {
        let timing = a.effect.getComputedTiming();
        let duration = Number.isFinite(timing.duration) ? timing.duration : 0;
        a.pause();
        a.currentTime = (timing.delay || 0) + duration * animationFraction;
      }
    }
    let read = (el, pseudo) => {
      let style = getComputedStyle(el, pseudo);
      let res = {};
      for (let i = 0; i < style.length; i++) {
        let name = style.item(i);
        res[name] = style.getPropertyValue(name);
      }
      for (let name of customProps) {
        let v = style.getPropertyValue(name);
        if (v !== '') res[name] = v;
      }
      return res;
    };
    let describe = (el) => {
      let parts = [];
      for (let e = el; e && e.nodeType === 1; e = e.parentElement) {
        let s = e.localName;
        if (e.id && e.id !== '__equiv') s += '#' + e.id;
        if (e.classList.length) s += '.' + [...e.classList].join('.');
        let parent = e.parentElement;
        if (parent) s += `:nth-child(${[...parent.children].indexOf(e) + 1})`;
        parts.unshift(s);
      }
      return parts.join(' > ');
    };
    let out = [];
    for (let el of document.querySelectorAll('*')) {
      if (['head', 'style', 'script', 'meta', 'title', 'link'].includes(el.localName)) continue;
      let entry = {path: describe(el), styles: {'': read(el, null)}};
      for (let p of PSEUDOS) {
        // Skip pseudo-elements that are not generated for this element.
        if (p === '::before' || p === '::after') {
          let content = getComputedStyle(el, p).content;
          if (content === 'none' || content === 'normal') continue;
        }
        if (p === '::marker' && !/list-item/.test(getComputedStyle(el).display)) continue;
        if ((p === '::placeholder') && !['input', 'textarea'].includes(el.localName)) continue;
        if (p === '::file-selector-button' && el.localName !== 'input') continue;
        entry.styles[p] = read(el, p);
      }
      out.push(entry);
    }
    return out;
  }, pseudos, customProps, animationFraction);
}

function numbersClose(a, b) {
  // Treat values equal if they differ only by tiny floating point noise.
  let re = /-?\d*\.?\d+(?:e[-+]?\d+)?/gi;
  if (a.replace(re, '#') !== b.replace(re, '#')) return false;
  let na = a.match(re) || [], nb = b.match(re) || [];
  return na.every((x, i) => Math.abs(Number(x) - Number(nb[i])) <= 1e-3 * Math.max(1, Math.abs(Number(x))));
}

function compareSnapshots(a, b, context) {
  let diffs = [];
  if (a.length !== b.length) {
    diffs.push({context, path: '(document)', property: '(element count)', original: String(a.length), transformed: String(b.length)});
    return diffs;
  }
  for (let i = 0; i < a.length; i++) {
    for (let pseudo of new Set([...Object.keys(a[i].styles), ...Object.keys(b[i].styles)])) {
      let sa = a[i].styles[pseudo], sb = b[i].styles[pseudo];
      if (!sa || !sb) {
        diffs.push({context, path: a[i].path + pseudo, property: '(generated)', original: String(!!sa), transformed: String(!!sb), kind: 'computed'});
        continue;
      }
      for (let prop of new Set([...Object.keys(sa), ...Object.keys(sb)])) {
        let va = sa[prop] ?? '', vb = sb[prop] ?? '';
        if (va === vb) continue;
        // Custom property values are token sequences and are observable via
        // var(), style() queries and CSSOM, so only whitespace is ignored.
        let kind = numbersClose(va, vb) ? 'rounding'
          : prop.startsWith('--') && va.replace(/\s+/g, '') === vb.replace(/\s+/g, '') ? 'whitespace'
          : 'computed';
        diffs.push({context, path: a[i].path + pseudo, property: prop, original: va, transformed: vb, kind});
      }
    }
  }
  return diffs;
}

export async function compare({originalCss, transformedCss, fixtures, viewports, states, animations, chrome, concurrency = 4}) {
  let browser = await puppeteer.launch({
    headless: true,
    executablePath: chrome ?? findChrome(),
    args: ['--no-sandbox', '--font-render-hinting=none'],
  });
  let both = originalCss + transformedCss;
  let customProps = [...new Set([...both.matchAll(/--[\w-]+(?=\s*:)/g)].map(m => m[0]))];
  // Only inspect pseudo-elements the CSS can style.
  let pseudos = PSEUDOS.filter(p => new RegExp(':?' + p.replace(/^::/, ':') + '\\b', 'i').test(both));
  let hasAnimations = animations && /@keyframes/i.test(both);
  let jobs = [];
  for (let fixture of fixtures) {
    for (let viewport of viewports) {
      for (let stateSet of states) {
        // Sample animation times without forced states only, to bound runtime.
        let fractions = hasAnimations && !stateSet.length ? DEFAULT_ANIMATION_SAMPLES : [0];
        for (let fraction of fractions) {
          let context = `${fixture.name} @ ${viewport.width}x${viewport.height}` +
            (stateSet.length ? ` :${stateSet.join(':')}` : '') +
            (hasAnimations && !stateSet.length ? ` anim=${fraction}` : '');
          jobs.push({fixture, viewport, stateSet, fraction, context});
        }
      }
    }
  }
  try {
    let render = async (page, css, {fixture, viewport, stateSet, fraction}) => {
      await page.setViewport(viewport);
      await page.setContent(documentFor(fixture.html, css), {waitUntil: 'load'});
      let client = stateSet.length ? await forceStates(page, stateSet) : null;
      let snap = await snapshot(page, pseudos, customProps, fraction);
      if (client) await client.detach();
      return snap;
    };
    let diffs = [];
    let next = 0;
    let worker = async () => {
      let page = await browser.newPage();
      while (next < jobs.length) {
        let job = jobs[next++];
        let a = await render(page, originalCss, job);
        let b = await render(page, transformedCss, job);
        diffs.push(...compareSnapshots(a, b, job.context));
      }
      await page.close();
    };
    await Promise.all(Array.from({length: Math.max(1, Math.min(concurrency, jobs.length))}, worker));
    return {browser: await browser.version(), checks: jobs.length, diffs};
  } finally {
    await browser.close();
  }
}

export async function buildFixtures({originalCss, html, seeds, chrome}) {
  if (html != null) return [{name: 'fixture', html}];
  let browser = await puppeteer.launch({headless: true, executablePath: chrome ?? findChrome(), args: ['--no-sandbox']});
  try {
    let page = await browser.newPage();
    let vocab = await collectVocabulary(page, originalCss);
    return Array.from({length: seeds}, (_, i) => ({name: `seed${i + 1}`, html: generateFixture(vocab, i + 1)}));
  } finally {
    await browser.close();
  }
}

async function main() {
  let opts;
  try {
    opts = parseArgs(process.argv.slice(2));
  } catch (err) {
    console.error(err.message + '\nRun with --help for usage.');
    process.exit(2);
  }
  let originalCss = fs.readFileSync(opts.input, 'utf8');
  let transformedCss;
  if (opts.transformed) {
    transformedCss = fs.readFileSync(opts.transformed, 'utf8');
  } else {
    try {
      transformedCss = runCli(opts.bin ?? findBinary(), opts.input, opts.cliArgs);
    } catch (err) {
      console.error(`Lightning CSS failed:\n${err.stderr || err.message}`);
      process.exit(2);
    }
  }
  let auto = autoMatrix(originalCss);
  let viewports = (opts.viewports ?? auto.viewports).split(',').map(v => {
    let [width, height] = v.split('x').map(Number);
    return {width, height};
  });
  let states = (opts.states ?? auto.states).split(',').map(s => (s === 'none' ? [] : s.split('+')));
  let fixtures = await buildFixtures({originalCss, html: opts.html && fs.readFileSync(opts.html, 'utf8'), seeds: opts.seeds, chrome: opts.chrome});
  let result = await compare({originalCss, transformedCss, fixtures, viewports, states, animations: opts.animations, chrome: opts.chrome, concurrency: opts.concurrency});

  let significant = result.diffs.filter(d => d.kind === 'computed');
  let minor = result.diffs.filter(d => d.kind !== 'computed');
  if (opts.json) {
    console.log(JSON.stringify({transformedCss, ...result, equivalent: significant.length === 0}, null, 2));
  } else {
    console.log(`Browser: ${result.browser}`);
    console.log(`Transformed CSS (${originalCss.length} -> ${transformedCss.length} bytes):\n${transformedCss.trim()}\n`);
    console.log(`Compared ${result.checks} renders across ${fixtures.length} fixture(s).`);
    // Collapse differences with the same values (e.g. `color` and every
    // currentcolor-derived border color) into one entry.
    let groups = new Map();
    for (let d of significant) {
      let key = `${d.original}\u0000${d.transformed}`;
      if (!groups.has(key)) groups.set(key, {...d, properties: new Set(), count: 0});
      let g = groups.get(key);
      g.properties.add(d.property);
      g.count++;
    }
    if (groups.size === 0) {
      console.log('EQUIVALENT: no computed-style differences.');
    } else {
      console.log(`NOT EQUIVALENT: ${significant.length} computed-style difference(s), ${groups.size} distinct:`);
      for (let g of [...groups.values()].slice(0, opts.maxDiffs)) {
        let props = [...g.properties].sort((a, b) => a.length - b.length);
        let shown = props.length > 6 ? props.slice(0, 6).join(', ') + `, +${props.length - 6} more` : props.join(', ');
        console.log(`\n  ${shown}  (x${g.count}, e.g. ${g.context})\n    at ${g.path}\n    original:    ${g.original}\n    transformed: ${g.transformed}`);
      }
      if (groups.size > opts.maxDiffs) console.log(`\n  ... ${groups.size - opts.maxDiffs} more`);
    }
    if (minor.length) {
      let kinds = minor.reduce((acc, d) => ((acc[d.kind] = (acc[d.kind] || 0) + 1), acc), {});
      console.log(`\nIgnored minor differences: ${Object.entries(kinds).map(([k, n]) => `${n} ${k}`).join(', ')}` +
        ' (whitespace inside custom property values; float rounding noise). Use --json to inspect.');
    }
  }
  process.exit(significant.length ? 1 : 0);
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  main().catch(err => {
    console.error(err.stack || err.message);
    process.exit(2);
  });
}
