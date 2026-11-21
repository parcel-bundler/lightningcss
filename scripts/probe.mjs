#!/usr/bin/env node
// Ask Chromium directly how it parses or evaluates something. This is the
// quickest way to settle "what does a real browser do with X" questions.
//
// Usage:
//   node scripts/probe.mjs [--css FILE | --css-text CSS] [--html HTML]
//        [--viewport WxH] [--media dark|print|...] EXPR...
//
// Each EXPR is a JavaScript expression evaluated in the page after the CSS and
// HTML load; results are printed as JSON. Helpers available in expressions:
//   cs(selector, prop, pseudo?)  computed value of prop on the first match
//   parse(prop, value)           how CSSOM serializes a declaration ('' = invalid)
//   rules()                      the stylesheet's rules as serialized by CSSOM
//
// Examples:
//   node scripts/probe.mjs "matchMedia('(min-width)').media" "matchMedia('(min-width), print').matches"
//   node scripts/probe.mjs "CSS.supports('selector(:lang(en, fr))')" "parse('line-height', '0px')"
//   node scripts/probe.mjs --css-text '.a{color:light-dark(red,blue)}' --html '<div class=a></div>' \
//     --media dark "cs('.a', 'color')"

import puppeteer from 'puppeteer';
import fs from 'fs';
import {findChrome, MEDIA, emulateMedia} from './equivalence.mjs';

let argv = process.argv.slice(2);
let css = '', html = '', viewport = {width: 1280, height: 800}, media = MEDIA.default, exprs = [];
for (let i = 0; i < argv.length; i++) {
  switch (argv[i]) {
    case '--css': css = fs.readFileSync(argv[++i], 'utf8'); break;
    case '--css-text': css = argv[++i]; break;
    case '--html': html = argv[++i]; break;
    case '--viewport': {
      let [width, height] = argv[++i].split('x').map(Number);
      viewport = {width, height};
      break;
    }
    case '--media': media = MEDIA[argv[++i]]; if (!media) throw new Error('Unknown media'); break;
    case '-h': case '--help': break;
    default: exprs.push(argv[i]);
  }
}
if (!exprs.length || argv.includes('--help') || argv.includes('-h')) {
  console.error(fs.readFileSync(new URL(import.meta.url), 'utf8').split('\n\n')[1].replace(/^\/\/ ?/gm, ''));
  process.exit(2);
}

let browser = await puppeteer.launch({headless: true, executablePath: findChrome(), args: ['--no-sandbox']});
try {
  let page = await browser.newPage();
  await page.setViewport(viewport);
  await emulateMedia(page, media);
  await page.setContent(`<!doctype html><html><head><style id="__probe">${css.replace(/<\/style/gi, '<\\/style')}</style></head><body>${html}</body></html>`);
  console.log(`// ${await browser.version()}, ${viewport.width}x${viewport.height}, media: ${media.name}`);
  for (let expr of exprs) {
    let result = await page.evaluate((expr) => {
      let cs = (sel, prop, pseudo) => {
        let el = document.querySelector(sel);
        return el ? getComputedStyle(el, pseudo).getPropertyValue(prop) : '(no element)';
      };
      let parse = (prop, value) => {
        let s = document.createElement('div').style;
        s.setProperty(prop, value);
        return s.getPropertyValue(prop);
      };
      let rules = () => {
        let out = [];
        let visit = (list) => { for (let r of list) out.push(r.cssText); };
        visit(document.getElementById('__probe').sheet.cssRules);
        return out;
      };
      try {
        return {value: (0, eval)(`(cs, parse, rules) => (${expr})`)(cs, parse, rules)};
      } catch (err) {
        return {error: String(err)};
      }
    }, expr);
    console.log(`${expr}\n  => ${result.error ? 'ERROR ' + result.error : JSON.stringify(result.value)}`);
  }
} finally {
  await browser.close();
}
