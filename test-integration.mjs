import puppeteer from 'puppeteer';
import fetch from 'node-fetch';
import assert from 'assert';
import {diff} from 'jest-diff';
import * as css from 'lightningcss';

let urls = [
  'https://getbootstrap.com/docs/5.1/examples/headers/',
  'https://getbootstrap.com/docs/5.1/examples/heroes/',
  'https://getbootstrap.com/docs/5.1/examples/features/',
  'https://getbootstrap.com/docs/5.1/examples/sidebars/',
  'https://getbootstrap.com/docs/5.1/examples/footers/',
  'https://getbootstrap.com/docs/5.1/examples/dropdowns/',
  'https://getbootstrap.com/docs/5.1/examples/list-groups/',
  'https://getbootstrap.com/docs/5.1/examples/modals/',
  'http://csszengarden.com',
  'http://csszengarden.com/221/',
  'http://csszengarden.com/219/',
  'http://csszengarden.com/218/',
  'http://csszengarden.com/217/',
  'http://csszengarden.com/216/',
  'http://csszengarden.com/215/'
];

let success = true;
const browser = await puppeteer.launch();
const page = await browser.newPage();

for (let url of urls) {
  await test(url);
}

async function test(url) {
  console.log(`Testing ${url}...`);

  await page.goto(url);
  await page.waitForNetworkIdle();

  // Snapshot the computed styles of all elements
  let elements = await page.$$('body *');
  let computed = [];
  for (let element of elements) {
    let style = await element.evaluate(node => {
      let res = {};
      let style = window.getComputedStyle(node);
      for (let i = 0; i < style.length; i++) {
        res[style.item(i)] = style.getPropertyValue(style.item(i));
      }
      return res;
    });

    for (let key in style) {
      style[key] = normalize(key, style[key]);
    }

    computed.push(style);
  }

  // Find stylesheets, load, and minify.
  let stylesheets = await page.evaluate(() => {
    return [...document.styleSheets].map(styleSheet => styleSheet.href).filter(Boolean);
  });

  let texts = await Promise.all(stylesheets.map(async url => {
    let res = await fetch(url);
    return res.text();
  }));

  let minified = texts.map((code, i) => {
    let minified = css.transform({
      filename: 'test.css',
      code: Buffer.from(code),
      minify: true,
      targets: {
        chrome: 95 << 16
      }
    }).code.toString();
    console.log(new URL(stylesheets[i]).pathname, '–', code.length + ' bytes', '=>', minified.length + ' bytes');
    return minified;
  });

  await page.setCacheEnabled(false);

  // Disable the original stylesheets and insert a <style> element containing the minified CSS for each.
  await page.evaluate(minified => {
    let i = 0;
    for (let stylesheet of [...document.styleSheets]) {
      if (stylesheet.href) {
        stylesheet.disabled = true;

        let style = document.createElement('style');
        style.textContent = minified[i++].replace(/url\((.*?)\)/g, (orig, url) => {
          if (/['"]?data:/.test(url)) {
            return orig;
          }

          // Rewrite urls so they are relative to the original stylesheet.
          return `url(${new URL(url, stylesheet.href)})`;
        });

        stylesheet.ownerNode.insertAdjacentElement('beforebegin', style);
      }
    }
  }, minified);

  await page.waitForNetworkIdle();

  // Now get the computed style for each element again and compare.
  let i = 0;
  for (let element of elements) {
    let style = await element.evaluate(node => {
      let res = {};
      let style = window.getComputedStyle(node);
      for (let i = 0; i < style.length; i++) {
        let name = style.item(i);
        res[name] = style.getPropertyValue(name);
      }
      return res;
    });

    for (let key in style) {
      style[key] = normalize(key, style[key]);

      // Ignore prefixed properties that were removed during minification.
      if (key.startsWith('-webkit-box-') && style[key] !== computed[i][key]) {
        style[key] = computed[i][key];
      }
    }

    try {
      assert.deepEqual(style, computed[i]);
    } catch (err) {
      success = false;
      console.log(diff(computed[i], style));
      console.log(await element.evaluate(node => node.outerHTML))
      console.log(minified[0]);
    }

    i++;
  }

  console.log('');
}

function normalize(key, value) {
  if (key === 'background-position') {
    value = value.replace(/(^|\s)0(%|px)/g, '$10');
  }

  if (key === 'background-image') {
    // Center is implied.
    value = value.replace('radial-gradient(at center center, ', 'radial-gradient(');
  }

  return value.split(' ').map(v => {
    if (/^[\d\.]+px$/.test(v)) {
      let val = parseFloat(v);
      return Math.round(val) + 'px';
    }

    return v;
  }).join(' ');
}

if (success) {
  console.log('Pass!');
  process.exit(0);
} else {
  console.log('Fail!');
  process.exit(1);
}
