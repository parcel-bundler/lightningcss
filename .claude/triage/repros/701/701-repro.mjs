// #701: bundling lightningcss (esbuild, as Vite/serverless-esbuild/Bun.build do) failed with
// `Could not resolve "../pkg"` because node/index.js had a static require of the WASM fallback.
// Removed in v1.32.0 (#1170). check(): esbuild can bundle the loader without resolve errors.
async function bundleLoader(lightningcss, {require}, extra = {}) {
  // Find the node/index.js of the version under test via the shared CJS module cache.
  let entry = Object.keys(require.cache).find(k => /lightningcss[\\/]node[\\/]index\.js$/.test(k));
  if (!entry) return 'ERROR: could not locate loaded lightningcss/node/index.js';
  let esbuild = require('esbuild');
  try {
    let res = await esbuild.build({entryPoints: [entry], bundle: true, platform: 'node', write: false, logLevel: 'silent', ...extra});
    return `ok; warnings: ${res.warnings.map(w => w.text).join(' | ') || 'none'}`;
  } catch (err) {
    return `ERROR: ${(err.errors || []).map(e => e.text).join(' | ') || err.message}`;
  }
}

export const cases = [
  {
    name: 'esbuild bundle of lightningcss loader resolves',
    run: (l, ctx) => bundleLoader(l, ctx),
    // A source checkout (local) has a built lightningcss.<platform>.node next to the package, which
    // esbuild's glob then tries to load ("No loader is configured for .node"); published packages
    // don't ship that file, so only an unresolvable module counts as failure.
    check: out => !out.includes('Could not resolve'),
  },
  {
    // Control: the documented workaround (mark lightningcss external) always works.
    name: 'control: lightningcss marked external',
    run: async (l, {require}) => {
      let esbuild = require('esbuild');
      let res = await esbuild.build({stdin: {contents: "module.exports = require('lightningcss')", resolveDir: '.'}, bundle: true, platform: 'node', write: false, logLevel: 'silent', external: ['lightningcss']});
      return `ok; ${res.outputFiles[0].text.includes('require("lightningcss")')}`;
    },
    check: out => out === 'ok; true',
  },
];
