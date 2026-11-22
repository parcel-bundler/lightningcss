// #1163: no prebuilt binaries for Linux ppc64le / s390x (Node.js CI needs them; AIX out of scope).
// node/index.js loads `lightningcss-${platform}-${arch}-gnu`, so the packages must be named
// lightningcss-linux-ppc64-gnu and lightningcss-linux-s390x-gnu (process.arch is 'ppc64'/'s390x').
// check(): the published package lists them as optionalDependencies. Control: existing triples.
import fs from 'fs';
import path from 'path';

function optionalDeps({require}) {
  let entry = Object.keys(require.cache).find(k => /lightningcss[\\/]node[\\/]index\.js$/.test(k));
  let pkg = JSON.parse(fs.readFileSync(path.join(path.dirname(entry), '..', 'package.json'), 'utf8'));
  let deps = Object.keys(pkg.optionalDependencies || {});
  if (deps.length) return deps.sort().join('\n');
  // Source checkout: optionalDependencies are generated at release time from the triples in
  // scripts/build-npm.js, so report those instead.
  let script = fs.readFileSync(path.join(path.dirname(entry), '..', 'scripts', 'build-npm.js'), 'utf8');
  return [...script.matchAll(/name: '([\w-]+)'/g)].map(m => 'triple:' + m[1]).sort().join('\n');
}

export const cases = [
  {
    name: 'ppc64le + s390x prebuilt packages listed',
    run: (_l, ctx) => optionalDeps(ctx),
    check: out => (out.includes('lightningcss-linux-ppc64-gnu') || out.includes('triple:powerpc64le-unknown-linux-gnu')) &&
      (out.includes('lightningcss-linux-s390x-gnu') || out.includes('triple:s390x-unknown-linux-gnu')),
  },
  {
    name: 'control: x64/arm64 linux gnu packages listed',
    run: (_l, ctx) => optionalDeps(ctx),
    check: out => (out.includes('lightningcss-linux-x64-gnu') || out.includes('triple:x86_64-unknown-linux-gnu')) &&
      (out.includes('lightningcss-linux-arm64-gnu') || out.includes('triple:aarch64-unknown-linux-gnu')),
  },
];
