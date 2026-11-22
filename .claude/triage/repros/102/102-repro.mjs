// #102: bundle() + cssModules. Today each file is hashed separately, but only the entry
// file's classes are exported, so classes defined only in an @import-ed file are
// unreachable and a class defined in both files gets two different names.
// check() encodes the maintainer's stated design (issue comment, 2022-03-12): @import-ed
// files behave as if written in the entry file -> same generated name for `.test` in both
// files, and imported classes appear in the entry's exports. This is pending a maintainer
// decision (the reporter instead asked for per-file export maps); revisit if it changes.
import path from 'path';
import {fileURLToPath} from 'url';
const dir = path.join(path.dirname(fileURLToPath(import.meta.url)), '102-fixture');
function run(l) {
  const r = l.bundle({filename: path.join(dir, 'index.css'), cssModules: true});
  const ex = Object.fromEntries(
    Object.entries(r.exports || {}).sort(([a], [b]) => a.localeCompare(b)).map(([k, v]) => [k, v.name]),
  );
  return r.code.toString() + '\n' + JSON.stringify(ex);
}
const selectors = (out) => [...out.matchAll(/^\.(\S+) \{/gm)].map((m) => m[1]);
export const cases = [
  {
    name: 'same-class-in-both-files-gets-one-name',
    run,
    check: (out) => {
      const s = selectors(out).filter((n) => n.endsWith('test'));
      return s.length === 2 && s[0] === s[1];
    },
  },
  {
    name: 'imported-only-class-is-exported',
    run,
    check: (out) => /"only-in-app":"[^"]+"/.test(out),
  },
  // control: the entry's own class is exported and matches a rule in the output
  {
    name: 'control-entry-class-exported',
    run,
    check: (out) => {
      const m = out.match(/"test":"([^"]+)"/);
      return !!m && selectors(out).includes(m[1]);
    },
  },
];
