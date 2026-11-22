// #137 variant (3000 iterations: 1.33.0 fails only ~2 in 300, so fewer runs can miss it): whether bundling errors depends on the race (unconditional + supports + media imports of the same file).
import path from 'path';
import {fileURLToPath} from 'url';
const dir = path.join(path.dirname(fileURLToPath(import.meta.url)), '137-fixture');
const N = Number(process.env.N || 3000);
export function run(l) {
  const seen = new Map();
  for (let i = 0; i < N; i++) {
    let out;
    try { out = l.bundle({filename: path.join(dir, process.env.ENTRY || 'a3.css'), minify: true}).code.toString(); }
    catch (e) { out = 'ERROR: ' + e.message; }
    seen.set(out, (seen.get(out) || 0) + 1);
  }
  return `distinct=${seen.size}\n` + [...seen].map(([o, c]) => `${c}x ${o}`).join('\n');
}
export const check = (out) => out.startsWith('distinct=1\n');
