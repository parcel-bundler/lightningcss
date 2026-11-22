// #137: @supports/@media conditions merged from duplicate @imports come out in nondeterministic order.
// Bundles the same entry N times in one process and reports the distinct outputs.
import path from 'path';
import {fileURLToPath} from 'url';
const dir = path.join(path.dirname(fileURLToPath(import.meta.url)), '137-fixture');
const N = Number(process.env.N || 200);
const entry = process.env.ENTRY || 'a.css';
export function run(l) {
  const seen = new Map();
  for (let i = 0; i < N; i++) {
    const out = l.bundle({filename: path.join(dir, entry), minify: true}).code.toString();
    seen.set(out, (seen.get(out) || 0) + 1);
  }
  return `distinct=${seen.size}\n` + [...seen].map(([o, c]) => `${c}x ${o}`).join('\n');
}
export const check = (out) => out.startsWith('distinct=1\n');
