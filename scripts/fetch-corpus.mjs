#!/usr/bin/env node
// Fetch the pinned real-world CSS corpus listed in scripts/real-corpus.json
// from npm into target/review/real-corpus/<name>.css (cached). Prints the
// paths of the resulting files, one per line.
//
// Usage: node scripts/fetch-corpus.mjs [name...]
import fs from 'fs';
import path from 'path';
import {execFileSync} from 'child_process';
import {fileURLToPath} from 'url';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const OUT = path.join(ROOT, 'target/review/real-corpus');
const {corpus} = JSON.parse(fs.readFileSync(path.join(ROOT, 'scripts/real-corpus.json'), 'utf8'));
const only = process.argv.slice(2);

// Minimal glob: supports `*` within a single path segment.
function glob(dir, pattern) {
  let [head, ...rest] = pattern.split('/');
  let re = new RegExp('^' + head.replace(/[.+^${}()|[\]\\]/g, '\\$&').replace(/\*/g, '[^/]*') + '$');
  let matches = fs.existsSync(dir) ? fs.readdirSync(dir).filter(f => re.test(f)).sort() : [];
  return matches.flatMap(m => (rest.length ? glob(path.join(dir, m), rest.join('/')) : [path.join(dir, m)]));
}

fs.mkdirSync(OUT, {recursive: true});
for (let entry of corpus) {
  if (only.length && !only.includes(entry.name)) continue;
  let target = path.join(OUT, `${entry.name}.css`);
  let stamp = `${entry.package}@${entry.version} ${entry.files.join(',')}`;
  if (fs.existsSync(target) && fs.readFileSync(target + '.stamp', 'utf8').trim() === stamp) {
    console.log(target);
    continue;
  }
  // Each package goes in its own empty directory; its contents are only read.
  let dir = fs.mkdtempSync(path.join(OUT, `.fetch-${entry.name}-`));
  try {
    let tgz = execFileSync('npm', ['pack', '--silent', `${entry.package}@${entry.version}`], {cwd: dir, encoding: 'utf8'}).trim().split('\n').pop();
    execFileSync('tar', ['xzf', tgz], {cwd: dir});
    let files = entry.files.flatMap(f => glob(path.join(dir, 'package'), f));
    if (!files.length) throw new Error(`no files matched ${entry.files} in ${entry.package}@${entry.version}`);
    fs.writeFileSync(target, files.map(f => fs.readFileSync(f, 'utf8')).join('\n'));
    fs.writeFileSync(target + '.stamp', stamp);
    console.log(target);
  } catch (err) {
    console.error(`warning: ${entry.name}: ${err.message}`);
  } finally {
    fs.rmSync(dir, {recursive: true, force: true});
  }
}
