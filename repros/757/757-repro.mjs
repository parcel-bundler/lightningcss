// #757 (dup #1193): lightningcss-cli's `bin` is a text placeholder that postinstall.js replaces
// with the native binary. When install scripts don't run (pnpm >= 10 default, bun's untrusted
// lifecycle scripts, npm --ignore-scripts), executing the bin gives `This: not found` (exit 127).
// This checks the placeholder in the repo checkout (cli/lightningcss, which is what gets
// published), so every version column shows the same master result.
// check(): running the bin without postinstall either works or prints an actionable error,
// never the shell trying to execute the placeholder sentence.
import {spawnSync} from 'child_process';

export const cases = [
  {
    name: 'cli bin without postinstall',
    run: (_l, {require}) => {
      let bin = require.resolve('./cli/lightningcss');
      let r = spawnSync('sh', [bin, '--version'], {encoding: 'utf8'});
      return `status=${r.status}\n${r.stdout}${r.stderr}`;
    },
    check: out => !/This: (command )?not found/.test(out) &&
      (out.startsWith('status=0') || /postinstall|install script|lightningcss-cli-/i.test(out)),
  },
];
