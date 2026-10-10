// @ts-check

import { test } from 'uvu';
import * as assert from 'uvu/assert';
import * as fs from 'node:fs';
import * as path from 'node:path';
import { fileURLToPath } from 'node:url';
import { parse } from '@babel/parser';

let dir = path.join(path.dirname(fileURLToPath(import.meta.url)), '..');

// package.json declares `engines: { node: ">= 12.0.0" }`, so every JavaScript
// file shipped in the npm package (node/*.js and node/*.mjs) must parse on
// Node 12. A SyntaxError is raised at parse time, before any code runs, so
// unsupported syntax cannot be guarded at runtime. Check each shipped file
// for syntax that Node 12 cannot parse.
const UNSUPPORTED = [
  ['OptionalMemberExpression', 'optional chaining (?.) requires Node 14'],
  ['OptionalCallExpression', 'optional call (?.) requires Node 14'],
  ['ClassPrivateMethod', 'private class methods (#m()) require Node 14.6'],
  ['ImportExpression', 'dynamic import() requires Node 12.17 in CommonJS'],
];

/** @param {unknown} node @param {boolean} inAsync @param {string[]} problems */
function visit(node, inAsync, problems) {
  if (node == null || typeof node !== 'object') {
    return;
  }

  let n = /** @type {import('@babel/types').Node} */ (node);
  if (typeof n.type === 'string') {
    for (let [type, message] of UNSUPPORTED) {
      if (n.type === type) {
        problems.push(`${n.type} at ${loc(n)}: ${message}`);
      }
    }

    switch (n.type) {
      case 'LogicalExpression':
        if (n.operator === '??') {
          problems.push(`?? at ${loc(n)}: nullish coalescing requires Node 14`);
        }
        break;
      case 'AssignmentExpression':
        if (n.operator === '??=' || n.operator === '||=' || n.operator === '&&=') {
          problems.push(`${n.operator} at ${loc(n)}: logical assignment requires Node 15`);
        }
        break;
      case 'CallExpression':
        if (n.callee.type === 'Import') {
          problems.push(`import() at ${loc(n)}: dynamic import requires Node 12.17 in CommonJS`);
        }
        break;
      case 'AwaitExpression':
        if (!inAsync) {
          problems.push(`await at ${loc(n)}: top-level await requires Node 14.8`);
        }
        break;
      case 'ForOfStatement':
        if (n.await && !inAsync) {
          problems.push(`for await at ${loc(n)}: top-level await requires Node 14.8`);
        }
        break;
      case 'NumericLiteral':
        // @ts-ignore - extra is not in the public types.
        if (n.extra && /_/.test(n.extra.raw)) {
          problems.push(`numeric separator at ${loc(n)}: numeric separators require Node 12.5`);
        }
        break;
    }

    if (/^(FunctionDeclaration|FunctionExpression|ObjectMethod|ClassMethod|ClassPrivateMethod|ArrowFunctionExpression)$/.test(n.type)) {
      inAsync = !!/** @type {any} */ (n).async;
    }
  }

  for (let key of Object.keys(n)) {
    if (key === 'loc' || key === 'start' || key === 'end' || key === 'extra' || key === 'leadingComments' || key === 'trailingComments' || key === 'innerComments') {
      continue;
    }
    let value = /** @type {any} */ (n)[key];
    if (Array.isArray(value)) {
      for (let item of value) {
        visit(item, inAsync, problems);
      }
    } else {
      visit(value, inAsync, problems);
    }
  }
}

/** @param {import('@babel/types').Node} node */
function loc(node) {
  return node.loc ? `line ${node.loc.start.line}` : 'unknown location';
}

for (let file of fs.readdirSync(dir).sort()) {
  if (!file.endsWith('.js') && !file.endsWith('.mjs')) {
    continue;
  }

  test(`${file} parses on Node 12`, () => {
    let code = fs.readFileSync(path.join(dir, file), 'utf8');
    let ast = parse(code, {
      sourceType: file.endsWith('.mjs') ? 'module' : 'script',
    });

    let problems = [];
    visit(ast.program, false, problems);
    assert.equal(problems, []);
  });
}

test.run();
