const fs = require('fs');

let dir = `${__dirname}/../`;
let index = fs.readFileSync(dir + '/node/index.d.ts', 'utf8');
index = '// @flow\n' + index;
index = index.replace(/export interface (.*?)(?: extends (.+?))? \{((?:.|\n)*?)\}/g, (_, name, ext, contents) => `export type ${name} =${ext ? ` ${ext} &` : ''} {|${contents}|};`);
index = index.replace(/export declare function/g, 'declare export function');
index = index.replace("Omit<TransformOptions, 'code'>", "$Rest<TransformOptions, {|code: TransformOptions['code']|}>");

let targets = fs.readFileSync(dir + '/node/targets.d.ts', 'utf8');
targets = targets.replace(/export interface (.*?) \{((?:.|\n)*?)\}/g, 'export type $1 = {|$2|};');
index = index.replace(/import type \{\s*Targets\s*\} from '.\/targets';/, targets);

fs.writeFileSync(dir + '/node/index.js.flow', index);
