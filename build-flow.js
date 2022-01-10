const fs = require('fs');

let index = fs.readFileSync(__dirname + '/node/index.d.ts', 'utf8');
index = '// @flow\n' + index;
index = index.replace(/export interface (.*?) \{((?:.|\n)*?)\}/g, 'export type $1 = {|$2|};');
index = index.replace(/export declare function/g, 'declare export function');

let targets = fs.readFileSync(__dirname + '/node/targets.d.ts', 'utf8');
targets = targets.replace(/export interface (.*?) \{((?:.|\n)*?)\}/g, 'export type $1 = {|$2|};');
index = index.replace("import type {Targets} from './targets';", targets);

fs.writeFileSync(__dirname + '/node/index.js.flow', index);
