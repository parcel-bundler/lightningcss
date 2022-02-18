import {expectType} from 'tsd';
import {CSSModuleExports, Dependency, transform} from './index';

const filename = 'file.css';
const code = Buffer.from('.foo { color: red }');
expectType<Buffer | undefined>(transform({filename, code}).map);
expectType<CSSModuleExports | undefined>(transform({filename, code}).exports);
expectType<Dependency[] | undefined>(transform({filename, code}).dependencies);

const withMap = transform({filename, code, sourceMap: true});
expectType<Buffer>(withMap.map);

const withCssModules = transform({filename, code, cssModules: true});
expectType<CSSModuleExports>(withCssModules.exports);

const withDependencies = transform({filename, code, analyzeDependencies: true});
expectType<Dependency[]>(withDependencies.dependencies);

const allFlags = transform({
  filename,
  code,
  sourceMap: true,
  cssModules: true,
  analyzeDependencies: true,
});
expectType<CSSModuleExports>(allFlags.exports);
expectType<Buffer>(allFlags.map);
expectType<Dependency[]>(allFlags.dependencies);
