// #747: custom `resolver` for the synchronous bundle() API.
// Files are virtual (served by resolver.read), so nothing touches disk.
// check() = the resolver is honored (the requested feature).
const files = {
  '/virtual/main.css': '@import "pkg";\n.main { color: red }',
  '/virtual/node_modules/pkg/index.css': '.pkg { color: blue }',
};
const resolver = (log) => ({
  read(file) { log.push(`read ${file}`); return files[file]; },
  resolve(specifier, from) {
    log.push(`resolve ${specifier}`);
    return specifier === 'pkg' ? '/virtual/node_modules/pkg/index.css' : specifier;
  },
});
const ok = (o) => o.includes('.pkg{color:#00f}') && o.includes('.main{color:red}');

export const cases = [
  {
    name: 'bundle-sync-with-resolver',
    run: (l) => {
      let log = [];
      try {
        let res = l.bundle({filename: '/virtual/main.css', minify: true, resolver: resolver(log)});
        return res.code.toString() + `\n/* ${log.join(', ')} */`;
      } catch (e) {
        return `ERROR: ${String(e.message || e).split('\n')[0].replace(/\/[^ '"]*virtual/, '/virtual')} /* calls: ${log.length} */`;
      }
    },
    check: ok,
  },
  {
    // Control: the async API supports the same resolver.
    name: 'control-bundleAsync-with-resolver',
    run: async (l) => {
      let log = [];
      let res = await l.bundleAsync({filename: '/virtual/main.css', minify: true, resolver: resolver(log)});
      return res.code.toString() + `\n/* ${log.join(', ')} */`;
    },
    check: ok,
  },
];
