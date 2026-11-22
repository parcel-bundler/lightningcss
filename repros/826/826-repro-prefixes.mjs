// #826 (fregante's comment) / #1371: include: Features.VendorPrefixes on `stretch` without targets.
// Panics (unreachable! in size.rs) in releases 1.22.0-1.33.0; fixed on master by #1377 (9d0da3f).
// Kept in its own file because a release panic aborts the process and would hide the other cases.
export const cases = [
  {
    // include VendorPrefixes adds the stretch prefixes without targets.
    name: 'control-include-vendor-prefixes-adds-stretch-prefixes',
    run: l => l.transform({filename: 'a.css', code: Buffer.from(`.a{width:stretch}`), minify: true, include: l.Features.VendorPrefixes}).code.toString(),
    check: out => out.includes('-webkit-fill-available') && out.includes('-moz-available'),
  },
];
