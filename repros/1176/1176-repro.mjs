// #1176: container-type: anchored and @container anchored(fallback: ...)
const css = `.popover{container-type:anchored;position-try-fallbacks:flip-block}
@container anchored(fallback: flip-block){.arrow{inset-block-start:auto}}
@container anchored(fallback: flip-block flip-inline){.arrow{color:red}}
.b{container-type:anchored inline-size}`;
export async function run(l) {
  const out = [];
  for (const errorRecovery of [false, true]) {
    try {
      const r = l.transform({filename: 'a.css', code: Buffer.from(css), minify: true, errorRecovery});
      out.push(`errorRecovery=${errorRecovery}: ${r.code}` + (r.warnings.length ? `\n  warnings: ${r.warnings.map(w => w.message).join(' | ')}` : ''));
    } catch (e) { out.push(`errorRecovery=${errorRecovery}: ERROR ${e.message}`); }
  }
  return out.join('\n');
}
// correct = strict mode parses it and keeps both container types and anchored() queries
export const check = o => !o.includes('ERROR') && o.includes('anchored(fallback:flip-block)') && /container-type:anchored inline-size/.test(o) && !o.includes('warnings');
