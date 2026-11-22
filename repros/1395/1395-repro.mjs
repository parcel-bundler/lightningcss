// #1395: relative color hue keyword `h` must be a <number> (css-color-5: "h is a <number> ...
// 90 is equivalent to 90deg"). Chromium 153: tan(h) uses radians, calc(h + 10deg)/min(h, 10deg)
// are invalid, calc(h + 10) is valid.
export const cases = [
  {
    name: 'hue-plus-number-resolved',
    css: `.a{color:hsl(from red calc(h + 180) s l)}`,
    options: {minify: true},
    check: (out) => out === '.a{color:#0ff}' || out === '.a{color:aqua}',
  },
  {
    name: 'hue-plus-number-lowered-for-old-targets',
    css: `.a{color:oklch(from #336699 l c calc(h + 120))}`,
    options: {minify: true, targets: {safari: 15 << 16}},
    check: (out) => !out.includes('from'),
  },
  {
    name: 'tan-h-radians',
    css: `.a{color:lch(from red l c tan(h))}`,
    options: {minify: true},
    // tan(40.8577 rad) ~ 0.0153; tan(40.8577deg) ~ 0.8649 (wrong)
    check: (out) => !out.includes('.864935') && (/ \.01\d*\)/.test(out) || out.includes('tan(h)')),
  },
  {
    name: 'hue-plus-angle-not-folded',
    css: `.a{color:lch(from red l c calc(h + 10deg))}`,
    options: {minify: true},
    check: (out) => !out.includes('50.8577'),
  },
  {
    name: 'min-hue-angle-not-folded',
    css: `.a{color:lch(from red l c min(h, 10deg))}`,
    options: {minify: true},
    check: (out) => out.includes('from'),
  },
  {
    name: 'control-plain-h',
    css: `.a{color:lch(from red l c h)}`,
    options: {minify: true},
    check: (out) => out.includes('lch(54.2905% 106.837 40.8577)'),
  },
  {
    name: 'control-lightness-math',
    css: `.a{color:lch(from red calc(l + 10) c h)}`,
    options: {minify: true},
    check: (out) => out.includes('lch(64.2905% 106.837 40.8577)'),
  },
];
