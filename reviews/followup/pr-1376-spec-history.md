**Two stages:**

1. **Carry-forward of individual analogous components (2022).** Chris Lilley added it on 2022-09-01 in [0da5a5ae7](https://github.com/w3c/csswg-drafts/commit/0da5a5ae7) ("Draft text for carried forward missing values, [#7536](https://github.com/w3c/csswg-drafts/issues/7536)"). The changelog since the [5 Jul 2022 CR](https://www.w3.org/TR/2022/CR-css-color-4-20220705/) says: "Clarified how missing values in colors to be interpolated are carried forward". It was first published in the [1 Nov 2022 CRD](https://www.w3.org/TR/2022/CRD-css-color-4-20221101/). Later refinements: the order relative to powerless handling ([#8602](https://github.com/w3c/csswg-drafts/issues/8602), Jun 2023), and carry-forward before premultiplication (Jan 2024).

2. **"Analogous set" (2026).** The CSSWG resolved this in June 2024 ([#10210 resolution](https://github.com/w3c/csswg-drafts/issues/10210#issuecomment-2162753105)). Lea Verou added the text on 2026-04-10 in [bc56eb2f5](https://github.com/w3c/csswg-drafts/commit/bc56eb2f5), changelog line: "Expanded the concept of analogous components to analogous sets of components, to minimize none → 0 conversions". A wording clarification followed on 2026-07-09 ([#14134](https://github.com/w3c/csswg-drafts/issues/14134), [526bc74ae](https://github.com/w3c/csswg-drafts/commit/526bc74ae)). Both are in the ED and in the [current TR CRD](https://www.w3.org/TR/css-color-4/) (dated 2026-11-23).

**Browsers:** I tested only local Chromium (HeadlessChrome 153):
- Per-component carry-forward works: `color-mix(in lch, lab(none 20 20), lch(80% 30 40))` gives L=80.
- Analogous sets do not: `color-mix(in lch, lab(50% none none), lch(50% 40 120))` gives `lch(50 20 120)`. The spec result is `lch(50 40 120)`.

I couldn't check Firefox, Safari, wpt.fyi or the issue threads (the proxy blocked them).
