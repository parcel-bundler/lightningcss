<aside>

[[toc]]

</aside>

# Transpilation

Lightning CSS includes support for transpiling modern CSS syntax to support older browsers, including vendor prefixing and syntax lowering.

## Browser targets

By default Lightning CSS does not perform any transpilation of CSS syntax for older browsers. This means that if you write your code using modern syntax or without vendor prefixes, that’s what Lightning CSS will output. You can declare your app’s supported browsers using the `targets` option. When this is declared, Lightning CSS will transpile your code accordingly to ensure compatibility with your supported browsers.

Targets are defined using an object that specifies the minimum version of each browser you want to support. The easiest way to build a targets object is to use [browserslist](https://browserslist.dev). This lets you use a query that automatically updates over time as new browser versions are released, market share changes, etc. The following example will return a targets object listing browsers with >= 0.25% market share.

```js
import browserslist from 'browserslist';
import { transform, browserslistToTargets } from 'lightningcss';

// Call this once per build.
let targets = browserslistToTargets(browserslist('>= 0.25%'));

// Use `targets` for each file you transform.
let { code, map } = transform({
  // ...
  targets
});
```

For the best performance, you should call browserslist once for your whole build process, and reuse the same `targets` object when calling `transform` for each file.

Under the hood, `targets` are represented using an object that maps browser names to minimum versions. Version numbers are represented using a single 24-bit number, with one semver component (major, minor, patch) per byte. For example, this targets object would represent Safari 13.2.0.

```js
let targets = {
  safari: (13 << 16) | (2 << 8)
};
```

### CLI

When using the CLI, targets can be provided by passing a [browserslist](https://browserslist.dev) query to the `--targets` option. Alternatively, if the `--browserslist` option is provided, then `lightningcss` finds browserslist configuration, selects queries by environment and loads the resulting queries as targets.

Configuration discovery and targets resolution is modeled after the original `browserslist` Node package. The configuration is resolved in the following order:

- If a `BROWSERSLIST` environment variable is present, then load targets from its value.
- If a `BROWSERSLIST_CONFIG` environment variable is present, then load the browserslist configuration from the file at the provided path.
- If none of the above apply, then find, parse and use targets from the first `browserslist`, `.browserslistrc`, or `package.json` configuration file in any parent directory.

Browserslist configuration files may contain sections denoted by square brackets. Use these to specify different targets for different environments. Targets which are not placed in a section are added to `defaults` and used if no section matches the environment.

```ini
# Defaults, applied when no other section matches the provided environment.
firefox ESR

# Targets applied only to the staging environment.
[staging]
samsung >= 4
```

When using parsed configuration from `browserslist`, `.browserslistrc`, or `package.json` configuration files, the environment is determined by:

- the `BROWSERSLIST_ENV` environment variable if present
- the `NODE_ENV` environment variable if present
- otherwise `"production"` is used.

If no targets are found for the resulting environment, then the `defaults` configuration section is used.

### Feature flags

In most cases, setting the `targets` option and letting Lightning CSS automatically compile your CSS works great. However, in other cases you might need a little more control over exactly what features are compiled and which are not. That's where the `include` and `exclude` options come in.

The `include` and `exclude` options allow you to explicitly turn on or off certain features. These override the defaults based on the provided browser targets. For example, you might want to only compile colors, and handle auto prefixing or other features with another tool. Or you may want to handle everything except vendor prefixing with Lightning CSS. These options make that possible.

The `include` and `exclude` options are configured using the `Features` enum, which can be imported from `lightningcss`. You can bitwise OR multiple flags together to turn them on or off.

```js
import { transform, Features } from 'lightningcss';

let { code, map } = transform({
  // ...
  targets,
  // Always compile colors and CSS nesting, regardless of browser targets.
  include: Features.Colors | Features.Nesting,
  // Never add any vendor prefixes, regardless of targets.
  exclude: Features.VendorPrefixes
});
```

Here is a full list of available flags, described in the sections below:

<div class="features">

* `Nesting`
* `NotSelectorList`
* `DirSelector`
* `LangSelectorList`
* `IsSelector`
* `TextDecorationThicknessPercent`
* `MediaIntervalSyntax`
* `MediaRangeSyntax`
* `CustomMediaQueries`
* `ClampFunction`
* `ColorFunction`
* `OklabColors`
* `LabColors`
* `P3Colors`
* `HexAlphaColors`
* `SpaceSeparatedColorNotation`
* `LightDark`
* `FontFamilySystemUi`
* `DoublePositionGradients`
* `VendorPrefixes`
* `LogicalProperties`
* `Selectors` – shorthand for `Nesting | NotSelectorList | DirSelector | LangSelectorList | IsSelector`
* `MediaQueries` – shorthand for `MediaIntervalSyntax | MediaRangeSyntax | CustomMediaQueries`
* `Colors` – shorthand for `ColorFunction | OklabColors | LabColors | P3Colors | HexAlphaColors | SpaceSeparatedColorNotation | LightDark`

</div>

## Vendor prefixing

Based on your configured browser targets, Lightning CSS automatically adds vendor prefixed fallbacks for many CSS features. For example, when using the [`image-set()`](https://developer.mozilla.org/en-US/docs/Web/CSS/image/image-set()) function, Lightning CSS will output a fallback `-webkit-image-set()` value as well, since Chrome does not yet support the unprefixed value.

```css
.logo {
  background: image-set(url(logo.png) 2x, url(logo.png) 1x);
}
```

compiles to:

```css
.logo {
  background: -webkit-image-set(url(logo.png) 2x, url(logo.png) 1x);
  background: image-set("logo.png" 2x, "logo.png");
}
```

In addition, if your CSS source code (or more likely a library) includes unnecessary vendor prefixes, Lightning CSS will automatically remove them to reduce bundle sizes. For example, when compiling for modern browsers, prefixed versions of the `transition` property will be removed, since the unprefixed version is supported by all browsers.

```css
.button {
  -webkit-transition: background 200ms;
  -moz-transition: background 200ms;
  transition: background 200ms;
}
```

becomes:

```css
.button {
  transition: background .2s;
}
```

## Syntax lowering

Lightning CSS automatically compiles many modern CSS syntax features to more compatible output that is supported in your target browsers.

### Nesting

The [CSS Nesting](https://drafts.csswg.org/css-nesting/) spec enables style rules to be nested, with the selectors of the child rules extending the parent selector in some way. This is very commonly supported by CSS pre-processors like Sass, but with this spec, it will eventually be supported natively in browsers. Lightning CSS compiles this syntax to un-nested style rules that are supported in all browsers today.

```css
.foo {
  color: blue;

  .bar {
    color: red;
  }
}
```

is equivalent to:

```css
.foo {
  color: blue;
}

.foo .bar {
  color: red;
}
```

[Conditional rules](https://drafts.csswg.org/css-nesting/#conditionals) such as `@media` may also be nested within a style rule, without repeating the selector. For example:

```css
.foo {
  display: grid;

  @media (orientation: landscape) {
    grid-auto-flow: column;
  }
}
```

is equivalent to:

```css
.foo {
  display: grid;
}

@media (orientation: landscape) {
  .foo {
    grid-auto-flow: column;
  }
}
```

### Color mix

The [`color-mix()`](https://drafts.csswg.org/css-color-5/#color-mix) function allows you to mix two colors by the specified amount in a certain color space. Lightning CSS will evaluate this function statically when all components are known (i.e. not variables).

```css
.foo {
  color: color-mix(in hsl, hsl(120deg 10% 20%) 25%, hsl(30deg 30% 40%));
}
```

compiles to:

```css
.foo {
  color: #706a43;
}
```

### Relative colors

Relative colors allow you to modify the components of a color using math functions. In addition, you can convert colors between color spaces. Lightning CSS performs these calculations statically when all components are known (i.e. not variables).

This example lightens `slateblue` by 10% in the LCH color space.

```css
.foo {
  color: lch(from slateblue calc(l * 1.1) c h);
}
```

compiles to:

```css
.foo {
  color: lch(49.0282% 65.7776 296.794);
}
```

### LAB colors

Lightning CSS will convert [`lab()`](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/lab()), [`lch()`](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/lch()), [`oklab()`](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/oklab), and [`oklch()`](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/oklch) colors to fallback values for unsupported browsers when needed. These functions allow you to define colors in higher gamut color spaces, making it possible to use colors that cannot be represented by RGB.

```css
.foo {
  color: lab(40% 56.6 39);
}
```

compiles to:

```css
.foo {
  color: #b32323;
  color: color(display-p3 .643308 .192455 .167712);
  color: lab(40% 56.6 39);
}
```

As shown above, a `display-p3` fallback is included in addition to RGB when a target browser supports the P3 color space. This preserves high color gamut colors when possible.

### Color function

Lightning CSS converts the [`color()`](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/color()) function to RGB when needed for compatibility with older browsers. This allows you to use predefined color spaces such as `display-p3`, `xyz`, and `a98-rgb`.

```css
.foo {
  color: color(a98-rgb 0.44091 0.49971 0.37408);
}
```

compiles to:

```css
.foo {
  color: #6a805d;
  color: color(a98-rgb .44091 .49971 .37408);
}
```

### HWB colors

Lightning CSS converts [`hwb()`](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/hwb) colors to RGB.

```css
.foo {
  color: hwb(194 0% 0%);
}
```

compiles to:

```css
.foo {
  color: #00c4ff;
}
```

### Color notation

Space separated color notation is converted to hex when needed. Hex colors with alpha are also converted to `rgba()` when unsupported by all targets.

```css
.foo {
  color: #7bffff80;
  background: rgb(123 255 255);
}
```

compiles to:

```css
.foo {
  color: rgba(123, 255, 255, .5);
  background: #7bffff;
}
```

### light-dark() color function

The [`light-dark()`](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/light-dark) function allows you to specify a light mode and dark mode color in a single declaration, without needing to write a separate media query rule. In addition, it uses the [`color-scheme`](https://developer.mozilla.org/en-US/docs/Web/CSS/color-scheme) property to control which theme to use, which allows you to set it programmatically. The `color-scheme` property also inherits so themes can be nested and the nearest ancestor color scheme applies.

Lightning CSS converts the `light-dark()` function to use CSS variable fallback when your browser targets don't support it natively. For this to work, you must set the `color-scheme` property on an ancestor element. The following example shows how you can support both operating system and programmatic overrides for the color scheme.

```css
html {
  color-scheme: light dark;
}

html[data-theme=light] {
  color-scheme: light;
}

html[data-theme=dark] {
  color-scheme: dark;
}

button {
  background: light-dark(#aaa, #444);
}
```

compiles to:

```css
html {
  --lightningcss-light: initial;
  --lightningcss-dark: ;
  color-scheme: light dark;
}

@media (prefers-color-scheme: dark) {
  html {
    --lightningcss-light: ;
    --lightningcss-dark: initial;
  }
}

html[data-theme="light"] {
  --lightningcss-light: initial;
  --lightningcss-dark: ;
  color-scheme: light;
}

html[data-theme="dark"] {
  --lightningcss-light: ;
  --lightningcss-dark: initial;
  color-scheme: dark;
}

button {
  background: var(--lightningcss-light, #aaa) var(--lightningcss-dark, #444);
}
```

### Logical properties

CSS [logical properties](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Logical_Properties) allow you to define values in terms of writing direction, so that UIs mirror in right-to-left languages. Lightning CSS will compile these to use the `:dir()` selector when unsupported. If the `:dir()` selector is unsupported, it is compiled as described [below](#%3Adir()-selector).

```css
.foo {
  border-start-start-radius: 20px
}
```

compiles to:

```css
.foo:dir(ltr) {
  border-top-left-radius: 20px;
}

.foo:dir(rtl) {
  border-top-right-radius: 20px;
}
```


### :dir() selector

The [`:dir()`](https://developer.mozilla.org/en-US/docs/Web/CSS/:dir) selector matches elements based on the writing direction. Lightning CSS compiles this to use the `:lang()` selector when unsupported, which approximates this behavior as closely as possible.

```css
a:dir(rtl) {
  color:red
}
```

compiles to:

```css
a:lang(ae, ar, arc, bcc, bqi, ckb, dv, fa, glk, he, ku, mzn, nqo, pnb, ps, sd, ug, ur, yi) {
  color: red;
}
```

If multiple arguments to `:lang()` are unsupported, it is compiled as described [below](#%3Alang()-selector).

### :lang() selector

The [`:lang()`](https://developer.mozilla.org/en-US/docs/Web/CSS/:lang) selector matches elements based on their language. Some browsers do not support multiple arguments to this function, so Lightning CSS compiles them to use `:is()` when needed.

```css
a:lang(en, fr) {
  color:red
}
```

compiles to:

```css
a:is(:lang(en), :lang(fr)) {
  color: red;
}
```

When the `:is()` selector is unsupported, it is compiled as described [below](#%3Ais()-selector).

### :is() selector

The [`:is()`](https://developer.mozilla.org/en-US/docs/Web/CSS/:is) matches when one of its arguments matches. Lightning CSS falls back to the `:-webkit-any` and `:-moz-any` prefixed selectors.

```css
p:is(:first-child, .lead) {
  margin-top: 0;
}
```

compiles to:

```css
p:-webkit-any(:first-child, .lead) {
  margin-top: 0;
}

p:-moz-any(:first-child, .lead) {
  margin-top: 0;
}

p:is(:first-child, .lead) {
  margin-top: 0;
}
```

<div class="warning">

**Note**: The prefixed versions of these selectors do not support complex selectors (e.g. selectors with combinators). Lightning CSS will only output prefixes if the arguments are simple selectors. Complex selectors in `:is()` are not currently compiled.

</div>

### :not() selector

The [`:not()`](https://developer.mozilla.org/en-US/docs/Web/CSS/:not) selector can accept multiple arguments, and matches if none of the arguments match. Some older browsers only support a single argument, so Lightning CSS compiles this when needed. The `:is` selector is used to ensure the specificity remains the same, with fallback to `-webkit-any` and `-moz-any` as needed (described above).

```css
p:not(:first-child, .lead) {
  margin-top: 1em;
}
```

compiles to:

```css
p:not(:is(:first-child, .lead)) {
  margin-top: 1em;
}
```

### Math functions

Lightning CSS simplifies [math functions](https://w3c.github.io/csswg-drafts/css-values/#math) including `clamp()`, `round()`, `rem()`, `mod()`, `abs()`, and `sign()`, [trigonometric functions](https://w3c.github.io/csswg-drafts/css-values/#trig-funcs) including `sin()`, `cos()`, `tan()`, `asin()`, `acos()`, `atan()`, and `atan2()`, and [exponential functions](https://w3c.github.io/csswg-drafts/css-values/#exponent-funcs) including `pow()`, `log()`, `sqrt()`, `exp()`, and `hypot()` when all arguments are known (i.e. not variables). In addition, the numeric constants `e`, `pi`, `infinity`, `-infinity`, and `NaN` are supported in all calculations.

```css
.foo {
  width: round(calc(100px * sin(pi / 4)), 5px);
}
```

compiles to:

```css
.foo {
  width: 70px;
}
```

### Media query ranges

[Media query range syntax](https://developer.mozilla.org/en-US/docs/Web/CSS/Media_Queries/Using_media_queries#syntax_improvements_in_level_4) allows defining media queries using comparison operators to create ranges and intervals. Lightning CSS compiles this to the corresponding `min` and `max` media features when needed.

```css
@media (480px <= width <= 768px) {
  .foo { color: red }
}
```

compiles to:

```css
@media (min-width: 480px) and (max-width: 768px) {
  .foo { color: red }
}
```

### Shorthands

Lightning CSS compiles the following shorthands to corresponding longhands when the shorthand is not supported in all target browsers:

* Alignment shorthands: [place-items](https://developer.mozilla.org/en-US/docs/Web/CSS/place-items), [place-content](https://developer.mozilla.org/en-US/docs/Web/CSS/place-content), [place-self](https://developer.mozilla.org/en-US/docs/Web/CSS/place-self)
* [Overflow shorthand](https://developer.mozilla.org/en-US/docs/Web/CSS/overflow) with multiple values (e.g. `overflow: hidden auto`)
* [text-decoration](https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration) with thickness, style, color, etc.
* Two value [display](https://developer.mozilla.org/en-US/docs/Web/CSS/display) syntax (e.g. `display: inline flex`)

### Double position gradients

CSS gradients support using two positions in a color stop to repeat the color at two subsequent positions. When unsupported, Lightning CSS compiles it.

```css
.foo {
  background: linear-gradient(green, red 30% 40%, pink);
}
```

compiles to:

```css
.foo {
  background: linear-gradient(green, red 30%, red 40%, pink);
}
```

### system-ui font

The `system-ui` font allows you to use the operating system default font. When unsupported, Lightning CSS compiles it to a font stack that works across major platforms.

```css
.foo {
  font-family: system-ui;
}
```

compiles to:

```css
.foo {
  font-family: system-ui, -apple-system, BlinkMacSystemFont, Segoe UI, Roboto, Noto Sans, Ubuntu, Cantarell, Helvetica Neue;
}
```

## Draft syntax

Lightning CSS can also be configured to compile several draft specs that are not yet available natively in any browser. Because these are drafts and the syntax can still change, they must be enabled manually in your project.

### Custom media queries

Support for [custom media queries](https://drafts.csswg.org/mediaqueries-5/#custom-mq) is included in the Media Queries Level 5 draft spec. This allows you to define media queries that are reused in multiple places within a CSS file. Lightning CSS will perform this substitution ahead of time when this feature is enabled.

For example:

```css
@custom-media --modern (color), (hover);

@media (--modern) and (width > 1024px) {
  .a { color: green; }
}
```

is equivalent to:

```css
@media ((color) or (hover)) and (width > 1024px) {
  .a { color: green; }
}
```

Because custom media queries are a draft, they are not enabled by default. To use them, enable the `customMedia` option under `drafts` when calling the Lightning CSS API. When using the CLI, enable the `--custom-media` flag.

```js
let { code, map } = transform({
  // ...
  drafts: {
    customMedia: true
  }
});
```

## Pseudo class replacement

Lightning CSS supports replacing CSS pseudo classes such as `:focus-visible` with normal CSS classes that can be applied using JavaScript. This makes it possible to polyfill these pseudo classes for older browsers.

```js
let { code, map } = transform({
  // ...
  pseudoClasses: {
    focusVisible: 'focus-visible'
  }
});
```

The above configuration will result in the `:focus-visible` pseudo class in all selectors being replaced with the `.focus-visible` class. This enables you to use a JavaScript [polyfill](https://github.com/WICG/focus-visible), which will apply the `.focus-visible` class as appropriate.

The following pseudo classes may be configured as shown above:

* `hover` – corresponds to the `:hover` pseudo class
* `active` – corresponds to the `:active` pseudo class
* `focus` – corresponds to the `:focus` pseudo class
* `focusVisible` – corresponds to the `:focus-visible` pseudo class
* `focusWithin` – corresponds to the `:focus-within` pseudo class

## Non-standard syntax

For compatibility with other tools, Lightning CSS supports parsing some non-standard CSS syntax. This must be enabled by turning on a flag under the `nonStandard` option.

```js
let { code, map } = transform({
  // ...
  nonStandard: {
    deepSelectorCombinator: true
  }
});

```

Currently the following features are supported:

* `deepSelectorCombinator` – enables parsing the Vue/Angular `>>>` and `/deep/` selector combinators.
