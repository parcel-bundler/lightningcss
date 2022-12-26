[[toc]]

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

The following features are supported:

* [Color Level 5](https://drafts.csswg.org/css-color-5/)
  - [`color-mix()`](https://drafts.csswg.org/css-color-5/#color-mix) function
  - Relative color syntax, e.g. `lab(from purple calc(l * .8) a b)`
* [Color Level 4](https://drafts.csswg.org/css-color-4/)
  - [`lab()`](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/lab()), [`lch()`](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/lch()), `oklab()`, and `oklch()` colors
  - [`color()`](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/color()) function supporting predefined color spaces such as `display-p3` and `xyz`
  - [`hwb()`](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/hwb()) function
  - Space separated components in `rgb()` and `hsl()` functions
  - Hex colors with alpha, e.g. `#rgba` and `#rrggbbaa`
* [Logical properties](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Logical_Properties), e.g. `margin-inline-start`
* [Media query range syntax](https://developer.mozilla.org/en-US/docs/Web/CSS/Media_Queries/Using_media_queries#syntax_improvements_in_level_4), e.g. `@media (width <= 100px)` or `@media (100px < width < 500px)`
* Selectors
  - `:not` and `:lang` with multiple arguments
  - `:dir`
  - `:is`
* `clamp()`, `round()`, `rem()`, `mod()`, `abs()`, and `sign()` [math functions](https://w3c.github.io/csswg-drafts/css-values/#math)
* `sin()`, `cos()`, `tan()`, `asin()`, `acos()`, `atan()`, and `atan2()` [trigonometric functions](https://w3c.github.io/csswg-drafts/css-values/#trig-funcs)
* `pow()`, `log()`, `sqrt()`, `exp()`, and `hypot()` [exponential functions](https://w3c.github.io/csswg-drafts/css-values/#exponent-funcs)
* [Double position gradient stops](https://css-tricks.com/while-you-werent-looking-css-gradients-got-better/) (e.g. `red 40% 80%`)
* Two-value [`overflow`](https://developer.mozilla.org/en-US/docs/Web/CSS/overflow) shorthand
* Multi-value [`display`](https://developer.mozilla.org/en-US/docs/Web/CSS/display) property (e.g. `inline flex`)
* Alignment shorthands, e.g. [`place-items`](https://developer.mozilla.org/en-US/docs/Web/CSS/place-items) and [`place-content`](https://developer.mozilla.org/en-US/docs/Web/CSS/place-content)
* `system-ui` font family fallbacks

## Draft syntax

Lightning CSS can also be configured to compile several draft specs that are not yet available natively in any browser. Because these are drafts and the syntax can still change, they must be enabled manually in your project.

### Nesting

The [CSS Nesting](https://drafts.csswg.org/css-nesting/) draft spec enables style rules to be nested, with the selectors of the child rules extending the parent selector in some way. This is very commonly supported by CSS pre-processors like SASS, but with this spec, it will eventually be supported natively in browsers. Lightning CSS compiles this syntax to un-nested style rules that are supported in all browsers today.

Because nesting is a draft, it is not enabled by default. To use it, enable the `nesting` option under `drafts` when calling the Lightning CSS API. When using the CLI, enable the `--nesting` flag.

```js
let { code, map } = transform({
  // ...
  drafts: {
    nesting: true
  }
});
```

Once enabled, any CSS file in your project can use nested style rules.

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

When a nested rule starts with an element selector, it may be ambiguous with a property. In these cases, the spec requires the selector be prefixed with a [nesting selector](https://drafts.csswg.org/css-nesting/#nest-selector) (`&`).

```css
.foo {
  color: red;

  & ul {
    color: blue;
  }
}
```

is equivalent to:

```css
.foo {
  color: red;
}

.foo ul {
  color: blue;
}
```

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
