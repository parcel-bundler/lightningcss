# @parcel/css

A CSS parser, transformer, and minifier written in Rust.

## Features

- **Extremely fast** – Parsing and minifying large files is completed in milliseconds, often with significantly smaller output than other tools. See [benchmarks](#benchmarks) below.
- **Typed property values** – many other CSS parsers treat property values as an untyped series of tokens. This means that each transformer that wants to do something with these values must interpret them itself, leading to duplicate work and inconsistencies. `@parcel/css` parses all values using the grammar from the CSS specification, and exposes a specific value type for each property.
- **Browser-grade parser** – `@parcel/css` is built on the [cssparser](https://github.com/servo/rust-cssparser) and [selectors](https://github.com/servo/servo/tree/master/components/selectors) crates created by Mozilla and used by Firefox and Servo. These provide a solid general purpose CSS-parsing foundation on top of which `@parcel/css` implements support for all specific CSS rules and properties.
- **Minification** – One of the main purposes of `@parcel/css` is to minify CSS to make it smaller. This includes many optimizations including:
  - Combining longhand properties into shorthands where possible.
  - Merging adjacent rules with the same selectors or declarations when it is safe to do so.
  - Combining CSS transforms into a single matrix or vice versa when smaller.
  - Removing vendor prefixes that are not needed, based on the provided browser targets.
  - Reducing `calc()` expressions where possible.
  - Converting colors to shorter hex notation where possible.
  - Minifying gradients.
  - Minifying CSS grid templates.
  - Normalizing property value order.
  - Removing default property sub-values which will be inferred by browsers.
  - Many micro-optimizations, e.g. converting to shorter units, removing unnecessary quotation marks, etc.
- **Vendor prefixing** – `@parcel/css` accepts a list of browser targets, and automatically adds (and removes) vendor prefixes.
- **Syntax lowering** – `@parcel/css` parses modern CSS syntax, and generates more compatible output where needed, based on browser targets.
  - CSS Nesting (draft spec)
  - Custom media queries (draft spec)
  - Logical properties
  - CSS Level 4 Color syntax
    - Space separated components in `rgb` and `hsl` functions
    - Hex with alpha syntax
    - `hwb()` color syntax
    - Percent syntax for opacity
  - Double position gradient stops (e.g. `red 40% 80%`)
  - `clamp()` function
  - Alignment shorthands (e.g. `place-items`)
  - Two-value `overflow` shorthand
  - Media query range syntax (e.g. `@media (width <= 100px)` or `@media (100px < width < 500px)`)
  - Multi-value `display` property (e.g. `inline flex`)
- **CSS modules** – `@parcel/css` supports compiling a subset of [CSS modules](https://github.com/css-modules/css-modules) features.
  - Locally scoped class and id selectors
  - Locally scoped custom identifiers, e.g. `@keyframes` names, grid lines/areas, `@counter-style` names, etc.
  - `:local()` and `:global()` selectors
  - The `composes` property

## Documentation

`@parcel/css` can be used from [Parcel](https://parceljs.org), as a standalone library from JavaScript or Rust, or wrapped as a plugin within any other tool.

### From Node

See the [TypeScript definitions](https://github.com/parcel-bundler/parcel-css/blob/master/node/index.d.ts) for full API docs.

Here is a simple example that compiles the input CSS for Safari 13.2, and minifies the output.

```js
const css = require('@parcel/css');

let {code, map} = css.transform({
  filename: 'style.css',
  code: Buffer.from('.foo { color: red }'),
  minify: true,
  sourceMap: true,
  targets: {
    // Semver versions are represented using a single 24-bit number, with one component per byte.
    // e.g. to represent 13.2.0, the following could be used.
    safari: (13 << 16) | (2 << 8)
  }
});
```

You can also convert the results of running `browserslist` into targets which can be passed to `@parcel/css`:

```js
const browserslist = require('browserslist');
const css = require('@parcel/css');

let targets = css.browserslistToTargets(browserslist('>= 0.25%'));
```

### From Rust

See the Rust API docs on [docs.rs](https://docs.rs/parcel_css). More docs and examples are coming soon. For now, start with the [StyleSheet](https://docs.rs/parcel_css/1.0.0-alpha.10/parcel_css/stylesheet/struct.StyleSheet.html) API.

### With Parcel

Add the following to your `.parcelrc`:

```json
{
  "extends": "@parcel/config-default",
  "transformers": {
    "*.css": ["@parcel/transformer-css-experimental"]
  },
  "optimizers": {
    "*.css": ["@parcel/optimizer-css"]
  }
}
```

You should also add a `browserslist` property to your `package.json`, which defines the target browsers that your CSS will be compiled for.

While Parcel CSS handles the most commonly used PostCSS plugins like `autoprefixer`, `postcss-preset-env`, and CSS modules, you may still need PostCSS for more custom plugins like TailwindCSS. If that's the case, just add @parcel/transformer-postcss before @parcel/transformer-css-experimental, and your PostCSS config will be picked up automatically. You can remove the plugins listed above from your PostCSS config, and they'll be handled by Parcel CSS.

You can also configure Parcel CSS in the `package.json` in the root of your project. Currently, three options are supported: `drafts`, which can be used to enable CSS nesting and custom media queries, `pseudoClasses`, which allows replacing some pseudo classes like `:focus-visible` with normal classes that can be applied via JavaScript (e.g. polyfills), and `cssModules`, which enables CSS modules globally rather than only for files ending in `.module.css`.

```json
{
  "@parcel/transformer-css": {
    "cssModules": true,
    "drafts": {
      "nesting": true,
      "customMedia": true
    },
    "pseudoClasses": {
      "focusVisible": "focus-ring"
    }
  }
}
```

### From Deno or in browser

The `@parcel/css-wasm` package can be used in Deno or directly in browsers. This uses a WebAssembly build of Parcel CSS. Use `TextEncoder` and `TextDecoder` convert code from a string to a typed array and back.

```js
import init, {transform} from 'https://unpkg.com/@parcel/css-wasm';

await init();

let {code, map} = transform({
  filename: 'style.css',
  code: new TextEncoder().encode('.foo { color: red }'),
  minify: true,
});

console.log(new TextDecoder().decode(code));
```

## Benchmarks

<img width="666" alt="chart" src="https://user-images.githubusercontent.com/19409/149202953-fe174902-aba1-4f3e-babb-f02a30f92b91.png#gh-light-mode-only">
<img width="666" alt="chart" src="https://user-images.githubusercontent.com/19409/149202998-861c738d-70d6-454c-9cb0-7cdc6733b5c0.png#gh-dark-mode-only">

```
$ node bench.js bootstrap-4.css 
cssnano: 542.956ms
159636 bytes

esbuild: 17.411ms
160332 bytes

parcel-css: 4.602ms
143154 bytes


$ node bench.js animate.css
cssnano: 283.105ms
71723 bytes

esbuild: 11.858ms
72183 bytes

parcel-css: 1.973ms
23666 bytes


$ node bench.js tailwind.css 
cssnano: 2.198s
1925626 bytes

esbuild: 107.668ms
1961642 bytes

parcel-css: 43.368ms
1824130 bytes
```
