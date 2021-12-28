# @parcel/css

A CSS parser, transformer, and minifier written in Rust.

## Features

- **Extremely fast** – Parsing and minifying large files is completed in milliseconds, often with significantly smaller output than other tools. See [benchmarks](#benchmarks) below.
- **Typed property values** – many other CSS parsers treat property values as an untyped series of tokens. This means that each transformer that wants to do something with these values must interpret them itself, leading to duplicate work and inconsistencies. `@parcel/css` parses all values using the grammar from the CSS specification, and exposes a specific value type for each property.
- **Browser-grade parser** – `@parcel/css` is built on the [cssparser](https://github.com/servo/rust-cssparser) and [selectors](https://github.com/servo/servo/tree/master/components/selectors) crates created by Mozilla and used by Firefox and Servo. These provide a solid general purpose CSS-parsing foundation on top of which `@parcel/css` implements support for all specific CSS rules and properties.
- **Minification** – One of the main purposes of `@parcel/css` is to minify CSS to make it smaller. This includes many optimizations including:
  - Combining longhand properties into shorthands where possible.
  - Merging adjacent rules with the same selectors or declarations when it is safe to do so.
  - Combining CSS transforms into a single matrix or visa versa when smaller.
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

### From JavaScript

More docs to come, but here is a simple example:

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

### From Rust

See the Rust API docs on [docs.rs](https://docs.rs/parcel_css).

### With Parcel

Add the following to your `.parcelrc`:

```json
{
  "extends": "@parcel/config-default",
  "optimizers": {
    "*.css": ["@parcel/optimizer-css"]
  }
}
```

## Benchmarks

```
$ node bench.js bootstrap-4.css 
cssnano: 542.956ms
159636 bytes

esbuild: 17.411ms
160332 bytes

parcel-css: 4.74ms
143985 bytes


$ node bench.js animate.css
cssnano: 283.105ms
71723 bytes

esbuild: 11.858ms
72183 bytes

parcel-css: 1.989ms
23666 bytes


$ node bench.js tailwind.css 
cssnano: 2.198s
1925626 bytes

esbuild: 107.668ms
1961642 bytes

parcel-css: 45.701ms
1799209 bytes
```
