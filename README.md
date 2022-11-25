# ⚡️ Lightning CSS

An extremely fast CSS parser, transformer, and minifier written in Rust. Use it with [Parcel](https://parceljs.org), as a standalone library or CLI, or via a plugin with any other tool.

<img width="680" alt="performance and build size charts" src="https://user-images.githubusercontent.com/19409/189022599-28246659-f94a-46a4-9de0-b6d17adb0e22.png#gh-light-mode-only">
<img width="680" alt="performance and build size charts" src="https://user-images.githubusercontent.com/19409/189022693-6956b044-422b-4f56-9628-d59c6f791095.png#gh-dark-mode-only">

## Features

- **Extremely fast** – Parsing and minifying large files is completed in milliseconds, often with significantly smaller output than other tools. See [benchmarks](#benchmarks) below.
- **Typed property values** – many other CSS parsers treat property values as an untyped series of tokens. This means that each transformer that wants to do something with these values must interpret them itself, leading to duplicate work and inconsistencies. Lightning CSS parses all values using the grammar from the CSS specification, and exposes a specific value type for each property.
- **Browser-grade parser** – Lightning CSS is built on the [cssparser](https://github.com/servo/rust-cssparser) and [selectors](https://github.com/servo/servo/tree/master/components/selectors) crates created by Mozilla and used by Firefox and Servo. These provide a solid general purpose CSS-parsing foundation on top of which Lightning CSS implements support for all specific CSS rules and properties.
- **Minification** – One of the main purposes of Lightning CSS is to minify CSS to make it smaller. This includes many optimizations including:
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
- **Vendor prefixing** – Lightning CSS accepts a list of browser targets, and automatically adds (and removes) vendor prefixes.
- **Browserslist configuration** – Lightning CSS supports opt-in browserslist configuration discovery to resolve browser targets and integrate with your existing tools and config setup.
- **Syntax lowering** – Lightning CSS parses modern CSS syntax, and generates more compatible output where needed, based on browser targets.
  - CSS Nesting (draft spec)
  - Custom media queries (draft spec)
  - Logical properties
  * [Color Level 5](https://drafts.csswg.org/css-color-5/)
    - `color-mix()` function
    - Relative color syntax, e.g. `lab(from purple calc(l * .8) a b)`
  - [Color Level 4](https://drafts.csswg.org/css-color-4/)
    - `lab()`, `lch()`, `oklab()`, and `oklch()` colors
    - `color()` function supporting predefined color spaces such as `display-p3` and `xyz`
    - Space separated components in `rgb` and `hsl` functions
    - Hex with alpha syntax
    - `hwb()` color syntax
    - Percent syntax for opacity
    - `#rgba` and `#rrggbbaa` hex colors
  - Selectors
    - `:not` with multiple arguments
    - `:lang` with multiple arguments
    - `:dir`
    - `:is`
  - Double position gradient stops (e.g. `red 40% 80%`)
  - `clamp()`, `round()`, `rem()`, and `mod()` math functions
  - Alignment shorthands (e.g. `place-items`)
  - Two-value `overflow` shorthand
  - Media query range syntax (e.g. `@media (width <= 100px)` or `@media (100px < width < 500px)`)
  - Multi-value `display` property (e.g. `inline flex`)
  - `system-ui` font family fallbacks
- **CSS modules** – Lightning CSS supports compiling a subset of [CSS modules](https://github.com/css-modules/css-modules) features.
  - Locally scoped class and id selectors
  - Locally scoped custom identifiers, e.g. `@keyframes` names, grid lines/areas, `@counter-style` names, etc.
  - Opt-in support for locally scoped CSS variables and other dashed identifiers.
  - `:local()` and `:global()` selectors
  - The `composes` property

## Documentation

Lightning CSS can be used from [Parcel](https://parceljs.org), as a standalone library from JavaScript or Rust, using a standalone CLI, or wrapped as a plugin within any other tool.

### From Node

See the [TypeScript definitions](https://github.com/parcel-bundler/lightningcss/blob/master/node/index.d.ts) for full API docs.

Here is a simple example that compiles the input CSS for Safari 13.2, and minifies the output.

```js
const css = require('lightningcss');

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

You can also convert the results of running `browserslist` into targets which can be passed to Lightning CSS:

```js
const browserslist = require('browserslist');
const css = require('lightningcss');

let targets = css.browserslistToTargets(browserslist('>= 0.25%'));
```

Bundling is also possible by using the `bundle` API. This processes `@import` rules and inlines them. This API requires filesystem access, so it does not accept `code` directly via the API.

```js
let {code, map} = css.bundle({
  filename: 'style.css',
  minify: true
});
```

The `bundleAsync` API is an asynchronous version of `bundle`, which also accepts a custom `resolver` object. This allows you to provide custom JavaScript functions for resolving `@import` specifiers to file paths, and reading files from the file system (or another source). The `read` and `resolve` functions are both optional, and may either return a string synchronously, or a Promise for asynchronous resolution.

```js
let {code, map} = await css.bundleAsync({
  filename: 'style.css',
  minify: true,
  resolver: {
    read(filePath) {
      return fs.readFileSync(filePath, 'utf8');
    },
    resolve(specifier, from) {
      return path.resolve(path.dirname(from), specifier);
    }
  }
});
```

Note that using a custom resolver can slow down bundling significantly, especially when reading files asynchronously. Use `readFileSync` rather than `readFile` if possible for better performance, or omit either of the methods if you don't need to override the default behavior.

### From Rust

See the Rust API docs on [docs.rs](https://docs.rs/lightningcss).

### With Parcel

Parcel includes Lightning CSS as the default CSS transformer. You should also add a `browserslist` property to your `package.json`, which defines the target browsers that your CSS will be compiled for.

While Lightning CSS handles the most commonly used PostCSS plugins like `autoprefixer`, `postcss-preset-env`, and CSS modules, you may still need PostCSS for more custom plugins like TailwindCSS. If that's the case, your PostCSS config will be picked up automatically. You can remove the plugins listed above from your PostCSS config, and they'll be handled by Lightning CSS.

You can also configure Lightning CSS in the `package.json` in the root of your project. Currently, three options are supported: `drafts`, which can be used to enable CSS nesting and custom media queries, `pseudoClasses`, which allows replacing some pseudo classes like `:focus-visible` with normal classes that can be applied via JavaScript (e.g. polyfills), and `cssModules`, which enables CSS modules globally rather than only for files ending in `.module.css`, or accepts an options object.

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

See the [Parcel docs](https://parceljs.org/languages/css) for more details.

### From Deno or in browser

The `lightningcss-wasm` package can be used in Deno or directly in browsers. This uses a WebAssembly build of Lightning CSS. Use `TextEncoder` and `TextDecoder` convert code from a string to a typed array and back.

```js
import init, {transform} from 'https://unpkg.com/lightningcss-wasm';

await init();

let {code, map} = transform({
  filename: 'style.css',
  code: new TextEncoder().encode('.foo { color: red }'),
  minify: true,
});

console.log(new TextDecoder().decode(code));
```

### With webpack

css-minimizer-webpack-plugin has builtin support for Lightning CSS. Install Lightning CSS in your project, and configure the plugin as documented [in its README](https://github.com/webpack-contrib/css-minimizer-webpack-plugin#using-custom-minifier-lightningcss-previously-parcelcss).

### From the CLI

Lightning CSS includes a standalone CLI that can be used to compile, minify, and bundle CSS files. It can be used when you only need to compile CSS, and don't need more advanced functionality from a larger build tool such as code splitting and support for other languages.

To use the CLI, install the `lightningcss-cli` package with an npm compatible package manager:

```shell
npm install lightningcss-cli
```

Then, you can run the `lightningcss` command via `npx`, `yarn`, or by setting up a script in your package.json.

```json
{
  "scripts": {
    "build": "lightningcss --minify --nesting --bundle --targets '>= 0.25%' --sourcemap input.css -o output.css"
  }
}
```

To see all of the available options, use the `--help` argument:

```shell
npx lightningcss --help
```

#### Browserslist configuration

If the `--browserslist` option is provided, then `lightningcss` finds browserslist configuration,
selects queries by environment and loads the resulting queries as targets.

Configuration discovery and targets resolution is modeled after the original `browserslist` nodeJS package.
The configuration is resolved in the following order:

- If a `BROWSERSLIST` environment variable is present, then load targets from its value. This is analog to the `--targets` CLI option.
  _Example:_ `BROWSERSLIST="firefox ESR" lightningcss [OPTIONS] <INPUT_FILE>`
- If a `BROWSERSLIST_CONFIG` environment variable is present, then resolve the file at the provided path.
  Then parse and use targets from `package.json` or any browserslist configuration file pointed to by the environment variable.
  _Example:_ `BROWSERSLIST_CONFIG="../config/browserslist" lightningcss [OPTIONS] <INPUT_FILE>`
- If none of the above apply, then find, parse and use targets from the first `browserslist`, `.browserslistrc`
  or `package.json` configuration file in any parent directory.

Browserslist configuration files may contain sections denoted by angular brackets `[]`.
Use these to specify different targets for different environments.
Targets which are not placed in a section are added to `defaults` and used if no section matches the environment.

_Example:_

```
# Defaults, applied when no other section matches the provided environment.
firefox ESR

[staging]
# Targets applied only to the staging environment.
samsung >= 4
```

When using parsed configuration from `browserslist`, `.browserslistrc` or `package.json` configuration files,
the environment determined by

- the `BROWSERSLIST_ENV` environment variable if present,
- otherwise the `NODE_ENV` environment variable if present,
- otherwise `production` is used.

If no targets are found for the resulting environment, then the `defaults` configuration section is used.

### Error recovery

By default, Lightning CSS is strict, and will error when parsing an invalid rule or declaration. However, sometimes you may encounter a third party library that you can't easily modify, which unintentionally contains invalid syntax, or IE-specific hacks. In these cases, you can enable the `errorRecovery` option (or `--error-recovery` CLI flag). This will skip over invalid rules and declarations, omitting them in the output, and producing a warning instead of an error. You should also open an issue or PR to fix the issue in the library if possible.

## Benchmarks

<img width="680" alt="performance and build size charts" src="https://user-images.githubusercontent.com/19409/189022599-28246659-f94a-46a4-9de0-b6d17adb0e22.png#gh-light-mode-only">
<img width="680" alt="performance and build size charts" src="https://user-images.githubusercontent.com/19409/189022693-6956b044-422b-4f56-9628-d59c6f791095.png#gh-dark-mode-only">

```
$ node bench.js bootstrap-4.css
cssnano: 544.809ms
159636 bytes

esbuild: 17.199ms
160332 bytes

lightningcss: 4.16ms
143091 bytes


$ node bench.js animate.css
cssnano: 283.105ms
71723 bytes

esbuild: 11.858ms
72183 bytes

lightningcss: 1.973ms
23666 bytes


$ node bench.js tailwind.css
cssnano: 2.198s
1925626 bytes

esbuild: 107.668ms
1961642 bytes

lightningcss: 43.368ms
1824130 bytes
```

For more benchmarks comparing more tools and input, see [here](http://goalsmashers.github.io/css-minification-benchmark/). Note that some of the tools shown perform unsafe optimizations that may change the behavior of the original CSS in favor of smaller file size. Lightning CSS does not do this – the output CSS should always behave identically to the input. Keep this in mind when comparing file sizes between tools.
