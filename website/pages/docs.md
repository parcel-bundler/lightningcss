<aside>

[[toc]]

</aside>

# Getting Started

Lightning CSS can be used from [Parcel](https://parceljs.org), as a standalone library from JavaScript or Rust, using a standalone CLI, or wrapped as a plugin within any other tool.

## From Node

See the [TypeScript definitions](https://github.com/parcel-bundler/lightningcss/blob/master/node/index.d.ts) for full API docs.

Here is a simple example that compiles the input CSS for Safari 13.2, and minifies the output.

```js
import { transform } from 'lightningcss';

let { code, map } = transform({
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
import browserslist from 'browserslist';
import { browserslistToTargets } from 'lightningcss';

let targets = browserslistToTargets(browserslist('>= 0.25%'));
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

## From Rust

See the Rust API docs on [docs.rs](https://docs.rs/lightningcss).

## With Parcel

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

## From Deno or in browser

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

## With webpack

css-minimizer-webpack-plugin has builtin support for Lightning CSS. Install Lightning CSS in your project, and configure the plugin as documented [in its README](https://github.com/webpack-contrib/css-minimizer-webpack-plugin#using-custom-minifier-lightningcss-previously-parcelcss).

## From the CLI

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
