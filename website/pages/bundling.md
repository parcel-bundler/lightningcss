<aside>

[[toc]]

</aside>

# Bundling

Lightning CSS supports bundling dependencies referenced by CSS `@import` rules into a single output file. When calling the Lightning CSS API, use the `bundle` or `bundleAsync` function instead of `transform`. When using the CLI, enable the `--bundle` flag.

This API requires filesystem access, so it does not accept `code` directly via the API. Instead, the `filename` option is used to read the entry file directly.

```js
import { bundle } from 'lightningcss';

let { code, map } = bundle({
  filename: 'style.css',
  minify: true
});
```

## Dependencies

CSS files can contain dependencies referenced by `@import` syntax, as well as references to classes in other files via [CSS modules](css-modules.html).

### @import

The [`@import`](https://developer.mozilla.org/en-US/docs/Web/CSS/@import) at-rule can be used to inline another CSS file into the same CSS bundle as the containing file. This means that at runtime a separate network request will not be needed to load the dependency. Referenced files should be relative to the containing CSS file.

```css
@import 'other.css';
```

`@import` rules must appear before all other rules in a stylesheet except `@charset` and `@layer` statement rules. Later import rules will cause an error to be emitted.

### CSS modules

Dependencies are also bundled when referencing another file via [CSS modules composition](css-modules.html#dependencies) or [external variables](css-modules.html#local-css-variables). See the linked CSS modules documentation for more details.

## Conditional imports

The `@import` rule can be conditional by appending a media query or `supports()` query. Lightning CSS will preserve this behavior by wrapping the inlined rules in `@media` and `@supports` rules as needed.

```css
/* a.css */
@import "b.css" print;
@import "c.css" supports(display: grid);

.a { color: red }
```

```css
/* b.css */
.b { color: green }
```

```css
/* c.css */
.c { display: grid }
```

compiles to:

```css
@media print {
  .b { color: green }
}

@supports (display: grid) {
  .c { display: grid }
}

.a { color: red }
```

<div class="warning">

**Note**: There are currently two cases where combining conditional rules is unsupported:

1. Importing the same CSS file with only a media query, and again with only a supports query. This would require duplicating all rules in the file.
2. Importing a file with a negated media type (e.g. `not print`) within another file with a negated media type.

</div>

## Cascade layers

Imported CSS rules can also be placed into a CSS cascade layer, allowing you to control the order they apply. Nested imports will be placed into nested layers.

```css
/* a.css */
@import "b.css" layer(foo);
.a { color: red }
```

```css
/* b.css */
@import "c.css" layer(bar);
.b { color: green }
```

```css
/* c.css */
.c { color: green }
```

compiles to:

```css
@layer foo.bar {
  .c { color: green }
}

@layer foo {
  .b { color: green }
}

.a { color: red }
```

<div class="warning">

**Note**: There are two unsupported layer combinations that will currently emit a compiler error:

1. Importing the same CSS file with different layer names. This would require duplicating all imported rules multiple times.
2. Nested anonymous layers.

</div>

## Bundling order

When `@import` rules are processed in browsers, if the same file appears more than once, the _last_ instance applies. This is the opposite from behavior in other languages like JavaScript. Lightning CSS follows this behavior when bundling so that the output behaves the same as if it were not bundled.

```css
/* index.css */
@import "a.css";
@import "b.css";
@import "a.css";
```

```css
/* a.css */
body { background: green }
```

```css
/* b.css */
body { background: red }
```

compiles to:

```css
body { background: green }
```

## Custom resolvers

The `bundleAsync` API is an asynchronous version of `bundle`, which also accepts a custom `resolver` object. This allows you to provide custom JavaScript functions for resolving `@import` specifiers to file paths, and reading files from the file system (or another source). The `read` and `resolve` functions are both optional, and may either return a string synchronously, or a Promise for asynchronous resolution.

```js
import { bundleAsync } from 'lightningcss';

let { code, map } = await bundleAsync({
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
