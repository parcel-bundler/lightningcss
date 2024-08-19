<aside>

[[toc]]

</aside>

# CSS modules

By default, CSS identifiers are global. If two files define the same class names, ids, custom properties, `@keyframes`, etc., they will potentially clash and overwrite each other. To solve this, Lightning CSS supports [CSS modules](https://github.com/css-modules/css-modules).

CSS modules treat the classes defined in each file as unique. Each class name or identifier is renamed to include a unique hash, and a mapping is exported to JavaScript to allow referencing them.

To enable CSS modules, provide the `cssModules` option when calling the Lightning CSS API. When using the CLI, enable the `--css-modules` flag.

```js
import {transform} from 'lightningcss';

let {code, map, exports} = transform({
  // ...
  cssModules: true,
  code: Buffer.from(`
    .logo {
      background: skyblue;
    }
  `),
});
```

This returns an `exports` object in addition to the compiled code and source map. Each property in the `exports` object maps from the original name in the source CSS to the compiled (i.e. hashed) name. You can use this mapping in your JavaScript or template files to reference the compiled classes and identifiers.

The exports object for the above example might look like this:

```js
{
  logo: {
    name: '8h19c6_logo',
    isReferenced: false,
    composes: []
  }
}
```

## Class composition

Style rules in CSS modules can reference other classes with the `composes` property. This causes the referenced class to be applied whenever the composed class is used, effectively providing a form of style mixins.

```css
.bg-indigo {
  background: indigo;
}

.indigo-white {
  composes: bg-indigo;
  color: white;
}
```

In the above example, whenever the `indigo-white` class is applied, the `bg-indigo` class will be applied as well. This is indicated in the `exports` object returned by Lightning CSS as follows:

```js
{
  'bg-indigo': {
    name: '8h19c6_bg-indigo',
    isReferenced: true,
    composes: []
  },
  'indigo-white': {
    name: '8h19c6_indigo-white',
    isReferenced: false,
    composes: [{
      type: 'local',
      name: '8h19c6_bg-indigo'
    }]
  }
}
```

Multiple classes can be composed at once by separating them with spaces.

```css
.logo {
  composes: bg-indigo padding-large;
}
```

### Dependencies

You can also reference class names defined in a different CSS file using the `from` keyword:

```css
.logo {
  composes: bg-indigo from './colors.module.css';
}
```

This outputs an exports object with the dependency information. It is the caller's responsibility to resolve this dependency and apply the target class name when using the `transform` API. When using the `bundle` API, this is handled automatically.

```js
{
  logo: {
    name: '8h19c6_logo',
    isReferenced: false,
    composes: [{
      type: 'dependency',
      name: 'bg-indigo',
      specifier: './colors.module.css'
    }]
  }
}
```

### Global composition

Global (i.e. non-hashed) classes can also be composed using the `global` keyword:

```css
.search {
  composes: search-widget from global;
}
```

## Global exceptions

Within a CSS module, all class and id selectors are local by default. You can also opt out of this behavior for a single selector using the `:global` pseudo class.

```css
.foo :global(.bar) {
  color: red;
}

.foo .bar {
  color: green;
}
```

compiles to:

```css
.EgL3uq_foo .bar {
  color: red;
}

.EgL3uq_foo .EgL3uq_bar {
  color: #ff0;
}
```

## Local CSS variables

By default, class names, id selectors, and the names of `@keyframes`, `@counter-style`, and CSS grid lines and areas are scoped to the module they are defined in. Scoping for CSS variables and other [`<dashed-ident>`](https://www.w3.org/TR/css-values-4/#dashed-idents) names can also be enabled using the `dashedIdents` option when calling the Lightning CSS API. When using the CLI, enable the `--css-modules-dashed-idents` flag.

```js
let {code, map, exports} = transform({
  // ...
  cssModules: {
    dashedIdents: true,
  },
});
```

When enabled, CSS variables will be renamed so they don't conflict with variable names defined in other files. Referencing a variable uses the standard `var()` syntax, which Lightning CSS will update to match the locally scoped variable name.

```css
:root {
  --accent-color: hotpink;
}

.button {
  background: var(--accent-color);
}
```

becomes:

```css
:root {
  --EgL3uq_accent-color: hotpink;
}

.EgL3uq_button {
  background: var(--EgL3uq_accent-color);
}
```

You can also reference variables defined in other files using the `from` keyword:

```css
.button {
  background: var(--accent-color from './vars.module.css');
}
```

Global variables may be referenced using the `from global` syntax.

```css
.button {
  color: var(--color from global);
}
```

The same syntax also applies to other CSS values that use the [`<dashed-ident>`](https://www.w3.org/TR/css-values-4/#dashed-idents) syntax. For example, the [@font-palette-values](https://drafts.csswg.org/css-fonts-4/#font-palette-values) rule and [font-palette](https://drafts.csswg.org/css-fonts-4/#propdef-font-palette) property use the `<dashed-ident>` syntax to define and refer to custom font color palettes, and will be scoped and referenced the same way as CSS variables.

## Custom naming patterns

By default, Lightning CSS prepends the hash of the filename to each class name and identifier in a CSS file. You can configure this naming pattern using the `pattern` when calling the Lightning CSS API. When using the CLI, provide the `--css-modules-pattern` option.

A pattern is a string with placeholders that will be filled in by Lightning CSS. This allows you to add custom prefixes or adjust the naming convention for scoped classes.

```js
let {code, map, exports} = transform({
  // ...
  cssModules: {
    pattern: 'my-company-[name]-[hash]-[local]',
  },
});
```

The following placeholders are currently supported:

- `[name]` - The base name of the file, without the extension.
- `[hash]` - A hash of the full file path.
- `[content-hash]` - A hash of the file contents.
- `[local]` - The original class name or identifier.

<div class="warning">

### CSS Grid

**Note:** CSS grid line names can be ambiguous due to automatic postfixing done by the browser, which generates line names ending with `-start` and `-end` for each grid template area. When using CSS grid, your `"pattern"` configuration must end with the `[local]` placeholder so that these references work correctly.

```js
let { code, map, exports } = transform({
  // ...
  cssModules: {
    // ❌ [local] must be at the end so that
    // auto-generated grid line names work
    pattern: '[local]-[hash]'
    // ✅ do this instead
    pattern: '[hash]-[local]'
  }
});
```

```css
.grid {
  grid-template-areas: 'nav main';
}

.nav {
  grid-column-start: nav-start;
}
```

</div>


### Pure mode

Just like the `pure` option of the `css-loader` for webpack, Lightning CSS also has a `pure` option that enforces usage of one or more id or class selectors for each rule. 


```js
let {code, map, exports} = transform({
  // ...
  cssModules: {
    pure: true,
  },
});
```

If you enable this option, Lightning CSS will throw an error for CSS rules that don't have at least one id or class selector, like `div`.
This is useful because selectors like `div` are not scoped and affects all elements on the page.



## Turning off feature scoping

Scoping of grid, animations, and custom identifiers can be turned off. By default all of these are scoped.

```js
let {code, map, exports} = transform({
  // ...
  cssModules: {
    animation: true,
    grid: true,
    customIdents: true,
  },
});
```

## Unsupported features

Lightning CSS does not currently implement all CSS modules features available in other implementations. Some of these may be added in the future.

- Non-function syntax for the `:local` and `:global` pseudo classes.
- The `@value` rule – superseded by standard CSS variables.
- The `:import` and `:export` ICSS rules.
