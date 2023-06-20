<aside>

[[toc]]

</aside>

# Minification

Lightning CSS can optimize your CSS to make it smaller, which can help improve the loading performance of your website. When using the Lightning CSS API, enable the `minify` option, or when using the CLI, use the `--minify` flag.

```js
import { transform } from 'lightningcss';

let { code, map } = transform({
  // ...
  minify: true
});
```

## Optimizations

The Lightning CSS minifier includes many optimizations to generate the smallest possible output for all rules, properties, and values in your stylesheet. Lightning CSS does not perform any optimizations that change the behavior of your CSS unless it can prove that it is safe to do so. For example, only adjacent style rules are merged to avoid changing the order and potentially breaking the styles.

### Shorthands

Lightning CSS will combine longhand properties into shorthands when all of the constituent longhand properties are defined. For example:

```css
.foo {
  padding-top: 1px;
  padding-left: 2px;
  padding-bottom: 3px;
  padding-right: 4px;
}
```

minifies to:

```css
.foo{padding:1px 4px 3px 2px}
```

This is supported across most shorthand properties defined in the CSS spec.

### Merge adjacent rules

Lightning CSS will merge adjacent style rules with the same selectors or declarations.

```css
.a {
  color: red;
}

.b {
  color: red;
}

.c {
  color: green;
}

.c {
  padding: 10px;
}
```

becomes:

```css
.a,.b{color:red}.c{color:green;padding:10px}
```

In addition to style rules, Lightning CSS will also merge adjacent `@media`, `@supports`, and `@container` rules with identical queries, and adjacent `@layer` rules with the same layer name.

Lightning CSS will not merge rules that are not adjacent, e.g. if another rule is between rules with the same declarations or selectors. This is because changing the order of the rules could cause the behavior of the compiled CSS to differ from the input CSS.

### Remove prefixes

Lightning CSS will remove vendor prefixed properties that are not needed according to your configured browser targets. This is more likely to affect precompiled libraries that include unused prefixes rather than your own code.

For example, when compiling for modern browsers, prefixed versions of the `transition` property will be removed, since the unprefixed version is supported by all browsers.

```css
.button {
  -webkit-transition: background 200ms;
  -moz-transition: background 200ms;
  transition: background 200ms;
}
```

becomes:

```css
.button{transition:background .2s}
```

See [Transpilation](transpilation.html) for more on how to configure browser targets.

### Reduce calc

Lightning CSS will reduce `calc()` and other math expressions to constant values where possible. When different units are used, the terms are reduced as much as possible.

```css
.foo {
  width: calc(100px * 2);
  height: calc(((75.37% - 63.5px) - 900px) + (2 * 100px));
}
```

minifies to:

```css
.foo{width:200px;height:calc(75.37% - 763.5px)}
```

Note that `calc()` expressions with variables are currently left unmodified by Lightning CSS.

### Minify colors

Lightning CSS will minify colors to the smallest format possible without changing the color gamut. For example, named colors as well as `rgb()` and `hsl()` colors are converted to hex notation, using hex alpha notation when supported by your browser targets.

```css
.foo {
  color: rgba(255, 255, 0, 0.8)
}
```

minifies to:

```css
.foo{color:#ff0c}
```

Note that only colors in the RGB gamut (including HSL and HWB) are converted to hex. Colors in other color spaces such as LAB or P3 are preserved.

In addition to static colors, Lightning CSS also supports many color functions such as `color-mix()` and relative colors. When all components are known, Lightning CSS precomputes the result of these functions and outputs a static color. This both reduces the size and makes the syntax compatible with more browser targets.

```css
.foo {
  color: rgb(from rebeccapurple r calc(g * 2) b);
  background: color-mix(in hsl, hsl(120deg 10% 20%), hsl(30deg 30% 40%));
}
```

minifies to:

```css
.foo{color:#669;background:#545c3d}
```

Note that these conversions cannot be performed when any of the components include CSS variables.

### Normalizing values

Lightning CSS parses all properties and values according to the CSS specification, filling in defaults where appropriate. When minifying, it omits default values where possible since the browser will fill those in when parsing.

```css
.foo {
  background: 0% 0% / auto repeat scroll padding-box border-box red;
}
```

minifies to:

```css
.foo{background:red}
```

In addition to removing defaults, Lightning CSS also omits quotes, whitespace, and optional delimiters where possible. It also converts values to shorter equivalents where possible.

```css
.foo {
  font-weight: bold;
  background-position: center center;
  background-image: url("logo.png");
}
```

minifies to:

```css
.foo{background-image:url(logo.png);background-position:50%;font-weight:700}
```

### CSS grid templates

Lightning CSS will minify the `grid-template-areas` property to remove unnecessary whitespace and placeholders in template strings.

```css
.foo {
  grid-template-areas: "head head"
                       "nav  main"
                       "foot ....";
}
```

minifies to:

```css
.foo{grid-template-areas:"head head""nav main""foot."}
```

### Reduce transforms

Lightning CSS will reduce CSS transform functions to shorter equivalents where possible.

```css
.foo {
  transform: translate(0, 50px);
}
```

minifies to:

```css
.foo{transform:translateY(50px)}
```

In addition, the `matrix()` and `matrix3d()` functions are converted to their equivalent transforms when shorter:

```css
.foo {
  transform: matrix3d(1, 0, 0, 0, 0, 0.707106, 0.707106, 0, 0, -0.707106, 0.707106, 0, 100, 100, 10, 1);
}
```

minifies to:

```css
.foo{transform:translate3d(100px,100px,10px)rotateX(45deg)}
```

When a matrix would be shorter, individual transform functions are converted to a single matrix instead:

```css
.foo {
  transform: translate(100px, 200px) rotate(45deg) skew(10deg) scale(2);
}
```

minifies to:

```css
.foo{transform:matrix(1.41421,1.41421,-1.16485,1.66358,100,200)}
```

## Unused symbols

If you know that certain class names, ids, `@keyframes` rules, CSS variables, or other CSS identifiers are unused (for example as part of a larger full project analysis), you can use the `unusedSymbols` option to remove them.

```js
let { code, map } = transform({
  // ...
  minify: true,
  unusedSymbols: ['foo', 'fade-in', '--color']
});
```

With this configuration, the following CSS:

```css
:root {
  --color: red;
}

.foo {
  color: var(--color);
}

@keyframes fade-in {
  from { opacity: 0 }
  to { opacity: 1 }
}

.bar {
  color: green;
}
```

minifies to:

```css
.bar{color:green}
```
