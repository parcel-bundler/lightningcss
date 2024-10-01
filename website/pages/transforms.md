<aside>

[[toc]]

</aside>

# Custom transforms

The Lightning CSS visitor API can be used to implement custom transform plugins in JavaScript. It is designed to enable custom non-standard extensions to CSS, making your code easier to author while shipping standard CSS to the browser. You can implement extensions such as custom shorthand properties or additional at-rules (e.g. mixins), build time transforms (e.g. convert units, inline constants, etc.), CSS rule analysis, and much more.

Custom transforms have a build time cost: it can be around 2x slower to compile with a JS visitor than without. This means visitors should generally be used to implement custom, non-standard CSS extensions. Common standard transforms such as compiling modern standard CSS features (and draft specs) for older browsers should be done in Rust as part of Lightning CSS itself. Please open an issue if there's a feature we don't handle yet.

## Visitors

Custom transforms are implemented by passing a `visitor` object to the Lightning CSS Node API. A visitor includes one or more functions which are called for specific value types such as `Rule`, `Property`, or `Length`. In general, you should try to be as specific as possible about the types of values you want to handle. This way, Lightning CSS needs to call into JS as infrequently as possible, with the smallest objects possible, which improves performance. See the [TypeScript definitions](https://github.com/parcel-bundler/lightningcss/blob/eb49015cf887ae720b80a2856ccbdf61bf940ef1/node/index.d.ts#L184-L214) for a full list of available visitor functions.

Visitors can return a new value to update it. Each visitor accepts a different type of value, and usually expects the same type in return. This example multiplies all lengths by 2:

```js
import { transform } from 'lightningcss';

let res = transform({
  filename: 'test.css',
  minify: true,
  code: Buffer.from(`
    .foo {
      width: 12px;
    }
  `),
  visitor: {
    Length(length) {
      return {
        unit: length.unit,
        value: length.value * 2
      }
    }
  }
});

assert.equal(res.code.toString(), '.foo{width:24px}');
```

Some visitor functions accept an array as a return value, enabling you to replace one value with multiple, or remove a value by returning an empty array. You can also provide an object instead of a function to further reduce the number of times a visitor is called. For example, when providing a `Property` visitor, you can use an object with keys for specific property names. This improves performance by only calling your visitor function when needed.

This example adds `-webkit-overflow-scrolling: touch` before any `overflow` properties.

```js
let res = transform({
  filename: 'test.css',
  minify: true,
  code: Buffer.from(`
    .foo {
      overflow: auto;
    }
  `),
  visitor: {
    Property: {
      overflow(property) {
        return [{
          property: 'custom',
          value: {
            name: '-webkit-overflow-scrolling',
            value: [{
              type: 'token',
              value: {
                type: 'ident',
                value: 'touch'
              }
            }]
          }
        }, property];
      },
    }
  }
});

assert.equal(res.code.toString(), '.foo{-webkit-overflow-scrolling:touch;overflow:auto}');
```

## Value types

The Lightning CSS AST is very detailed – each CSS property has a specific value type with all parts fully normalized. For example, a shorthand property such as `background` includes values for all of its sub-properties such as `background-color`, `background-image`, `background-position`, etc. This makes it both easier and faster for custom transforms to correctly handle all value types without reimplementing parsing. See the [TypeScript definitions](https://github.com/parcel-bundler/lightningcss/blob/master/node/ast.d.ts) for full documentation of all values.

Known property values can be either _parsed_ or _unparsed_. Parsed values are fully expanded following the CSS specification. Unparsed values could not be parsed according to the grammar, and are stored as raw CSS tokens. This may occur because the value is invalid, or because it included unknown values such as CSS variables. Each property visitor function will need to handle both types of values.

```js
transform({
  code: Buffer.from(`
    .foo { width: 12px }
    .bar { width: var(--w) }
  `),
  visitor: {
    Property: {
      width(v) {
        if (v.property === 'unparsed') {
          // Handle unparsed value, e.g. `var(--w)`
        } else {
          // Handle parsed value, e.g. `12px`
        }
      }
    }
  }
});
```

Unknown properties, including custom properties, have the property type "custom". These values are also stored as raw CSS tokens. To visit custom properties, use the `custom` visitor function, or an object to filter by name. For example, to handle a custom `size` property and expand it to `width` and `height`, the following transform might be used.

```js
let res = transform({
  minify: true,
  code: Buffer.from(`
    .foo {
      size: 12px;
    }
  `),
  visitor: {
    Property: {
      custom: {
        size(property) {
          // Handle the size property when the value is a length.
          if (property.value[0].type === 'length') {
            let value = {
              type: 'length-percentage',
              value: { type: 'dimension', value: property.value[0].value }
            };

            return [
              { property: 'width', value },
              { property: 'height', value }
            ];
          }
        }
      }
    }
  }
});

assert.equal(res.code.toString(), '.foo{width:12px;height:12px}');
```

## Raw values

The Lightning CSS AST is very detailed, which is really useful when you need to transform it. However, it can be tedious to construct a full AST from scratch when returning entirely new values from a visitor. That's when raw values come in handy. You can return a `raw` property containing a string of CSS syntax from visitors that return declarations (i.e. properties) and tokens, and Lightning CSS will parse it for you and put it into the AST.

This example implements a custom `color` function, which returns a raw CSS color value as a string, rather than constructing the whole AST.

```js
let res = transform({
  minify: true,
  code: Buffer.from(`
    .foo {
      color: color('red');
    }
  `),
  visitor: {
    Function: {
      color() {
        return { raw: 'rgb(255, 0, 0)' };
      }
    }
  }
});

assert.equal(res.code.toString(), '.foo{color:red}');
```

## Entry and exit visitors

By default, visitors are called when traversing downward through the tree (a pre-order traversal). This means each node is visited before its children. Sometimes it is useful to process a node after its children instead (a post-order traversal). This can be done by using an `Exit` visitor function, such as `FunctionExit`.

For example, if you had a function visitor to double a length argument, and a visitor to replace an environment variable with a value, you could use an exit visitor to process the function after its arguments.

```js
let res = transform({
  filename: 'test.css',
  minify: true,
  code: Buffer.from(`
    .foo {
      padding: double(env(--branding-padding));
    }
  `),
  visitor: {
    FunctionExit: {
      // This will run after the EnvironmentVariable visitor, below.
      double(f) {
        if (f.arguments[0].type === 'length') {
          return {
            type: 'length',
            value: {
              unit: f.arguments[0].value.unit,
              value: f.arguments[0].value.value * 2
            }
          };
        }
      }
    },
    EnvironmentVariable: {
      // This will run before the FunctionExit visitor, above.
      '--branding-padding': () => ({
        type: 'length',
        value: {
          unit: 'px',
          value: 20
        }
      })
    }
  }
});

assert.equal(res.code.toString(), '.foo{padding:40px}');
```

## Composing visitors

Multiple visitors can be combined into one using the `composeVisitors` function. This lets you reuse visitors between projects by publishing them as plugins. The AST is visited in a single pass, running the functions from each visitor object as if they were written together.

```js
import { transform, composeVisitors } from 'lightningcss';

let environmentVisitor = {
  EnvironmentVariable: {
    '--branding-padding': () => ({
      type: 'length',
      value: {
        unit: 'px',
        value: 20
      }
    })
  }
};

let doubleFunctionVisitor = {
  FunctionExit: {
    double(f) {
      if (f.arguments[0].type === 'length') {
        return {
          type: 'length',
          value: {
            unit: f.arguments[0].value.unit,
            value: f.arguments[0].value.value * 2
          }
        };
      }
    }
  }
};

let res = transform({
  filename: 'test.css',
  minify: true,
  code: Buffer.from(`
    .foo {
      padding: double(env(--branding-padding));
    }
  `),
  visitor: composeVisitors([environmentVisitor, doubleFunctionVisitor])
});

assert.equal(res.code.toString(), '.foo{padding:40px}');
```

Each visitor object has the opportunity to visit every value once. If a visitor returns a new value, that value is visited by the other visitor objects but not again by the original visitor that created it. If other visitors subsequently modify the value, the previous visitors will not revisit the value. This is to avoid infinite loops.

## Unknown at-rules

By default, unknown at-rules are stored in the AST as raw tokens. This allows you to interpret them however you like by writing a custom visitor. The following example allows declaring static variables using named at-rules, and inlines them when an `at-keyword` token is seen:

```js
let declared = new Map();
let res = transform({
  filename: 'test.css',
  minify: true,
  code: Buffer.from(`
    @blue #056ef0;

    .menu_link {
      background: @blue;
    }
  `),
  visitor: {
    Rule: {
      unknown(rule) {
        declared.set(rule.name, rule.prelude);
        return [];
      }
    },
    Token: {
      'at-keyword'(token) {
        return declared.get(token.value);
      }
    }
  }
});

assert.equal(res.code.toString(), '.menu_link{background:#056ef0}');
```

## Custom at-rules

Raw tokens as stored in unknown at-rules are fine for simple cases, but in more complex cases, you may wish to interpret a custom at-rule body as a standard CSS declaration list or rule list. However, by default, Lightning CSS does not know how unknown rules should be parsed. You can define their syntax using the `customAtRules` option.

The syntax of the at-rule prelude can be defined with a [CSS syntax string](https://drafts.css-houdini.org/css-properties-values-api/#syntax-strings), which Lightning CSS will interpret and use to validate the input CSS. This uses the same syntax as the [@property](https://developer.mozilla.org/en-US/docs/Web/CSS/@property) rule. The body syntax is defined using one of the following options:

* `"declaration-list"` – A list of CSS declarations (property value pairs), as in a style rule or other at-rules like `@font-face`.
* `"rule-list"` – A list of nested CSS rules, including style rules and at rules. Directly nested declarations with CSS nesting are not allowed. This matches how rules like `@keyframes` are parsed.
* `"style-block"` – A list of CSS declarations and/or nested rules. This matches the behavior of rules like `@media` and `@supports` which support directly nested declarations when inside a style rule. Note that the [nesting](transpilation.html#nesting) and [targets](transpilation.html#browser-targets) options must be defined for nesting to be compiled.

This example defines two custom at-rules. `@mixin` defines a reusable style block, supporting both directly nested declarations and nested rules. A visitor function registers the mixin in a map and removes the custom rule. `@apply` looks up the requested mixin in the map and returns the nested rules, which are inlined into the parent.

```js
let mixins = new Map();
let res = transform({
  filename: 'test.css',
  minify: true,
  targets: { chrome: 100 << 16 },
  code: Buffer.from(`
    @mixin color {
      color: red;

      &.bar {
        color: yellow;
      }
    }

    .foo {
      @apply color;
    }
  `),
  customAtRules: {
    mixin: {
      prelude: '<custom-ident>',
      body: 'style-block'
    },
    apply: {
      prelude: '<custom-ident>'
    }
  },
  visitor: {
    Rule: {
      custom: {
        mixin(rule) {
          mixins.set(rule.prelude.value, rule.body.value);
          return [];
        },
        apply(rule) {
          return mixins.get(rule.prelude.value);
        }
      }
    }
  }
});

assert.equal(res.code.toString(), '.foo{color:red}.foo.bar{color:#ff0}');
```

## Examples

For examples of visitors that perform a variety of real world tasks, see the Lightning CSS [visitor tests](https://github.com/parcel-bundler/lightningcss/blob/master/node/test/visitor.test.mjs).

## Publishing a plugin

Visitor plugins can be published to npm in order to share them with others. Plugin packages simply consist of an exported visitor object, which users can compose with other plugins via the `composeVisitors` function as described above.

```js
// lightningcss-plugin-double-function
export default {
  FunctionExit: {
    double(f) {
      // ...
    }
  }
};
```

Plugins can also export a function in order to accept options.

```js
// lightningcss-plugin-env
export default (values) => ({
  EnvironmentVariable(env) {
    return values[env.name];
  }
});
```

Plugin package names should start with `lightningcss-plugin-` and be descriptive about what they do, e.g. `lightningcss-plugin-double-function`. In addition, they should include the `lightningcss-plugin` keyword in their package.json so people can find them on npm.

```json
{
  "name": "lightningcss-plugin-double-function",
  "keywords": ["lightningcss-plugin"],
  "main": "plugin.mjs"
}
```

## Using plugins

To use a published visitor plugin, install the package from npm, import it, and use the `composeVisitors` function as described above.

```js
import { transform, composeVisitors } from 'lightningcss';
import environmentVisitor from 'lightningcss-plugin-environment';
import doubleFunctionVisitor from 'lightningcss-plugin-double-function';

let res = transform({
  filename: 'test.css',
  minify: true,
  code: Buffer.from(`
    .foo {
      padding: double(env(--branding-padding));
    }
  `),
  visitor: composeVisitors([
    environmentVisitor({
      '--branding-padding': {
        type: 'length',
        value: {
          unit: 'px',
          value: 20
        }
      }
    }),
    doubleFunctionVisitor
  ])
});

assert.equal(res.code.toString(), '.foo{padding:40px}');
```
