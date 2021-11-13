# parcel-css

A WIP CSS parser, transformer, and minifier written in Rust.

## Features

- **Extremely fast** – Parsing an minifying large files is completed in milliseconds, often with significantly smaller output. See benchmarks below.
- **Typed property values** – many other CSS parsers treat property values as an untyped series of tokens. This means that each transformer that wants to do something with these values must interpret them itself, leading to duplicate work and inconsistencies. `parcel-css` parses all values using the grammar from the CSS specification, and exposes a specific value type for each property.
- **Minification** – One of the main purposes of `parcel-css` is to minify CSS to make it smaller. This includes many optimizations including:
  - Combining longhand properties into shorthands where possible.
  - Merging adjacent rules with the same selectors or declarations.
  - Combining CSS transforms into a single matrix or visa versa when smaller.
  - Removing vendor prefixes that are not needed, based on the provided browser targets.
  - Reducing `calc()` expressions where possible.
  - Converting colors to shorter hex notation where possible.
  - Minifying gradients.
  - Normalizing property value order.
  - Removing default property sub-values which will be inferred by browsers.
  - Many micro-optimizations, e.g. converting to shorter units, removing unnecessary quotation marks, etc.
- **Vendor prefixing** – `parcel-css` accepts a list of browser targets, and automatically adds (and removes) vendor prefixes.
- **CSS modules** – TODO
