const css = require('./native');

css.transform({
  filename: __filename,
  code: Buffer.from(`
@import "test.css";
@import "foo.css" print;

.foo + .bar:not(.baz) {
  background: url(img.png), url('test.jpg');
  color: blue;
}

.hi {}

@media screen and (max-width: 250px) {
  .baz {
    background: url(img.png);
  }
}
`)})
