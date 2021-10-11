const css = require('./native');

css.transform({
  filename: __filename,
  code: Buffer.from(`
@import "test.css";
@import "foo.css" print;

.foo + .bar:not(.baz) {
  background-color: blue;
  background-image: url(img.png), url('test.jpg');
  color: blue;
  width: 250px;
  height: 50%;
  min-block-size: min-content;
  max-height: fit-content(50%);
  top: auto;
  bottom: 5%;
}

.hi {}

@media screen and (max-width: 250px) {
  .baz {
    // background: url(img.png) ;
    color: var(--test);
  }
}
`)})
