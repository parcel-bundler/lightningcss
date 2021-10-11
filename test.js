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
  border-top-width: thin;
  border-bottom-width: 1in;
  border-top-color: red;
  border-bottom-style: dotted;
  border-width: thin thick thin thick;
  border: red dotted 2px;
  border-block-start: solid #00ff00 medium;
}

.hi {}

@media screen and (max-width: 250px) {
  .baz {
    // background: url(img.png) ;
    color: var(--test);
  }
}
`)})
