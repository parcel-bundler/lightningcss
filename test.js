const css = require('./native');

css.transform({
  filename: __filename,
  code: Buffer.from(`

.foo + .bar:not(.baz) {
  background-color: blue;
  background-image: url(img.png), url('test.jpg');
  background-position-x: right 20px;
  background-position-y: top 20px;
  color: blue;
  width: 250px;
  height: 50%;
  min-block-size: min-content;
  max-height: fit-content(50%);
  top: auto;
  bottom: 5%;
  border-top-width: thin;
  border-bottom-width: thin;
  border-right-width: thin;
  border-left-width: thick;
  // border-bottom-width: 1in;
  // border-top-color: red;
  // border-bottom-style: dotted;
  // border-width: thin thick thin thick;
  // border-bottom: red dotted 2px;
  // border-right: red dotted 2px;
  // border-left: red dotted 2px;
  // border-top: red dotted 2px;

  // border-left: solid #00ff00 medium;
  // border-top: solid #00ff00 medium;

  border-image: url(test.png) 60;
  border-image-source: url(foo.png);
  // border-radius: 10px 100px 10px 100px / 120px 120px;
  border-top-left-radius: 10px 120px;
  border-top-right-radius: 100px 120px;
  border-bottom-left-radius: 10px 120px;
  border-bottom-right-radius: 100px 120px;
  border-start-start-radius: 10px;

  margin-left: 10px;
  margin-right: 10px;
  margin-block-start: 15px;
  margin-block-end: 15px;
  margin-top: 20px;
  margin-bottom: 20px;
}

// .hi {}

// @media screen and (max-width: 250px) {
//   .baz {
//     // background: url(img.png) ;
//     color: var(--test);
//   }
// }
`)})
