const css = require('./native');

css.transform({
  filename: __filename,
  code: Buffer.from(`

.foo + .bar:not(.baz) {
  background-color: blue;
  // background-image: url(img.png), url('test.jpg');
  background: url(img.png), url(test.jpg) gray;
  background-position-x: right 20px, 10px;
  background-position-y: top 20px, 15px;
  background-size: 50px 50px, auto;
  background-repeat: repeat no-repeat, no-repeat;
  // outline: 2px auto red;
  outline-color: blue;
  outline-width: 2px;
  outline-style: solid;
  // background: url("chess.png") 40% / 10em gray, url(img.png);
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
  // margin-block-start: 15px;
  // margin-block-end: 15px;
  margin-top: 20px;
  margin-bottom: 20px;

  flex-direction: row;
  flex-wrap: wrap;

  flex-grow: 1;
  flex-shrink: 1;
  flex-basis: 0%;

  align-content: center;
  justify-content: center;

  align-self: center;
  justify-self: center;

  align-items: center;
  justify-items: center;

  row-gap: 10px;
  column-gap: 20px;

  font: 20px helvetica;
  font-weight: bold;
  font-size: 22px;
  font-variant-caps: small-caps;
}

// .hi {}

// @media screen and (max-width: 250px) {
//   .baz {
//     // background: url(img.png) ;
//     color: var(--test);
//   }
// }

@keyframes test {
  from {
    background: red;
  }

  50% {
    background: blue;
  }

  to {
    background: green;
  }
}
`)})
