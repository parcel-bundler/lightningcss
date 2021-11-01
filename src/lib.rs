extern crate napi;
#[macro_use]
extern crate napi_derive;
extern crate serde;
extern crate serde_bytes;
extern crate cssparser;
extern crate selectors;
extern crate itertools;
extern crate smallvec;

mod parser;
mod rules;
mod declaration;
mod media_query;
mod selector;
mod properties;
mod values;
mod printer;
mod traits;
mod macros;

use napi::{CallContext, JsObject, JsBuffer};
use serde::{Deserialize, Serialize};
use cssparser::{Parser, ParserInput, RuleListParser};
use crate::traits::ToCss;
use printer::Printer;
use std::fmt::Write;

use parser::TopLevelRuleParser;

#[derive(Serialize, Debug, Deserialize)]
struct Config {
  filename: String,
  #[serde(with = "serde_bytes")]
  code: Vec<u8>,
}

#[js_function(1)]
fn transform(ctx: CallContext) -> napi::Result<JsBuffer> {
  let opts = ctx.get::<JsObject>(0)?;
  let config: Config = ctx.env.from_js_value(opts)?;

  let code = unsafe { std::str::from_utf8_unchecked(&config.code) };

  // for (pos, declarations) in rule_list.flatten() {
  //   println!("{:?}", declarations);
  // }

  let res = compile(code, true);

  Ok(ctx.env.create_buffer_with_data(res.into_bytes())?.into_raw())
}

fn compile(code: &str, minify: bool) -> String {
  let mut input = ParserInput::new(&code);
  let mut parser = Parser::new(&mut input);
  let rule_list = RuleListParser::new_for_stylesheet(&mut parser, TopLevelRuleParser {});

  // for (pos, declarations) in rule_list.flatten() {
  //   println!("{:?}", declarations);
  // }

  let mut dest = String::new();
  let mut printer = Printer::new(&mut dest, minify);
  let mut first = true;
  // let mut last_style_rule = None;
  let mut rules = vec![];

  for (pos, rule) in rule_list.flatten() {
    // println!("{:?}", rule);
    let rule = match rule {
      parser::CssRule::Import(import) => {
        parser::CssRule::Import(parser::ImportRule {
          media: import.media,
          url: "test".into()
        })
      },
      parser::CssRule::Keyframes(mut keyframes) => {
        for keyframe in keyframes.keyframes.iter_mut() {
          keyframe.declarations.minify();
        }
        parser::CssRule::Keyframes(keyframes)
      }
      parser::CssRule::Style(mut style) => {
        for selector in style.selectors.0.iter() {
          for x in selector.iter() {
            match x {
              selectors::parser::Component::Class(c) => {
                // if c == "hi" {
                //   selectors::parser::Component::Class("RENAMED".into())
                // } else {
                //   selectors::parser::Component::Class(c.clone())
                // }
                if c == "hi" {
                  c.replace("RENAMED".into());
                }
              },
              _ => {}
            }
          }
        }

        style.declarations.minify();

        if let Some(parser::CssRule::Style(last_style_rule)) = rules.last_mut() {
          if style.selectors == last_style_rule.selectors {
            last_style_rule.declarations.declarations.extend(style.declarations.declarations);
            last_style_rule.declarations.minify();
            continue
          } else if style.declarations == last_style_rule.declarations {
            last_style_rule.selectors.0.extend(style.selectors.0);
            continue
          }
        }

        parser::CssRule::Style(style)
      },
      r => r
    };
    rules.push(rule);
  }

  for rule in rules {
    if first {
      first = false;
    } else {
      printer.newline();
    }

    rule.to_css(&mut printer);
    printer.newline();
  }

  dest
}

#[module_exports]
fn init(mut exports: JsObject) -> napi::Result<()> {
  exports.create_named_method("transform", transform)?;

  Ok(())
}

#[cfg(test)]
mod tests {
  use super::*;
  extern crate indoc;
  use self::indoc::indoc;

  fn test(source: &str, expected: &str) {
    let res = compile(source, false);
    assert_eq!(res, expected);
  }

  fn minify_test(source: &str, expected: &str) {
    let res = compile(source, true);
    assert_eq!(res, expected);
  }

  #[test]
  pub fn test_border() {
    test(r#"
      .foo {
        border-left: 2px solid red;
        border-right: 2px solid red;
        border-bottom: 2px solid red;
        border-top: 2px solid red;
      }
    "#, indoc! {r#"
      .foo {
        border: 2px solid red;
      }
    "#
    });

    test(r#"
      .foo {
        border-left-color: red;
        border-right-color: red;
        border-bottom-color: red;
        border-top-color: red;
      }
    "#, indoc! {r#"
      .foo {
        border-color: red;
      }
    "#
    });

    test(r#"
      .foo {
        border-left-width: thin;
        border-right-width: thin;
        border-bottom-width: thin;
        border-top-width: thin;
      }
    "#, indoc! {r#"
      .foo {
        border-width: thin;
      }
    "#
    });

    test(r#"
      .foo {
        border-left-style: dotted;
        border-right-style: dotted;
        border-bottom-style: dotted;
        border-top-style: dotted;
      }
    "#, indoc! {r#"
      .foo {
        border-style: dotted;
      }
    "#
    });

    test(r#"
      .foo {
        border-left-width: thin;
        border-left-style: dotted;
        border-left-color: red;
      }
    "#, indoc! {r#"
      .foo {
        border-left: thin dotted red;
      }
    "#
    });

    test(r#"
      .foo {
        border-left-width: thick;
        border-left: thin dotted red;
      }
    "#, indoc! {r#"
      .foo {
        border-left: thin dotted red;
      }
    "#
    });

    test(r#"
      .foo {
        border-left-width: thick;
        border: thin dotted red;
      }
    "#, indoc! {r#"
      .foo {
        border: thin dotted red;
      }
    "#
    });

    test(r#"
      .foo {
        border: thin dotted red;
        border-right-width: thick;
      }
    "#, indoc! {r#"
      .foo {
        border: thin dotted red;
        border-right-width: thick;
      }
    "#
    });

    test(r#"
      .foo {
        border: thin dotted red;
        border-right: thick dotted red;
      }
    "#, indoc! {r#"
      .foo {
        border: thin dotted red;
        border-right-width: thick;
      }
    "#
    });

    test(r#"
      .foo {
        border: thin dotted red;
        border-right-width: thick;
        border-right-style: solid;
      }
    "#, indoc! {r#"
      .foo {
        border: thin dotted red;
        border-right: thick solid red;
      }
    "#
    });

    test(r#"
      .foo {
        border-top: thin dotted red;
        border-block-start: thick solid green;
      }
    "#, indoc! {r#"
      .foo {
        border-top: thin dotted red;
        border-block-start: thick solid green;
      }
    "#
    });

    test(r#"
      .foo {
        border: thin dotted red;
        border-block-start-width: thick;
        border-left-width: medium;
      }
    "#, indoc! {r#"
      .foo {
        border: thin dotted red;
        border-block-start-width: thick;
        border-left-width: medium;
      }
    "#
    });

    test(r#"
      .foo {
        border-block-start: thin dotted red;
        border-inline-end: thin dotted red;
      }
    "#, indoc! {r#"
      .foo {
        border-block-start: thin dotted red;
        border-inline-end: thin dotted red;
      }
    "#
    });

    test(r#"
      .foo {
        border-block-start-width: thin;
        border-block-start-style: dotted;
        border-block-start-color: red;
        border-inline-end: thin dotted red;
      }
    "#, indoc! {r#"
      .foo {
        border-block-start: thin dotted red;
        border-inline-end: thin dotted red;
      }
    "#
    });

    test(r#"
      .foo {
        border-block-start: thin dotted red;
        border-block-end: thin dotted red;
      }
    "#, indoc! {r#"
      .foo {
        border-block: thin dotted red;
      }
    "#
    });

    minify_test(r#"
      .foo {
        border: none;
      }
    "#, indoc! {".foo{border:0}"
    });
  }

  #[test]
  pub fn test_border_image() {
    test(r#"
      .foo {
        border-image: url(test.png) 60;
      }
    "#, indoc! {r#"
      .foo {
        border-image: url(test.png) 60;
      }
    "#
    });

    test(r#"
      .foo {
        border-image: url(test.png) 60;
        border-image-source: url(foo.png);
      }
    "#, indoc! {r#"
      .foo {
        border-image: url(foo.png) 60;
      }
    "#
    });

    test(r#"
      .foo {
        border-image-source: url(foo.png);
        border-image-slice: 10 40 10 40 fill;
        border-image-width: 10px;
        border-image-outset: 0;
        border-image-repeat: round round;
      }
    "#, indoc! {r#"
      .foo {
        border-image: url(foo.png) 10 40 fill / 10px round;
      }
    "#
    });
  }

  #[test]
  pub fn test_border_radius() {
    test(r#"
      .foo {
        border-radius: 10px 100px 10px 100px;
      }
    "#, indoc! {r#"
      .foo {
        border-radius: 10px 100px;
      }
    "#
    });

    test(r#"
      .foo {
        border-radius: 10px 100px 10px 100px / 120px 120px;
      }
    "#, indoc! {r#"
      .foo {
        border-radius: 10px 100px / 120px;
      }
    "#
    });

    test(r#"
      .foo {
        border-top-left-radius: 10px 120px;
        border-top-right-radius: 100px 120px;
        border-bottom-left-radius: 10px 120px;
        border-bottom-right-radius: 100px 120px;
      }
    "#, indoc! {r#"
      .foo {
        border-radius: 10px 100px / 120px;
      }
    "#
    });

    test(r#"
      .foo {
        border-radius: 10px 100px 10px 100px / 120px 120px;
        border-start-start-radius: 10px;
      }
    "#, indoc! {r#"
      .foo {
        border-radius: 10px 100px / 120px;
        border-start-start-radius: 10px;
      }
    "#
    });

    test(r#"
      .foo {
        border-start-start-radius: 10px;
        border-radius: 10px 100px 10px 100px / 120px 120px;
      }
    "#, indoc! {r#"
      .foo {
        border-radius: 10px 100px / 120px;
      }
    "#
    });

    test(r#"
      .foo {
        border-top-left-radius: 10px 120px;
        border-top-right-radius: 100px 120px;
        border-start-start-radius: 10px;
        border-bottom-left-radius: 10px 120px;
        border-bottom-right-radius: 100px 120px;
      }
    "#, indoc! {r#"
      .foo {
        border-top-left-radius: 10px 120px;
        border-top-right-radius: 100px 120px;
        border-start-start-radius: 10px;
        border-bottom-left-radius: 10px 120px;
        border-bottom-right-radius: 100px 120px;
      }
    "#
    });
  }

  #[test]
  pub fn test_outline() {
    test(r#"
      .foo {
        outline-width: 2px;
        outline-style: solid;
        outline-color: blue;
      }
    "#, indoc! {r#"
      .foo {
        outline: 2px solid #00f;
      }
    "#
    });

    test(r#"
      .foo {
        outline: 2px solid blue;
      }
    "#, indoc! {r#"
      .foo {
        outline: 2px solid #00f;
      }
    "#
    });

    test(r#"
      .foo {
        outline: 2px solid red;
        outline-color: blue;
      }
    "#, indoc! {r#"
      .foo {
        outline: 2px solid #00f;
      }
    "#
    });
  }

  #[test]
  pub fn test_margin() {
    test(r#"
      .foo {
        margin-left: 10px;
        margin-right: 10px;
        margin-top: 20px;
        margin-bottom: 20px;
      }
    "#, indoc! {r#"
      .foo {
        margin: 20px 10px;
      }
    "#
    });

    test(r#"
      .foo {
        margin-block-start: 15px;
        margin-block-end: 15px;
      }
    "#, indoc! {r#"
      .foo {
        margin-block: 15px;
      }
    "#
    });

    test(r#"
      .foo {
        margin-left: 10px;
        margin-right: 10px;
        margin-inline-start: 15px;
        margin-inline-end: 15px;
        margin-top: 20px;
        margin-bottom: 20px;
      
      }
    "#, indoc! {r#"
      .foo {
        margin-left: 10px;
        margin-right: 10px;
        margin-inline: 15px;
        margin-top: 20px;
        margin-bottom: 20px;
      }
    "#
    });
  }

  #[test]
  pub fn test_padding() {
    test(r#"
      .foo {
        padding-left: 10px;
        padding-right: 10px;
        padding-top: 20px;
        padding-bottom: 20px;
      }
    "#, indoc! {r#"
      .foo {
        padding: 20px 10px;
      }
    "#
    });

    test(r#"
      .foo {
        padding-block-start: 15px;
        padding-block-end: 15px;
      }
    "#, indoc! {r#"
      .foo {
        padding-block: 15px;
      }
    "#
    });

    test(r#"
      .foo {
        padding-left: 10px;
        padding-right: 10px;
        padding-inline-start: 15px;
        padding-inline-end: 15px;
        padding-top: 20px;
        padding-bottom: 20px;
      
      }
    "#, indoc! {r#"
      .foo {
        padding-left: 10px;
        padding-right: 10px;
        padding-inline: 15px;
        padding-top: 20px;
        padding-bottom: 20px;
      }
    "#
    });
  }

  #[test]
  pub fn test_background() {
    test(r#"
      .foo {
        background: url(img.png);
        background-position-x: 20px;
        background-position-y: 10px;
        background-size: 50px 100px;
        background-repeat: repeat no-repeat;
      }
    "#, indoc! {r#"
      .foo {
        background: url(img.png) 20px 10px / 50px 100px repeat-x;
      }
    "#
    });

    test(r#"
      .foo {
        background-color: red;
        background-position: 0% 0%;
        background-size: auto;
        background-repeat: repeat;
        background-clip: border-box;
        background-origin: padding-box;
        background-attachment: scroll;
        background-image: none
      }
    "#, indoc! {r#"
      .foo {
        background: red;
      }
    "#
    });

    test(r#"
      .foo {
        background-color: gray;
        background-position: 40% 50%;
        background-size: 10em auto;
        background-repeat: round;
        background-clip: border-box;
        background-origin: border-box;
        background-attachment: fixed;
        background-image: url('chess.png');
      }
    "#, indoc! {r#"
      .foo {
        background: gray url(chess.png) 40% / 10em round fixed border-box;
      }
    "#
    });

    test(r#"
      .foo {
        background: url(img.png), url(test.jpg) gray;
        background-position-x: right 20px, 10px;
        background-position-y: top 20px, 15px;
        background-size: 50px 50px, auto;
        background-repeat: repeat no-repeat, no-repeat;      
      }
    "#, indoc! {r#"
      .foo {
        background: url(img.png) right 20px top 20px / 50px 50px repeat-x, gray url(test.jpg) 10px 15px no-repeat;
      }
    "#
    });

    minify_test(r#"
      .foo {
        background-position: center center;
      }
    "#, indoc! {".foo{background-position:50% 50%}"
    });
  }

  #[test]
  pub fn test_flex() {
    test(r#"
      .foo {
        flex-direction: column;
        flex-wrap: wrap;
      }
    "#, indoc! {r#"
      .foo {
        flex-flow: column wrap;
      }
    "#
    });

    test(r#"
      .foo {
        flex-direction: row;
        flex-wrap: wrap;
      }
    "#, indoc! {r#"
      .foo {
        flex-flow: wrap;
      }
    "#
    });

    test(r#"
      .foo {
        flex-direction: row;
        flex-wrap: nowrap;
      }
    "#, indoc! {r#"
      .foo {
        flex-flow: row;
      }
    "#
    });

    test(r#"
      .foo {
        flex-direction: column;
        flex-wrap: nowrap;
      }
    "#, indoc! {r#"
      .foo {
        flex-flow: column;
      }
    "#
    });

    test(r#"
      .foo {
        flex-grow: 1;
        flex-shrink: 1;
        flex-basis: 0%;
      }
    "#, indoc! {r#"
      .foo {
        flex: 1;
      }
    "#
    });

    test(r#"
      .foo {
        flex-grow: 1;
        flex-shrink: 0;
        flex-basis: 0%;
      }
    "#, indoc! {r#"
      .foo {
        flex: 1 0;
      }
    "#
    });

    test(r#"
      .foo {
        flex-grow: 1;
        flex-shrink: 0;
        flex-basis: auto;
      }
    "#, indoc! {r#"
      .foo {
        flex: 1 0 auto;
      }
    "#
    });

    test(r#"
      .foo {
        flex-grow: 1;
        flex-shrink: 1;
        flex-basis: auto;
      }
    "#, indoc! {r#"
      .foo {
        flex: auto;
      }
    "#
    });

    test(r#"
      .foo {
        flex: 0 0;
        flex-grow: 1;
      }
    "#, indoc! {r#"
      .foo {
        flex: 1 0;
      }
    "#
    });

    test(r#"
      .foo {
        align-content: center;
        justify-content: center;
      }
    "#, indoc! {r#"
      .foo {
        place-content: center;
      }
    "#
    });

    test(r#"
      .foo {
        align-content: first baseline;
        justify-content: safe right;
      }
    "#, indoc! {r#"
      .foo {
        place-content: baseline safe right;
      }
    "#
    });

    test(r#"
      .foo {
        place-content: first baseline unsafe left;
      }
    "#, indoc! {r#"
      .foo {
        place-content: baseline unsafe left;
      }
    "#
    });

    test(r#"
      .foo {
        place-content: center center;
      }
    "#, indoc! {r#"
      .foo {
        place-content: center;
      }
    "#
    });

    test(r#"
      .foo {
        align-self: center;
        justify-self: center;
      }
    "#, indoc! {r#"
      .foo {
        place-self: center;
      }
    "#
    });

    test(r#"
      .foo {
        align-self: center;
        justify-self: unsafe left;
      }
    "#, indoc! {r#"
      .foo {
        place-self: center unsafe left;
      }
    "#
    });

    test(r#"
      .foo {
        align-items: center;
        justify-items: center;
      }
    "#, indoc! {r#"
      .foo {
        place-items: center;
      }
    "#
    });

    test(r#"
      .foo {
        align-items: center;
        justify-items: legacy left;
      }
    "#, indoc! {r#"
      .foo {
        place-items: center legacy left;
      }
    "#
    });

    test(r#"
      .foo {
        row-gap: 10px;
        column-gap: 20px;
      }
    "#, indoc! {r#"
      .foo {
        gap: 10px 20px;
      }
    "#
    });

    test(r#"
      .foo {
        row-gap: 10px;
        column-gap: 10px;
      }
    "#, indoc! {r#"
      .foo {
        gap: 10px;
      }
    "#
    });

    test(r#"
      .foo {
        gap: 10px;
        column-gap: 20px;
      }
    "#, indoc! {r#"
      .foo {
        gap: 10px 20px;
      }
    "#
    });

    test(r#"
      .foo {
        column-gap: 20px;
        gap: 10px;
      }
    "#, indoc! {r#"
      .foo {
        gap: 10px;
      }
    "#
    });

    test(r#"
      .foo {
        row-gap: normal;
        column-gap: 20px;
      }
    "#, indoc! {r#"
      .foo {
        gap: normal 20px;
      }
    "#
    });
  }

  #[test]
  fn test_font() {
    test(r#"
      .foo {
        font-family: "Helvetica", "Times New Roman", sans-serif;
        font-size: 12px;
        font-weight: bold;
        font-style: italic;
        font-stretch: expanded;
        font-variant-caps: small-caps;
        line-height: 1.2em;
      }
    "#, indoc! {r#"
      .foo {
        font: italic small-caps bold expanded 12px / 1.2em Helvetica, Times New Roman, sans-serif;
      }
    "#
    });

    minify_test(r#"
      .foo {
        font-family: "Helvetica", "Times New Roman", sans-serif;
        font-size: 12px;
        font-weight: bold;
        font-style: italic;
        font-stretch: expanded;
        font-variant-caps: small-caps;
        line-height: 1.2em;
      }
    "#, indoc! {".foo{font:italic small-caps 700 50% 12px/1.2em Helvetica,Times New Roman,sans-serif}"
    });

    test(r#"
      .foo {
        font: 12px "Helvetica", "Times New Roman", sans-serif;
        line-height: 1.2em;
      }
    "#, indoc! {r#"
      .foo {
        font: 12px / 1.2em Helvetica, Times New Roman, sans-serif;
      }
    "#
    });

    minify_test(r#"
      .foo {
        font-family: "Helvetica", "Times New Roman", sans-serif;
        font-size: 12px;
        font-stretch: expanded;
      }
    "#, indoc! {".foo{font-family:Helvetica,Times New Roman,sans-serif;font-size:12px;font-stretch:50%}"
    });

    test(r#"
      .foo {
        font-family: "Helvetica", "Times New Roman", sans-serif;
        font-size: 12px;
        font-weight: bold;
        font-style: italic;
        font-stretch: expanded;
        font-variant-caps: all-small-caps;
        line-height: 1.2em;
      }
    "#, indoc! {r#"
      .foo {
        font: italic bold expanded 12px / 1.2em Helvetica, Times New Roman, sans-serif;
        font-variant-caps: all-small-caps;
      }
    "#
    });
  }

  #[test]
  fn test_selectors() {
    minify_test("[foo=\"baz\"] {}", "[foo=baz]{}");
    minify_test("[foo=\"foo bar\"] {}", "[foo=foo\\ bar]{}");
    minify_test("[foo=\"foo bar baz\"] {}", "[foo=\"foo bar baz\"]{}");
    minify_test(".test:not([foo=\"bar\"]) {}", ".test:not([foo=bar]){}");
    minify_test(".test + .foo {}", ".test+.foo{}");
    minify_test(".test ~ .foo {}", ".test~.foo{}");
    minify_test(".test .foo {}", ".test .foo{}");
  }

  #[test]
  fn test_keyframes() {
    minify_test(r#"
      @keyframes test {
        from {
          background: green;
        }

        50% {
          background: red;
        }

        100% {
          background: blue
        }
      }
    "#, "@keyframes test{0%{background:green}50%{background:red}to{background:#00f}}");
    minify_test(r#"
      @keyframes test {
        from {
          background: green;
          background-color: red;
        }

        100% {
          background: blue
        }
      }
    "#, "@keyframes test{0%{background:red}to{background:#00f}}");
  }

  #[test]
  fn test_important() {
    test(r#"
      .foo {
        align-items: center;
        justify-items: center !important;
      }
    "#, indoc! {r#"
      .foo {
        align-items: center;
        justify-items: center !important;
      }
    "#});

    test(r#"
      .foo {
        justify-items: center !important;
        align-items: center;
      }
    "#, indoc! {r#"
      .foo {
        align-items: center;
        justify-items: center !important;
      }
    "#});

    minify_test(r#"
      .foo {
        font-family: SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace !important;
      }
    "#, ".foo{font-family:SFMono-Regular,Menlo,Monaco,Consolas,Liberation Mono,Courier New,monospace!important}");
  }

  #[test]
  fn test_calc() {
    minify_test(".foo { width: calc(20px * 2) }", ".foo{width:40px}");
    minify_test(".foo { font-size: calc(100vw / 35) }", ".foo{font-size:2.85714vw}");
    minify_test(".foo { width: calc(20px * 2 * 3) }", ".foo{width:120px}");
    minify_test(".foo { width: calc(20px + 30px) }", ".foo{width:50px}");
    minify_test(".foo { width: calc(20px + 30px + 40px) }", ".foo{width:90px}");
    minify_test(".foo { width: calc(100% - 30px) }", ".foo{width:calc(100% - 30px)}");
    minify_test(".foo { width: calc(100% - 30px + 20px) }", ".foo{width:calc(100% - 10px)}");
    minify_test(".foo { width: calc(20px + 100% - 30px) }", ".foo{width:calc(100% - 10px)}");
    minify_test(".foo { width: calc(20px + 100% + 10vw - 30px) }", ".foo{width:calc(100% - 10px + 10vw)}");
    minify_test(".foo { width: calc(20px + 100% - 30px) }", ".foo{width:calc(100% - 10px)}");
    minify_test(".foo { width: calc(2 * (100% - 20px)) }", ".foo{width:calc(200% - 40px)}");
    minify_test(".foo { width: calc((100% - 20px) * 2) }", ".foo{width:calc(200% - 40px)}");
    minify_test(".foo { width: calc(100% - 20px * 2) }", ".foo{width:calc(100% - 40px)}");
    minify_test(".foo { width: calc(1px + 1px) }", ".foo{width:2px}");
    minify_test(".foo { width: calc(100vw / 2) }", ".foo{width:50vw}");
    minify_test(".foo { width: calc(50px - (20px - 30px)) }", ".foo{width:60px}");
    minify_test(".foo { width: calc(100px - (100px - 100%)) }", ".foo{width:100%}");
    minify_test(".foo { width: calc(100px + (100px - 100%)) }", ".foo{width:calc(200px - 100%)}");
    minify_test(".foo { width: calc(1px - (2em + 3%)) }", ".foo{width:calc(1px + -2em - 3%)}"); // TODO: fix sign
    minify_test(".foo { width: calc((100vw - 50em) / 2) }", ".foo{width:calc(50vw - 25em)}");
    minify_test(".foo { width: calc(1px - (2em + 4vh + 3%)) }", ".foo{width:calc(1px + -2em - 4vh - 3%)}"); // TODO
    minify_test(".foo { width: calc(1px + (2em + (3vh + 4px))) }", ".foo{width:calc(2em + 3vh + 5px)}");
    minify_test(".foo { width: calc(1px - (2em + 4px - 6vh) / 2) }", ".foo{width:calc(-1em - 1px + 3vh)}");
    minify_test(".foo { width: calc(100% - calc(50% + 25px)) }", ".foo{width:calc(50% - 25px)}");
    minify_test(".foo { width: calc(1px/100) }", ".foo{width:.01px}");
    minify_test(".foo { width: calc(100vw / 2 - 6px + 0px) }", ".foo{width:calc(50vw - 6px)}");
    minify_test(".foo { width: calc(1px + 1) }", ".foo{width:calc(1px + 1)}");
    minify_test(".foo { width: calc( (1em - calc( 10px + 1em)) / 2) }", ".foo{width:-5px}");
    minify_test(".foo { width: calc((100px - 1em) + (-50px + 1em)) }", ".foo{width:50px}");
    minify_test(".foo { width: calc(100% + (2 * 100px) - ((75.37% - 63.5px) - 900px)) }", ".foo{width:calc(24.63% + 1163.5px)}");
    minify_test(".foo { width: calc(((((100% + (2 * 30px) + 63.5px) / 0.7537) - (100vw - 60px)) / 2) + 30px) }", ".foo{width:calc(66.3394% + 141.929px - 50vw)}");
    minify_test(".foo { width: calc(((75.37% - 63.5px) - 900px) + (2 * 100px)) }", ".foo{width:calc(75.37% - 763.5px)}");
    minify_test(".foo { width: calc((900px - (10% - 63.5px)) + (2 * 100px)) }", ".foo{width:calc(1163.5px - 10%)}");
    minify_test(".foo { width: calc(500px/0) }", ".foo{width:calc(500px/0)}");
    minify_test(".foo { width: calc(500px/2px) }", ".foo{width:calc(500px/2px)}");
    minify_test(".foo { width: calc(100% / 3 * 3) }", ".foo{width:100%}");
    minify_test(".foo { width: calc(+100px + +100px) }", ".foo{width:200px}");
    minify_test(".foo { width: calc(+100px - +100px) }", ".foo{width:0}");
    minify_test(".foo { width: calc(200px * +1) }", ".foo{width:200px}");
    minify_test(".foo { width: calc(200px / +1) }", ".foo{width:200px}");
    minify_test(".foo { width: calc(1.1e+1px + 1.1e+1px) }", ".foo{width:22px}");
  }

  #[test]
  fn test_box_shadow() {
    minify_test(".foo { box-shadow: 64px 64px 12px 40px rgba(0,0,0,0.4) }", ".foo{box-shadow:64px 64px 12px 40px #0006}");
    minify_test(".foo { box-shadow: 12px 12px 0px 8px rgba(0,0,0,0.4) inset }", ".foo{box-shadow:inset 12px 12px 0 8px #0006}");
    minify_test(".foo { box-shadow: inset 12px 12px 0px 8px rgba(0,0,0,0.4) }", ".foo{box-shadow:inset 12px 12px 0 8px #0006}");
    minify_test(".foo { box-shadow: 12px 12px 8px 0px rgba(0,0,0,0.4) }", ".foo{box-shadow:12px 12px 8px #0006}");
    minify_test(".foo { box-shadow: 12px 12px 0px 0px rgba(0,0,0,0.4) }", ".foo{box-shadow:12px 12px #0006}");
    minify_test(".foo { box-shadow: 64px 64px 12px 40px rgba(0,0,0,0.4), 12px 12px 0px 8px rgba(0,0,0,0.4) inset }", ".foo{box-shadow:64px 64px 12px 40px #0006,inset 12px 12px 0 8px #0006}");
  }

  #[test]
  fn test_media() {
    minify_test("@media (min-width: 240px) { .foo { color: chartreuse }}", "@media (min-width:240px){.foo{color:#7fff00}}")
  }

  #[test]
  fn test_merge_rules() {
    test(r#"
      .foo {
        color: red;
      }
      .bar {
        color: red;
      }
    "#, indoc! {r#"
      .foo, .bar {
        color: red;
      }
    "#});
    test(r#"
      .foo {
        color: red;
      }
      .foo {
        background: green;
      }
    "#, indoc! {r#"
      .foo {
        color: red;
        background: green;
      }
    "#});
    test(r#"
      .foo {
        background: red;
      }
      .foo {
        background: green;
      }
    "#, indoc! {r#"
      .foo {
        background: green;
      }
    "#});
    test(r#"
      .foo {
        color: red;
      }

      .bar {
        background: green;
      }
    "#, indoc! {r#"
      .foo {
        color: red;
      }

      .bar {
        background: green;
      }
    "#});
    test(r#"
      .foo {
        color: red;
      }

      .baz {
        color: blue;
      }

      .bar {
        color: red;
      }
    "#, indoc! {r#"
      .foo {
        color: red;
      }

      .baz {
        color: #00f;
      }
      
      .bar {
        color: red;
      }
    "#});
  }

  #[test]
  fn test_opacity() {
    minify_test(".foo { opacity: 0 }", ".foo{opacity:0}");
    minify_test(".foo { opacity: 0% }", ".foo{opacity:0}");
    minify_test(".foo { opacity: 0.5 }", ".foo{opacity:.5}");
    minify_test(".foo { opacity: 50% }", ".foo{opacity:.5}");
    minify_test(".foo { opacity: 1 }", ".foo{opacity:1}");
    minify_test(".foo { opacity: 100% }", ".foo{opacity:1}");
  }

  #[test]
  fn test_transitions() {
    minify_test(".foo { transition-duration: 500ms }", ".foo{transition-duration:.5s}");
    minify_test(".foo { transition-duration: .5s }", ".foo{transition-duration:.5s}");
    minify_test(".foo { transition-duration: 99ms }", ".foo{transition-duration:99ms}");
    minify_test(".foo { transition-duration: .099s }", ".foo{transition-duration:99ms}");
    minify_test(".foo { transition-duration: 2000ms }", ".foo{transition-duration:2s}");
    minify_test(".foo { transition-duration: 2s }", ".foo{transition-duration:2s}");
    minify_test(".foo { transition-duration: calc(1s - 50ms) }", ".foo{transition-duration:.95s}");
    minify_test(".foo { transition-duration: calc(1s - 50ms + 2s) }", ".foo{transition-duration:2.95s}");
    minify_test(".foo { transition-duration: calc((1s - 50ms) * 2) }", ".foo{transition-duration:1.9s}");
    minify_test(".foo { transition-duration: calc(2 * (1s - 50ms)) }", ".foo{transition-duration:1.9s}");
    minify_test(".foo { transition-duration: calc((2s + 50ms) - (1s - 50ms)) }", ".foo{transition-duration:1.1s}");
    minify_test(".foo { transition-duration: 500ms, 50ms }", ".foo{transition-duration:.5s,50ms}");
    minify_test(".foo { transition-delay: 500ms }", ".foo{transition-delay:.5s}");
    minify_test(".foo { transition-property: background }", ".foo{transition-property:background}");
    minify_test(".foo { transition-property: background, opacity }", ".foo{transition-property:background,opacity}");
    minify_test(".foo { transition-timing-function: linear }", ".foo{transition-timing-function:linear}");
    minify_test(".foo { transition-timing-function: ease }", ".foo{transition-timing-function:ease}");
    minify_test(".foo { transition-timing-function: ease-in }", ".foo{transition-timing-function:ease-in}");
    minify_test(".foo { transition-timing-function: ease-out }", ".foo{transition-timing-function:ease-out}");
    minify_test(".foo { transition-timing-function: ease-in-out }", ".foo{transition-timing-function:ease-in-out}");
    minify_test(".foo { transition-timing-function: cubic-bezier(0.25, 0.1, 0.25, 1) }", ".foo{transition-timing-function:ease}");
    minify_test(".foo { transition-timing-function: cubic-bezier(0.42, 0, 1, 1) }", ".foo{transition-timing-function:ease-in}");
    minify_test(".foo { transition-timing-function: cubic-bezier(0, 0, 0.58, 1) }", ".foo{transition-timing-function:ease-out}");
    minify_test(".foo { transition-timing-function: cubic-bezier(0.42, 0, 0.58, 1) }", ".foo{transition-timing-function:ease-in-out}");
    minify_test(".foo { transition-timing-function: cubic-bezier(0.58, 0.2, 0.11, 1.2) }", ".foo{transition-timing-function:cubic-bezier(.58,.2,.11,1.2)}");
    minify_test(".foo { transition-timing-function: step-start }", ".foo{transition-timing-function:step-start}");
    minify_test(".foo { transition-timing-function: step-end }", ".foo{transition-timing-function:step-end}");
    minify_test(".foo { transition-timing-function: steps(1, start) }", ".foo{transition-timing-function:step-start}");
    minify_test(".foo { transition-timing-function: steps(1, jump-start) }", ".foo{transition-timing-function:step-start}");
    minify_test(".foo { transition-timing-function: steps(1, end) }", ".foo{transition-timing-function:step-end}");
    minify_test(".foo { transition-timing-function: steps(1, jump-end) }", ".foo{transition-timing-function:step-end}");
    minify_test(".foo { transition-timing-function: steps(5, jump-start) }", ".foo{transition-timing-function:steps(5,start)}");
    minify_test(".foo { transition-timing-function: steps(5, jump-end) }", ".foo{transition-timing-function:steps(5,end)}");
    minify_test(".foo { transition-timing-function: steps(5, jump-both) }", ".foo{transition-timing-function:steps(5,jump-both)}");
    minify_test(".foo { transition-timing-function: ease-in-out, cubic-bezier(0.42, 0, 1, 1) }", ".foo{transition-timing-function:ease-in-out,ease-in}");
    minify_test(".foo { transition-timing-function: cubic-bezier(0.42, 0, 1, 1), cubic-bezier(0.58, 0.2, 0.11, 1.2) }", ".foo{transition-timing-function:ease-in,cubic-bezier(.58,.2,.11,1.2)}");
    minify_test(".foo { transition-timing-function: step-start, steps(5, jump-start) }", ".foo{transition-timing-function:step-start,steps(5,start)}");
    minify_test(".foo { transition: width 2s ease }", ".foo{transition:width 2s}");
    minify_test(".foo { transition: width 2s ease, height 1000ms cubic-bezier(0.25, 0.1, 0.25, 1) }", ".foo{transition:width 2s,height 1s}");
    minify_test(".foo { transition: width 2s 1s }", ".foo{transition:width 2s 1s}");
    minify_test(".foo { transition: width 2s ease 1s }", ".foo{transition:width 2s 1s}");
    minify_test(".foo { transition: ease-in 1s width 4s }", ".foo{transition:width 1s ease-in 4s}");
    minify_test(".foo { transition: opacity 0s .6s }", ".foo{transition:opacity 0s .6s}");
    test(r#"
      .foo {
        transition-property: opacity;
        transition-duration: 0.09s;
        transition-timing-function: ease-in-out;
        transition-delay: 500ms;
      }
    "#, indoc! {r#"
      .foo {
        transition: opacity 90ms ease-in-out .5s;
      }
    "#});
    test(r#"
      .foo {
        transition: opacity 2s;
        transition-timing-function: ease;
        transition-delay: 500ms;
      }
    "#, indoc! {r#"
      .foo {
        transition: opacity 2s .5s;
      }
    "#});
    test(r#"
      .foo {
        transition-property: opacity;
        transition-duration: 0.09s;
        transition-timing-function: ease-in-out;
        transition-delay: 500ms;
        transition: color 2s;
      }
    "#, indoc! {r#"
      .foo {
        transition: color 2s;
      }
    "#});
    test(r#"
      .foo {
        transition-property: opacity, color;
        transition-duration: 2s, 4s;
        transition-timing-function: ease-in-out, ease-in;
        transition-delay: 500ms, 0s;
      }
    "#, indoc! {r#"
      .foo {
        transition: opacity 2s ease-in-out .5s, color 4s ease-in;
      }
    "#});
    test(r#"
      .foo {
        transition-property: opacity, color;
        transition-duration: 2s;
        transition-timing-function: ease-in-out;
        transition-delay: 500ms;
      }
    "#, indoc! {r#"
      .foo {
        transition-property: opacity, color;
        transition-duration: 2s;
        transition-delay: .5s;
        transition-timing-function: ease-in-out;
      }
    "#});
  }

  #[test]
  fn test_animation() {
    minify_test(".foo { animation-name: test }", ".foo{animation-name:test}");
    minify_test(".foo { animation-name: \"test\" }", ".foo{animation-name:test}");
    minify_test(".foo { animation-name: foo, bar }", ".foo{animation-name:foo,bar}");
    minify_test(".foo { animation-duration: 100ms }", ".foo{animation-duration:.1s}");
    minify_test(".foo { animation-duration: 100ms, 2000ms }", ".foo{animation-duration:.1s,2s}");
    minify_test(".foo { animation-timing-function: ease }", ".foo{animation-timing-function:ease}");
    minify_test(".foo { animation-timing-function: cubic-bezier(0.42, 0, 1, 1) }", ".foo{animation-timing-function:ease-in}");
    minify_test(".foo { animation-timing-function: ease, cubic-bezier(0.42, 0, 1, 1) }", ".foo{animation-timing-function:ease,ease-in}");
    minify_test(".foo { animation-iteration-count: 5 }", ".foo{animation-iteration-count:5}");
    minify_test(".foo { animation-iteration-count: 2.5 }", ".foo{animation-iteration-count:2.5}");
    minify_test(".foo { animation-iteration-count: 2.0 }", ".foo{animation-iteration-count:2}");
    minify_test(".foo { animation-iteration-count: infinite }", ".foo{animation-iteration-count:infinite}");
    minify_test(".foo { animation-iteration-count: 1, infinite }", ".foo{animation-iteration-count:1,infinite}");
    minify_test(".foo { animation-direction: reverse }", ".foo{animation-direction:reverse}");
    minify_test(".foo { animation-direction: alternate, reverse }", ".foo{animation-direction:alternate,reverse}");
    minify_test(".foo { animation-play-state: paused }", ".foo{animation-play-state:paused}");
    minify_test(".foo { animation-play-state: running, paused }", ".foo{animation-play-state:running,paused}");
    minify_test(".foo { animation-delay: 100ms }", ".foo{animation-delay:.1s}");
    minify_test(".foo { animation-delay: 100ms, 2000ms }", ".foo{animation-delay:.1s,2s}");
    minify_test(".foo { animation-fill-mode: forwards }", ".foo{animation-fill-mode:forwards}");
    minify_test(".foo { animation-fill-mode: Backwards,forwards }", ".foo{animation-fill-mode:backwards,forwards}");
    minify_test(".foo { animation: 3s ease-in 1s infinite reverse both running slidein }", ".foo{animation:slidein 3s ease-in 1s infinite reverse both}");
    minify_test(".foo { animation: 3s slidein paused ease 1s 1 reverse both }", ".foo{animation:slidein 3s 1s reverse both paused}");
    minify_test(".foo { animation: 3s ease ease }", ".foo{animation:ease 3s ease}");
    minify_test(".foo { animation: 3s cubic-bezier(0.25, 0.1, 0.25, 1) foo }", ".foo{animation:foo 3s}");
    minify_test(".foo { animation: foo 0s 3s infinite }", ".foo{animation:foo 0s 3s infinite}");
    minify_test(".foo { animation: none }", ".foo{animation:none}");
    test(r#"
      .foo {
        animation-name: foo;
        animation-duration: 0.09s;
        animation-timing-function: ease-in-out;
        animation-iteration-count: 2;
        animation-direction: alternate;
        animation-play-state: running;
        animation-delay: 100ms;
        animation-fill-mode: forwards;
      }
    "#, indoc! {r#"
      .foo {
        animation: foo 90ms ease-in-out .1s 2 alternate forwards;
      }
    "#});
    test(r#"
      .foo {
        animation-name: foo, bar;
        animation-duration: 0.09s, 200ms;
        animation-timing-function: ease-in-out, ease;
        animation-iteration-count: 2, 1;
        animation-direction: alternate, normal;
        animation-play-state: running, paused;
        animation-delay: 100ms, 0s;
        animation-fill-mode: forwards, none;
      }
    "#, indoc! {r#"
      .foo {
        animation: foo 90ms ease-in-out .1s 2 alternate forwards, bar .2s paused;
      }
    "#});
    test(r#"
      .foo {
        animation: bar 200ms;
        animation-timing-function: ease-in-out;
      }
    "#, indoc! {r#"
      .foo {
        animation: bar .2s ease-in-out;
      }
    "#});
    test(r#"
      .foo {
        animation-name: foo, bar;
        animation-duration: 0.09s;
        animation-timing-function: ease-in-out;
        animation-iteration-count: 2;
        animation-direction: alternate;
        animation-play-state: running;
        animation-delay: 100ms;
        animation-fill-mode: forwards;
      }
    "#, indoc! {r#"
      .foo {
        animation-name: foo, bar;
        animation-duration: 90ms;
        animation-timing-function: ease-in-out;
        animation-iteration-count: 2;
        animation-direction: alternate;
        animation-play-state: running;
        animation-delay: .1s;
        animation-fill-mode: forwards;
      }
    "#});
  }

  #[test]
  fn test_transform() {
    minify_test(".foo { transform: translate(2px, 3px)", ".foo{transform:translate(2px,3px)}");
    minify_test(".foo { transform: translate(2px, 0px)", ".foo{transform:translate(2px)}");
    minify_test(".foo { transform: translate(0px, 2px)", ".foo{transform:translateY(2px)}");
    minify_test(".foo { transform: translateX(2px)", ".foo{transform:translate(2px)}");
    minify_test(".foo { transform: translateY(2px)", ".foo{transform:translateY(2px)}");
    minify_test(".foo { transform: translateZ(2px)", ".foo{transform:translateZ(2px)}");
    minify_test(".foo { transform: translate3d(2px, 3px, 4px)", ".foo{transform:translate3d(2px,3px,4px)}");
    minify_test(".foo { transform: translate3d(10%, 20%, 4px)", ".foo{transform:translate3d(10%,20%,4px)}");
    minify_test(".foo { transform: translate3d(2px, 0px, 0px)", ".foo{transform:translate(2px)}");
    minify_test(".foo { transform: translate3d(0px, 2px, 0px)", ".foo{transform:translateY(2px)}");
    minify_test(".foo { transform: translate3d(0px, 0px, 2px)", ".foo{transform:translateZ(2px)}");
    minify_test(".foo { transform: translate3d(2px, 3px, 0px)", ".foo{transform:translate(2px,3px)}");
    minify_test(".foo { transform: scale(2, 3)", ".foo{transform:scale(2,3)}");
    minify_test(".foo { transform: scale(10%, 20%)", ".foo{transform:scale(.1,.2)}");
    minify_test(".foo { transform: scale(2, 2)", ".foo{transform:scale(2)}");
    minify_test(".foo { transform: scale(2, 1)", ".foo{transform:scaleX(2)}");
    minify_test(".foo { transform: scale(1, 2)", ".foo{transform:scaleY(2)}");
    minify_test(".foo { transform: scaleX(2)", ".foo{transform:scaleX(2)}");
    minify_test(".foo { transform: scaleY(2)", ".foo{transform:scaleY(2)}");
    minify_test(".foo { transform: scaleZ(2)", ".foo{transform:scaleZ(2)}");
    minify_test(".foo { transform: scale3d(2, 3, 4)", ".foo{transform:scale3d(2,3,4)}");
    minify_test(".foo { transform: scale3d(2, 1, 1)", ".foo{transform:scaleX(2)}");
    minify_test(".foo { transform: scale3d(1, 2, 1)", ".foo{transform:scaleY(2)}");
    minify_test(".foo { transform: scale3d(1, 1, 2)", ".foo{transform:scaleZ(2)}");
    minify_test(".foo { transform: scale3d(2, 2, 1)", ".foo{transform:scale(2)}");
    minify_test(".foo { transform: rotate(20deg)", ".foo{transform:rotate(20deg)}");
    minify_test(".foo { transform: rotateX(20deg)", ".foo{transform:rotateX(20deg)}");
    minify_test(".foo { transform: rotateY(20deg)", ".foo{transform:rotateY(20deg)}");
    minify_test(".foo { transform: rotateZ(20deg)", ".foo{transform:rotate(20deg)}");
    minify_test(".foo { transform: rotate(360deg)", ".foo{transform:rotate(360deg)}");
    minify_test(".foo { transform: rotate3d(2, 3, 4, 20deg)", ".foo{transform:rotate3d(2,3,4,20deg)}");
    minify_test(".foo { transform: rotate3d(1, 0, 0, 20deg)", ".foo{transform:rotateX(20deg)}");
    minify_test(".foo { transform: rotate3d(0, 1, 0, 20deg)", ".foo{transform:rotateY(20deg)}");
    minify_test(".foo { transform: rotate3d(0, 0, 1, 20deg)", ".foo{transform:rotate(20deg)}");
    minify_test(".foo { transform: skew(20deg)", ".foo{transform:skew(20deg)}");
    minify_test(".foo { transform: skew(20deg, 0deg)", ".foo{transform:skew(20deg)}");
    minify_test(".foo { transform: skew(0deg, 20deg)", ".foo{transform:skewY(20deg)}");
    minify_test(".foo { transform: skewX(20deg)", ".foo{transform:skew(20deg)}");
    minify_test(".foo { transform: skewY(20deg)", ".foo{transform:skewY(20deg)}");
    minify_test(".foo { transform: perspective(10px)", ".foo{transform:perspective(10px)}");
    minify_test(".foo { transform: matrix(1, 2, -1, 1, 80, 80)", ".foo{transform:matrix(1,2,-1,1,80,80)}");
    minify_test(".foo { transform: matrix3d(1, 0, 0, 0, 0, 1, 6, 0, 0, 0, 1, 0, 50, 100, 0, 1.1)", ".foo{transform:matrix3d(1,0,0,0,0,1,6,0,0,0,1,0,50,100,0,1.1)}");
    minify_test(
      ".foo{transform:translate(100px,200px) rotate(45deg) skew(10deg) scale(2)}",
      ".foo{transform:matrix(1.41421,1.41421,-1.16485,1.66358,100,200)}"
    );
    minify_test(
      ".foo{transform:translate(200px,300px) translate(100px,200px) scale(2)}",
      ".foo{transform:matrix(2,0,0,2,300,500)}"
    );
    minify_test(
      ".foo{transform:translate(100px,200px) rotate(45deg)}",
      ".foo{transform:translate(100px,200px)rotate(45deg)}"
    );
    minify_test(
      ".foo{transform:rotate3d(1, 1, 1, 45deg) translate3d(100px, 100px, 10px)}",
      ".foo{transform:rotate3d(1,1,1,45deg)translate3d(100px,100px,10px)}"
    );
    minify_test(
      ".foo{transform:translate3d(100px, 100px, 10px) skew(10deg) scale3d(2, 3, 4)}",
      ".foo{transform:matrix3d(2,0,0,0,.528981,3,0,0,0,0,4,0,100,100,10,1)}"
    );
    minify_test(
      ".foo{transform:matrix3d(0.804737854124365, 0.5058793634016805, -0.31061721752604554, 0, -0.31061721752604554, 0.804737854124365, 0.5058793634016805, 0, 0.5058793634016805, -0.31061721752604554, 0.804737854124365, 0, 100, 100, 10, 1)}",
      ".foo{transform:translate3d(100px,100px,10px)rotate3d(1,1,1,45deg)}"
    );
    minify_test(
      ".foo{transform:matrix3d(1, 0, 0, 0, 0, 0.7071067811865476, 0.7071067811865475, 0, 0, -0.7071067811865475, 0.7071067811865476, 0, 100, 100, 10, 1)}",
      ".foo{transform:translate3d(100px,100px,10px)rotateX(45deg)}"
    );
    minify_test(
      ".foo{transform:translate3d(100px, 200px, 10px) translate(100px, 100px)}",
      ".foo{transform:translate3d(200px,300px,10px)}"
    );
    minify_test(
      ".foo{transform:rotate(45deg) rotate(45deg)}",
      ".foo{transform:rotate(90deg)}"
    );
    minify_test(
      ".foo{transform:matrix(0.7071067811865476, 0.7071067811865475, -0.7071067811865475, 0.7071067811865476, 100, 100)}",
      ".foo{transform:translate(100px,100px)rotate(45deg)}"
    );
    minify_test(
      ".foo{transform:translateX(2in) translateX(50px)}",
      ".foo{transform:translate(242px)}"
    );
    minify_test(
      ".foo{transform:translateX(calc(2in + 50px))}",
      ".foo{transform:translate(242px)}"
    );
    minify_test(
      ".foo{transform:translateX(50%)}",
      ".foo{transform:translate(50%)}"
    );
    minify_test(
      ".foo{transform:translateX(calc(50% - 100px + 20px))}",
      ".foo{transform:translate(calc(50% - 80px))}"
    );
    minify_test(
      ".foo{transform:rotate(calc(10deg + 20deg))}",
      ".foo{transform:rotate(30deg)}"
    );
    minify_test(
      ".foo{transform:rotate(calc(10deg + 0.349066rad))}",
      ".foo{transform:rotate(30deg)}"
    );
    minify_test(
      ".foo{transform:rotate(calc(10deg + 1.5turn))}",
      ".foo{transform:rotate(550deg)}"
    );
    minify_test(
      ".foo{transform:rotate(calc(10deg * 2))}",
      ".foo{transform:rotate(20deg)}"
    );
    minify_test(
      ".foo{transform:rotate(calc(-10deg * 2))}",
      ".foo{transform:rotate(-20deg)}"
    );
    minify_test(
      ".foo{transform:rotate(calc(10deg + var(--test)))}",
      ".foo{transform:rotate(calc(10deg + var(--test)))}"
    );
    minify_test(".foo { transform: scale(calc(10% + 20%))", ".foo{transform:scale(.3)}");
  }
}
