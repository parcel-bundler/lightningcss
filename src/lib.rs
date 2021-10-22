extern crate napi;
#[macro_use]
extern crate napi_derive;
extern crate serde;
extern crate serde_bytes;
extern crate cssparser;
extern crate selectors;
extern crate itertools;

mod parser;
mod media_query;
mod properties;
mod values;

use napi::{CallContext, JsObject, JsUndefined};
use serde::{Deserialize, Serialize};
use cssparser::{Parser, ParserInput, RuleListParser, ToCss};

use parser::TopLevelRuleParser;

#[derive(Serialize, Debug, Deserialize)]
struct Config {
  filename: String,
  #[serde(with = "serde_bytes")]
  code: Vec<u8>,
}

#[js_function(1)]
fn transform(ctx: CallContext) -> napi::Result<JsUndefined> {
  let opts = ctx.get::<JsObject>(0)?;
  let config: Config = ctx.env.from_js_value(opts)?;

  let code = unsafe { std::str::from_utf8_unchecked(&config.code) };

  // for (pos, declarations) in rule_list.flatten() {
  //   println!("{:?}", declarations);
  // }

  let res = compile(code);
  println!("{}", res);

  ctx.env.get_undefined()
}

fn compile(code: &str) -> String {
  let mut input = ParserInput::new(&code);
  let mut parser = Parser::new(&mut input);
  let rule_list = RuleListParser::new_for_stylesheet(&mut parser, TopLevelRuleParser {});

  // for (pos, declarations) in rule_list.flatten() {
  //   println!("{:?}", declarations);
  // }

  let res = rule_list.flatten().map(|(pos, rule)| {
    println!("{:?}", rule);
    let rule = match rule {
      parser::CssRule::Import(import) => {
        parser::CssRule::Import(parser::ImportRule {
          media: import.media,
          url: "test".into()
        })
      },
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

        style.minify();

        parser::CssRule::Style(style)
      },
      r => r
    };
    rule.to_css_string()
  }).collect::<Vec<String>>().join("\n\n");

  res
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
    let res = compile(source);
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
        border: 2px solid #f00;
      }"#
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
        border-color: #f00;
      }"#
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
      }"#
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
      }"#
    });

    test(r#"
      .foo {
        border-left-width: thin;
        border-left-style: dotted;
        border-left-color: red;
      }
    "#, indoc! {r#"
      .foo {
        border-left: thin dotted #f00;
      }"#
    });

    test(r#"
      .foo {
        border-left-width: thick;
        border-left: thin dotted red;
      }
    "#, indoc! {r#"
      .foo {
        border-left: thin dotted #f00;
      }"#
    });

    test(r#"
      .foo {
        border-left-width: thick;
        border: thin dotted red;
      }
    "#, indoc! {r#"
      .foo {
        border: thin dotted #f00;
      }"#
    });

    test(r#"
      .foo {
        border: thin dotted red;
        border-right-width: thick;
      }
    "#, indoc! {r#"
      .foo {
        border: thin dotted #f00;
        border-right-width: thick;
      }"#
    });

    test(r#"
      .foo {
        border: thin dotted red;
        border-right: thick dotted red;
      }
    "#, indoc! {r#"
      .foo {
        border: thin dotted #f00;
        border-right-width: thick;
      }"#
    });

    test(r#"
      .foo {
        border: thin dotted red;
        border-right-width: thick;
        border-right-style: solid;
      }
    "#, indoc! {r#"
      .foo {
        border: thin dotted #f00;
        border-right: thick solid #f00;
      }"#
    });

    test(r#"
      .foo {
        border-top: thin dotted red;
        border-block-start: thick solid green;
      }
    "#, indoc! {r#"
      .foo {
        border-top: thin dotted #f00;
        border-block-start: thick solid #008000;
      }"#
    });

    test(r#"
      .foo {
        border: thin dotted red;
        border-block-start-width: thick;
        border-left-width: medium;
      }
    "#, indoc! {r#"
      .foo {
        border: thin dotted #f00;
        border-block-start-width: thick;
        border-left-width: medium;
      }"#
    });

    test(r#"
      .foo {
        border-block-start: thin dotted red;
        border-inline-end: thin dotted red;
      }
    "#, indoc! {r#"
      .foo {
        border-block-start: thin dotted #f00;
        border-inline-end: thin dotted #f00;
      }"#
    });

    test(r#"
      .foo {
        border-block-start-width: thin;
        border-block-start-style: dotted;
        border-block-start-color: red;
        border-inline-end: thin dotted #f00;
      }
    "#, indoc! {r#"
      .foo {
        border-block-start: thin dotted #f00;
        border-inline-end: thin dotted #f00;
      }"#
    });

    test(r#"
      .foo {
        border-block-start: thin dotted red;
        border-block-end: thin dotted red;
      }
    "#, indoc! {r#"
      .foo {
        border-block: thin dotted #f00;
      }"#
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
      }"#
    });

    test(r#"
      .foo {
        border-image: url(test.png) 60;
        border-image-source: url(foo.png);
      }
    "#, indoc! {r#"
      .foo {
        border-image: url(foo.png) 60;
      }"#
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
      }"#
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
      }"#
    });

    test(r#"
      .foo {
        border-radius: 10px 100px 10px 100px / 120px 120px;
      }
    "#, indoc! {r#"
      .foo {
        border-radius: 10px 100px / 120px;
      }"#
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
      }"#
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
      }"#
    });

    test(r#"
      .foo {
        border-start-start-radius: 10px;
        border-radius: 10px 100px 10px 100px / 120px 120px;
      }
    "#, indoc! {r#"
      .foo {
        border-radius: 10px 100px / 120px;
      }"#
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
      }"#
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
      }"#
    });

    test(r#"
      .foo {
        margin-block-start: 15px;
        margin-block-end: 15px;
      }
    "#, indoc! {r#"
      .foo {
        margin-block: 15px;
      }"#
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
      }"#
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
      }"#
    });

    test(r#"
      .foo {
        padding-block-start: 15px;
        padding-block-end: 15px;
      }
    "#, indoc! {r#"
      .foo {
        padding-block: 15px;
      }"#
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
      }"#
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
      }"#
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
        background: #f00;
      }"#
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
        background: #808080 url(chess.png) 40% / 10em round fixed border-box;
      }"#
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
        background: url(img.png) right 20px top 20px / 50px 50px repeat-x, #808080 url(test.jpg) 10px 15px no-repeat;
      }"#
    });
  }
}
