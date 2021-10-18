extern crate napi;
#[macro_use]
extern crate napi_derive;
extern crate serde;
extern crate serde_bytes;
extern crate cssparser;
extern crate selectors;

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
}
