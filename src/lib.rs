extern crate serde;
extern crate serde_bytes;
extern crate cssparser;
extern crate selectors;
extern crate itertools;
extern crate smallvec;
extern crate bitflags;

#[cfg(target_os = "macos")]
#[global_allocator]
static GLOBAL: jemallocator::Jemalloc = jemallocator::Jemalloc;

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

use serde::{Deserialize, Serialize};
use cssparser::{Parser, ParserInput, RuleListParser};
use crate::traits::ToCss;
use printer::Printer;
use properties::VendorPrefix;
use properties::prefixes::{Browsers, Feature};
use declaration::DeclarationHandler;
use std::collections::HashMap;

use parser::TopLevelRuleParser;

// ---------------------------------------------

#[cfg(target_arch = "wasm32")]
use serde_wasm_bindgen::{from_value};
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn transform(config_val: JsValue) -> Result<JsValue, JsValue> {
  let config: Config = from_value(config_val).map_err(JsValue::from)?;

  let code = unsafe { std::str::from_utf8_unchecked(&config.code) };

  Ok(compile(code, config.minify.unwrap_or(false), config.targets).into())
}

// ---------------------------------------------

#[cfg(not(target_arch = "wasm32"))]
extern crate napi;
#[cfg(not(target_arch = "wasm32"))]
#[macro_use]
extern crate napi_derive;
#[cfg(not(target_arch = "wasm32"))]
use napi::{CallContext, JsObject, JsBuffer};

#[cfg(not(target_arch = "wasm32"))]
#[js_function(1)]
fn transform(ctx: CallContext) -> napi::Result<JsBuffer> {
  let opts = ctx.get::<JsObject>(0)?;
  let config: Config = ctx.env.from_js_value(opts)?;
  let code = unsafe { std::str::from_utf8_unchecked(&config.code) };
  let res = compile(code, config.minify.unwrap_or(false), config.targets);

  Ok(ctx.env.create_buffer_with_data(res.into_bytes())?.into_raw())
}

#[cfg(not(target_arch = "wasm32"))]
#[module_exports]
fn init(mut exports: JsObject) -> napi::Result<()> {
  exports.create_named_method("transform", transform)?;

  Ok(())
}

// ---------------------------------------------

#[derive(Serialize, Debug, Deserialize)]
struct Config {
  pub filename: String,
  #[serde(with = "serde_bytes")]
  pub code: Vec<u8>,
  pub targets: Option<Browsers>,
  pub minify: Option<bool>
}

fn compile(code: &str, minify: bool, targets: Option<Browsers>) -> String {
  let mut input = ParserInput::new(&code);
  let mut parser = Parser::new(&mut input);
  let rule_list = RuleListParser::new_for_stylesheet(&mut parser, TopLevelRuleParser {});

  let mut dest = String::new();
  let mut printer = Printer::new(&mut dest, minify);
  let mut first = true;
  let mut rules = vec![];

  let mut handler = DeclarationHandler::new(false, targets);
  let mut important_handler = DeclarationHandler::new(true, targets);
  let mut keyframe_rules = HashMap::new();

  for rule in rule_list {
    let rule = if let Ok((_, rule)) = rule {
      rule
    } else {
      continue
    };
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
          keyframe.declarations.minify(&mut handler, &mut important_handler);
        }

        macro_rules! set_prefix {
          ($keyframes: ident) => {
            if $keyframes.vendor_prefix.contains(VendorPrefix::None) {
              if let Some(targets) = targets {
                $keyframes.vendor_prefix = Feature::AtKeyframes.prefixes_for(targets)
              }
            }
          };
        }

        // If there is an existing rule with the same name and identical keyframes,
        // merge the vendor prefixes from this rule into it.
        if let Some(existing_idx) = keyframe_rules.get(&keyframes.name) {
          if let Some(parser::CssRule::Keyframes(existing)) = &mut rules.get_mut(*existing_idx) {
            if existing.keyframes == keyframes.keyframes {
              existing.vendor_prefix |= keyframes.vendor_prefix;
              set_prefix!(existing);
              continue;
            }
          }
        }

        set_prefix!(keyframes);
        keyframe_rules.insert(keyframes.name.clone(), rules.len());
        parser::CssRule::Keyframes(keyframes)
      }
      parser::CssRule::Media(mut media) => {
        for rule in media.rules.iter_mut() {
          match rule {
            parser::CssRule::Style(style) => {
              style.declarations.minify(&mut handler, &mut important_handler)
            }
            _ => {}
          }
        }
        parser::CssRule::Media(media)
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

        style.declarations.minify(&mut handler, &mut important_handler);

        if let Some(parser::CssRule::Style(last_style_rule)) = rules.last_mut() {
          if style.selectors == last_style_rule.selectors {
            last_style_rule.declarations.declarations.extend(style.declarations.declarations);
            last_style_rule.declarations.minify(&mut handler, &mut important_handler);
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

#[cfg(test)]
mod tests {
  use super::*;
  extern crate indoc;
  use self::indoc::indoc;

  fn test(source: &str, expected: &str) {
    let res = compile(source, false, None);
    assert_eq!(res, expected);
  }

  fn minify_test(source: &str, expected: &str) {
    let res = compile(source, true, None);
    assert_eq!(res, expected);
  }

  fn prefix_test(source: &str, expected: &str, targets: Browsers) {
    let res = compile(source, false, Some(targets));
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

    test(r#"
      .foo {
        -webkit-border-image: url("test.png") 60;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-border-image: url(test.png) 60;
      }
    "#
    });

    test(r#"
      .foo {
        -webkit-border-image: url("test.png") 60;
        border-image: url("test.png") 60;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-border-image: url(test.png) 60;
        border-image: url(test.png) 60;
      }
    "#
    });

    test(r#"
      .foo {
        -webkit-border-image: url("test.png") 60;
        border-image-source: url(foo.png);
      }
    "#, indoc! {r#"
      .foo {
        -webkit-border-image: url(test.png) 60;
        border-image-source: url(foo.png);
      }
    "#
    });

    prefix_test(r#"
      .foo {
        border-image: url("test.png") 60;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-border-image: url(test.png) 60;
        -moz-border-image: url(test.png) 60;
        -o-border-image: url(test.png) 60;
        border-image: url(test.png) 60;
      }
    "#
    }, Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      opera: Some(12 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-image: url(foo.png) 10 40 fill / 10px round;
      }
    "#, indoc! {r#"
      .foo {
        border-image: url(foo.png) 10 40 fill / 10px round;
      }
    "#
    }, Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      opera: Some(12 << 16),
      ..Browsers::default()
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

    test(r#"
      .foo {
        -webkit-border-radius: 10px 100px 10px 100px;
        -moz-border-radius: 10px 100px 10px 100px;
        border-radius: 10px 100px 10px 100px;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-border-radius: 10px 100px;
        -moz-border-radius: 10px 100px;
        border-radius: 10px 100px;
      }
    "#
    });

    test(r#"
      .foo {
        -webkit-border-radius: 10px 100px 10px 100px;
        -moz-border-radius: 20px;
        border-radius: 30px;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-border-radius: 10px 100px;
        -moz-border-radius: 20px;
        border-radius: 30px;
      }
    "#
    });

    test(r#"
      .foo {
        -webkit-border-top-left-radius: 10px;
        -moz-border-top-left-radius: 10px;
        border-top-left-radius: 10px;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-border-top-left-radius: 10px;
        -moz-border-top-left-radius: 10px;
        border-top-left-radius: 10px;
      }
    "#
    });

    prefix_test(r#"
      .foo {
        border-radius: 30px;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-border-radius: 30px;
        -moz-border-radius: 30px;
        border-radius: 30px;
      }
    "#
    }, Browsers {
      safari: Some(4 << 16),
      firefox: Some(3 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-top-left-radius: 30px;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-border-top-left-radius: 30px;
        -moz-border-top-left-radius: 30px;
        border-top-left-radius: 30px;
      }
    "#
    }, Browsers {
      safari: Some(4 << 16),
      firefox: Some(3 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        -webkit-border-radius: 30px;
        -moz-border-radius: 30px;
        border-radius: 30px;
      }
    "#, indoc! {r#"
      .foo {
        border-radius: 30px;
      }
    "#
    }, Browsers {
      safari: Some(14 << 16),
      firefox: Some(46 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        -webkit-border-top-left-radius: 30px;
        -moz-border-top-left-radius: 30px;
        border-top-left-radius: 30px;
      }
    "#, indoc! {r#"
      .foo {
        border-top-left-radius: 30px;
      }
    "#
    }, Browsers {
      safari: Some(14 << 16),
      firefox: Some(46 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        -webkit-border-radius: 30px;
        -moz-border-radius: 30px;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-border-radius: 30px;
        -moz-border-radius: 30px;
      }
    "#
    }, Browsers {
      safari: Some(14 << 16),
      firefox: Some(46 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        -webkit-border-top-left-radius: 30px;
        -moz-border-top-right-radius: 30px;
        border-bottom-left-radius: 30px;
        border-bottom-right-radius: 30px;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-border-top-left-radius: 30px;
        -moz-border-top-right-radius: 30px;
        border-bottom-left-radius: 30px;
        border-bottom-right-radius: 30px;
      }
    "#
    }, Browsers {
      safari: Some(14 << 16),
      firefox: Some(46 << 16),
      ..Browsers::default()
    })
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

    test(r#"
      .foo {
        background: url(img.png) gray;
        background-clip: content-box;
        -webkit-background-clip: text;
      }
    "#, indoc! {r#"
      .foo {
        background: gray url(img.png) padding-box content-box;
        -webkit-background-clip: text;
      }
    "#
    });

    test(r#"
      .foo {
        background: url(img.png) gray;
        -webkit-background-clip: text;
        background-clip: content-box;
      }
    "#, indoc! {r#"
      .foo {
        background: gray url(img.png);
        -webkit-background-clip: text;
        background-clip: content-box;
      }
    "#
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

    test(r#"
      .foo {
        -webkit-flex-grow: 1;
        -webkit-flex-shrink: 1;
        -webkit-flex-basis: auto;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-flex: auto;
      }
    "#
    });
    test(r#"
      .foo {
        -webkit-flex-grow: 1;
        -webkit-flex-shrink: 1;
        -webkit-flex-basis: auto;
        flex-grow: 1;
        flex-shrink: 1;
        flex-basis: auto;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-flex: auto;
        flex: auto;
      }
    "#
    });
    prefix_test(r#"
      .foo {
        -webkit-box-orient: horizontal;
        -webkit-box-direction: normal;
        flex-direction: row;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-box-orient: horizontal;
        -webkit-box-direction: normal;
        -webkit-flex-direction: row;
        flex-direction: row;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        flex-direction: row;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-box-orient: horizontal;
        -moz-box-orient: horizontal;
        -webkit-box-direction: normal;
        -moz-box-direction: normal;
        -webkit-flex-direction: row;
        -ms-flex-direction: row;
        flex-direction: row;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -webkit-box-orient: horizontal;
        -webkit-box-direction: normal;
        -moz-box-orient: horizontal;
        -moz-box-direction: normal;
        -webkit-flex-direction: row;
        -ms-flex-direction: row;
        flex-direction: row;
      }
    "#, indoc! {r#"
      .foo {
        flex-direction: row;
      }
    "#},
    Browsers {
      safari: Some(14 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        flex-wrap: wrap;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-box-lines: multiple;
        -moz-box-lines: multiple;
        -webkit-flex-wrap: wrap;
        -ms-flex-wrap: wrap;
        flex-wrap: wrap;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -webkit-box-lines: multiple;
        -moz-box-lines: multiple;
        -webkit-flex-wrap: wrap;
        -ms-flex-wrap: wrap;
        flex-wrap: wrap;
      }
    "#, indoc! {r#"
      .foo {
        flex-wrap: wrap;
      }
    "#},
    Browsers {
      safari: Some(11 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        flex-flow: row wrap;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-box-orient: horizontal;
        -moz-box-orient: horizontal;
        -webkit-box-direction: normal;
        -moz-box-direction: normal;
        -webkit-flex-flow: wrap;
        -ms-flex-flow: wrap;
        flex-flow: wrap;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -webkit-box-orient: horizontal;
        -moz-box-orient: horizontal;
        -webkit-box-direction: normal;
        -moz-box-direction: normal;
        -webkit-flex-flow: wrap;
        -ms-flex-flow: wrap;
        flex-flow: wrap;
      }
    "#, indoc! {r#"
      .foo {
        flex-flow: wrap;
      }
    "#},
    Browsers {
      safari: Some(11 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        flex-grow: 1;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-box-flex: 1;
        -moz-box-flex: 1;
        -ms-flex-positive: 1;
        -webkit-flex-grow: 1;
        flex-grow: 1;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -webkit-box-flex: 1;
        -moz-box-flex: 1;
        -ms-flex-positive: 1;
        -webkit-flex-grow: 1;
        flex-grow: 1;
      }
    "#, indoc! {r#"
      .foo {
        flex-grow: 1;
      }
    "#},
    Browsers {
      safari: Some(11 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        flex-shrink: 1;
      }
    "#, indoc! {r#"
      .foo {
        -ms-flex-negative: 1;
        -webkit-flex-shrink: 1;
        flex-shrink: 1;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -ms-flex-negative: 1;
        -webkit-flex-shrink: 1;
        flex-shrink: 1;
      }
    "#, indoc! {r#"
      .foo {
        flex-shrink: 1;
      }
    "#},
    Browsers {
      safari: Some(11 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        flex-basis: 1px;
      }
    "#, indoc! {r#"
      .foo {
        -ms-flex-preferred-size: 1px;
        -webkit-flex-basis: 1px;
        flex-basis: 1px;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -ms-flex-preferred-size: 1px;
        -webkit-flex-basis: 1px;
        flex-basis: 1px;
      }
    "#, indoc! {r#"
      .foo {
        flex-basis: 1px;
      }
    "#},
    Browsers {
      safari: Some(11 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        flex: 1;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-box-flex: 1;
        -moz-box-flex: 1;
        -webkit-flex: 1;
        -ms-flex: 1;
        flex: 1;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -webkit-box-flex: 1;
        -moz-box-flex: 1;
        -webkit-flex: 1;
        -ms-flex: 1;
        flex: 1;
      }
    "#, indoc! {r#"
      .foo {
        flex: 1;
      }
    "#},
    Browsers {
      safari: Some(11 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        align-content: space-between;
      }
    "#, indoc! {r#"
      .foo {
        -ms-flex-line-pack: justify;
        -webkit-align-content: space-between;
        align-content: space-between;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -ms-flex-line-pack: justify;
        -webkit-align-content: space-between;
        align-content: space-between;
      }
    "#, indoc! {r#"
      .foo {
        align-content: space-between;
      }
    "#},
    Browsers {
      safari: Some(11 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        justify-content: space-between;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-box-pack: justify;
        -moz-box-pack: justify;
        -ms-flex-pack: justify;
        -webkit-justify-content: space-between;
        justify-content: space-between;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -webkit-box-pack: justify;
        -moz-box-pack: justify;
        -ms-flex-pack: justify;
        -webkit-justify-content: space-between;
        justify-content: space-between;
      }
    "#, indoc! {r#"
      .foo {
        justify-content: space-between;
      }
    "#},
    Browsers {
      safari: Some(11 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        place-content: space-between flex-end;
      }
    "#, indoc! {r#"
      .foo {
        -ms-flex-line-pack: justify;
        -webkit-box-pack: end;
        -moz-box-pack: end;
        -ms-flex-pack: end;
        -webkit-align-content: space-between;
        -webkit-justify-content: flex-end;
        place-content: space-between flex-end;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -ms-flex-line-pack: justify;
        -webkit-box-pack: end;
        -moz-box-pack: end;
        -ms-flex-pack: end;
        -webkit-align-content: space-between;
        -webkit-justify-content: flex-end;
        place-content: space-between flex-end;
      }
    "#, indoc! {r#"
      .foo {
        place-content: space-between flex-end;
      }
    "#},
    Browsers {
      safari: Some(11 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        align-self: flex-end;
      }
    "#, indoc! {r#"
      .foo {
        -ms-flex-item-align: end;
        -webkit-align-self: flex-end;
        align-self: flex-end;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -ms-flex-item-align: end;
        -webkit-align-self: flex-end;
        align-self: flex-end;
      }
    "#, indoc! {r#"
      .foo {
        align-self: flex-end;
      }
    "#},
    Browsers {
      safari: Some(11 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        place-self: center flex-end;
      }
    "#, indoc! {r#"
      .foo {
        -ms-flex-item-align: center;
        -webkit-align-self: center;
        place-self: center flex-end;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -ms-flex-item-align: center;
        -webkit-align-self: center;
        place-self: center flex-end;
      }
    "#, indoc! {r#"
      .foo {
        place-self: center flex-end;
      }
    "#},
    Browsers {
      safari: Some(11 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        align-items: flex-end;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-box-align: end;
        -moz-box-align: end;
        -ms-flex-align: end;
        -webkit-align-items: flex-end;
        align-items: flex-end;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -webkit-box-align: end;
        -moz-box-align: end;
        -ms-flex-align: end;
        -webkit-align-items: flex-end;
        align-items: flex-end;
      }
    "#, indoc! {r#"
      .foo {
        align-items: flex-end;
      }
    "#},
    Browsers {
      safari: Some(11 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        place-items: flex-end center;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-box-align: end;
        -moz-box-align: end;
        -ms-flex-align: end;
        -webkit-align-items: flex-end;
        place-items: flex-end center;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -webkit-box-align: end;
        -moz-box-align: end;
        -ms-flex-align: end;
        -webkit-align-items: flex-end;
        place-items: flex-end center;
      }
    "#, indoc! {r#"
      .foo {
        place-items: flex-end center;
      }
    "#},
    Browsers {
      safari: Some(11 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        order: 1;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-box-ordinal-group: 1;
        -moz-box-ordinal-group: 1;
        -ms-flex-order: 1;
        -webkit-order: 1;
        order: 1;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      firefox: Some(4 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -webkit-box-ordinal-group: 1;
        -moz-box-ordinal-group: 1;
        -ms-flex-order: 1;
        -webkit-order: 1;
        order: 1;
      }
    "#, indoc! {r#"
      .foo {
        order: 1;
      }
    "#},
    Browsers {
      safari: Some(11 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        -ms-flex: 0 0 8%;
        flex: 0 0 5%;
      }
    "#, indoc! {r#"
      .foo {
        -ms-flex: 0 0 8%;
        flex: 0 0 5%;
      }
    "#},
    Browsers {
      safari: Some(11 << 16),
      ..Browsers::default()
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
  fn test_vertical_align() {
    minify_test(".foo { vertical-align: middle }", ".foo{vertical-align:middle}");
    minify_test(".foo { vertical-align: 0.3em }", ".foo{vertical-align:.3em}");
  }

  #[test]
  fn test_selectors() {
    minify_test("[foo=\"baz\"] {}", "[foo=baz]{}");
    minify_test("[foo=\"foo bar\"] {}", "[foo=foo\\ bar]{}");
    minify_test("[foo=\"foo bar baz\"] {}", "[foo=\"foo bar baz\"]{}");
    minify_test("[foo=\"\"] {}", "[foo=\"\"]{}");
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
    minify_test(r#"
      @-webkit-keyframes test {
        from {
          background: green;
          background-color: red;
        }

        100% {
          background: blue
        }
      }
    "#, "@-webkit-keyframes test{0%{background:red}to{background:#00f}}");
    minify_test(r#"
      @-moz-keyframes test {
        from {
          background: green;
          background-color: red;
        }

        100% {
          background: blue
        }
      }
    "#, "@-moz-keyframes test{0%{background:red}to{background:#00f}}");
    minify_test(r#"
      @-webkit-keyframes test {
        from {
          background: green;
          background-color: red;
        }

        100% {
          background: blue
        }
      }
      @-moz-keyframes test {
        from {
          background: green;
          background-color: red;
        }

        100% {
          background: blue
        }
      }
    "#, "@-webkit-keyframes test{0%{background:red}to{background:#00f}}@-moz-keyframes test{0%{background:red}to{background:#00f}}");

    prefix_test(r#"
      @keyframes test {
        from {
          background: green;
        }
        to {
          background: blue
        }
      }
    "#, indoc! { r#"
      @-webkit-keyframes test {
        from {
          background: green;
        }

        to {
          background: #00f;
        }
      }

      @-moz-keyframes test {
        from {
          background: green;
        }

        to {
          background: #00f;
        }
      }

      @keyframes test {
        from {
          background: green;
        }

        to {
          background: #00f;
        }
      }
    "#}, Browsers {
      safari: Some(5 << 16),
      firefox: Some(6 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      @-webkit-keyframes test {
        from {
          background: green;
        }

        to {
          background: blue;
        }
      }
      @-moz-keyframes test {
        from {
          background: green;
        }

        to {
          background: blue;
        }
      }
      @keyframes test {
        from {
          background: green;
        }
        to {
          background: blue
        }
      }
    "#, indoc! { r#"
      @keyframes test {
        from {
          background: green;
        }

        to {
          background: #00f;
        }
      }
    "#}, Browsers {
      safari: Some(10 << 16),
      firefox: Some(17 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      @-webkit-keyframes test1 {
        from {
          background: green;
        }

        to {
          background: blue;
        }
      }

      @-moz-keyframes test2 {
        from {
          background: green;
        }

        to {
          background: blue;
        }
      }

      @keyframes test3 {
        from {
          background: green;
        }
        to {
          background: blue
        }
      }
    "#, indoc! { r#"
      @-webkit-keyframes test1 {
        from {
          background: green;
        }

        to {
          background: #00f;
        }
      }

      @-moz-keyframes test2 {
        from {
          background: green;
        }

        to {
          background: #00f;
        }
      }

      @keyframes test3 {
        from {
          background: green;
        }

        to {
          background: #00f;
        }
      }
    "#}, Browsers {
      safari: Some(10 << 16),
      firefox: Some(17 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      @-webkit-keyframes test {
        from {
          background: green;
        }

        to {
          background: red;
        }
      }
      @-moz-keyframes test {
        from {
          background: green;
        }

        to {
          background: pink;
        }
      }
      @keyframes test {
        from {
          background: green;
        }
        to {
          background: blue
        }
      }
    "#, indoc! { r#"
      @-webkit-keyframes test {
        from {
          background: green;
        }

        to {
          background: red;
        }
      }

      @-moz-keyframes test {
        from {
          background: green;
        }

        to {
          background: pink;
        }
      }

      @keyframes test {
        from {
          background: green;
        }

        to {
          background: #00f;
        }
      }
    "#}, Browsers {
      safari: Some(10 << 16),
      firefox: Some(17 << 16),
      ..Browsers::default()
    });
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
    minify_test(".foo { border-width: calc(1px + 2px) }", ".foo{border-width:3px}");
    minify_test(".foo { border-width: calc(1em + 2px + 2em + 3px) }", ".foo{border-width:calc(3em + 5px)}");
    
    minify_test(".foo { border-width: min(1em, 2px) }", ".foo{border-width:min(1em,2px)}");
    minify_test(".foo { border-width: min(1em + 2em, 2px + 2px) }", ".foo{border-width:min(3em,4px)}");
    minify_test(".foo { border-width: min(1em + 2px, 2px + 1em) }", ".foo{border-width:min(1em + 2px,2px + 1em)}");
    minify_test(".foo { border-width: min(1em + 2px + 2px, 2px + 1em + 1px) }", ".foo{border-width:min(1em + 4px,3px + 1em)}");
    minify_test(".foo { border-width: min(2px + 1px, 3px + 4px) }", ".foo{border-width:3px}");
    minify_test(".foo { border-width: min(1px, 1em, 2px, 3in) }", ".foo{border-width:min(1px,1em)}");

    minify_test(".foo { border-width: max(1em, 2px) }", ".foo{border-width:max(1em,2px)}");
    minify_test(".foo { border-width: max(1em + 2em, 2px + 2px) }", ".foo{border-width:max(3em,4px)}");
    minify_test(".foo { border-width: max(1em + 2px, 2px + 1em) }", ".foo{border-width:max(1em + 2px,2px + 1em)}");
    minify_test(".foo { border-width: max(1em + 2px + 2px, 2px + 1em + 1px) }", ".foo{border-width:max(1em + 4px,3px + 1em)}");
    minify_test(".foo { border-width: max(2px + 1px, 3px + 4px) }", ".foo{border-width:7px}");
    minify_test(".foo { border-width: max(1px, 1em, 2px, 3in) }", ".foo{border-width:max(3in,1em)}");

    minify_test(".foo { border-width: clamp(1px, 2px, 3px) }", ".foo{border-width:2px}");
    minify_test(".foo { border-width: clamp(1px, 10px, 3px) }", ".foo{border-width:3px}");
    minify_test(".foo { border-width: clamp(5px, 2px, 10px) }", ".foo{border-width:5px}");
    minify_test(".foo { border-width: clamp(100px, 2px, 10px) }", ".foo{border-width:100px}");
    minify_test(".foo { border-width: clamp(5px + 5px, 5px + 7px, 10px + 20px) }", ".foo{border-width:12px}");

    minify_test(".foo { border-width: clamp(1em, 2px, 4vh) }", ".foo{border-width:clamp(1em,2px,4vh)}");
    minify_test(".foo { border-width: clamp(1em, 2em, 4vh) }", ".foo{border-width:min(2em,4vh)}");
    minify_test(".foo { border-width: clamp(1em, 2vh, 4vh) }", ".foo{border-width:max(1em,2vh)}");
    minify_test(".foo { border-width: clamp(1px, 1px + 2em, 4px) }", ".foo{border-width:clamp(1px,1px + 2em,4px)}");
    minify_test(".foo { border-width: clamp(1px, 2pt, 1in) }", ".foo{border-width:2pt}");
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

    test(r#"
      .foo {
        -webkit-transition-property: opacity, color;
        -webkit-transition-duration: 2s, 4s;
        -webkit-transition-timing-function: ease-in-out, ease-in;
        -webkit-transition-delay: 500ms, 0s;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-transition: opacity 2s ease-in-out .5s, color 4s ease-in;
      }
    "#});

    test(r#"
      .foo {
        -webkit-transition-property: opacity, color;
        -webkit-transition-duration: 2s, 4s;
        -webkit-transition-timing-function: ease-in-out, ease-in;
        -webkit-transition-delay: 500ms, 0s;
        -moz-transition-property: opacity, color;
        -moz-transition-duration: 2s, 4s;
        -moz-transition-timing-function: ease-in-out, ease-in;
        -moz-transition-delay: 500ms, 0s;
        transition-property: opacity, color;
        transition-duration: 2s, 4s;
        transition-timing-function: ease-in-out, ease-in;
        transition-delay: 500ms, 0s;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-transition: opacity 2s ease-in-out .5s, color 4s ease-in;
        -moz-transition: opacity 2s ease-in-out .5s, color 4s ease-in;
        transition: opacity 2s ease-in-out .5s, color 4s ease-in;
      }
    "#});

    test(r#"
      .foo {
        -webkit-transition-property: opacity, color;
        -moz-transition-property: opacity, color;
        transition-property: opacity, color;
        -webkit-transition-duration: 2s, 4s;
        -moz-transition-duration: 2s, 4s;
        transition-duration: 2s, 4s;
        -webkit-transition-timing-function: ease-in-out, ease-in;
        transition-timing-function: ease-in-out, ease-in;
        -moz-transition-timing-function: ease-in-out, ease-in;
        -webkit-transition-delay: 500ms, 0s;
        -moz-transition-delay: 500ms, 0s;
        transition-delay: 500ms, 0s;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-transition: opacity 2s ease-in-out .5s, color 4s ease-in;
        -moz-transition: opacity 2s ease-in-out .5s, color 4s ease-in;
        transition: opacity 2s ease-in-out .5s, color 4s ease-in;
      }
    "#});

    test(r#"
      .foo {
        -webkit-transition-property: opacity;
        -moz-transition-property: color;
        transition-property: opacity, color;
        -webkit-transition-duration: 2s;
        -moz-transition-duration: 4s;
        transition-duration: 2s, 4s;
        -webkit-transition-timing-function: ease-in-out;
        -moz-transition-timing-function: ease-in-out;
        transition-timing-function: ease-in-out, ease-in;
        -webkit-transition-delay: 500ms;
        -moz-transition-delay: 0s;
        transition-delay: 500ms, 0s;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-transition-property: opacity;
        -moz-transition-property: color;
        transition-property: opacity, color;
        -webkit-transition-duration: 2s;
        -moz-transition-duration: 4s;
        transition-duration: 2s, 4s;
        -webkit-transition-timing-function: ease-in-out;
        -moz-transition-timing-function: ease-in-out;
        -webkit-transition-delay: .5s;
        transition-timing-function: ease-in-out, ease-in;
        -moz-transition-delay: 0s;
        transition-delay: .5s, 0s;
      }
    "#});

    test(r#"
      .foo {
        -webkit-transition-property: opacity;
        transition-property: opacity, color;
        -moz-transition-property: color;
        -webkit-transition-duration: 2s;
        transition-duration: 2s, 4s;
        -moz-transition-duration: 4s;
        -webkit-transition-timing-function: ease-in-out;
        transition-timing-function: ease-in-out, ease-in;
        -moz-transition-timing-function: ease-in-out;
        -webkit-transition-delay: 500ms;
        transition-delay: 500ms, 0s;
        -moz-transition-delay: 0s;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-transition-property: opacity;
        transition-property: opacity, color;
        -moz-transition-property: color;
        -webkit-transition-duration: 2s;
        transition-duration: 2s, 4s;
        -moz-transition-duration: 4s;
        -webkit-transition-timing-function: ease-in-out;
        transition-timing-function: ease-in-out, ease-in;
        -webkit-transition-delay: .5s;
        -moz-transition-timing-function: ease-in-out;
        transition-delay: .5s, 0s;
        -moz-transition-delay: 0s;
      }
    "#});

    test(r#"
      .foo {
        transition: opacity 2s;
        -webkit-transition-duration: 2s;
      }
    "#, indoc! {r#"
      .foo {
        transition: opacity 2s;
        -webkit-transition-duration: 2s;
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
    test(r#"
      .foo {
        -webkit-animation-name: foo;
        -webkit-animation-duration: 0.09s;
        -webkit-animation-timing-function: ease-in-out;
        -webkit-animation-iteration-count: 2;
        -webkit-animation-direction: alternate;
        -webkit-animation-play-state: running;
        -webkit-animation-delay: 100ms;
        -webkit-animation-fill-mode: forwards;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-animation: foo 90ms ease-in-out .1s 2 alternate forwards;
      }
    "#});
    test(r#"
      .foo {
        -moz-animation: bar 200ms;
        -moz-animation-timing-function: ease-in-out;
      }
    "#, indoc! {r#"
      .foo {
        -moz-animation: bar .2s ease-in-out;
      }
    "#});
    test(r#"
      .foo {
        -webkit-animation: bar 200ms;
        -webkit-animation-timing-function: ease-in-out;
        -moz-animation: bar 200ms;
        -moz-animation-timing-function: ease-in-out;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-animation: bar .2s ease-in-out;
        -moz-animation: bar .2s ease-in-out;
      }
    "#});

    prefix_test(r#"
      .foo {
        animation: bar .2s ease-in-out;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-animation: bar .2s ease-in-out;
        -moz-animation: bar .2s ease-in-out;
        animation: bar .2s ease-in-out;
      }
    "#}, Browsers {
      firefox: Some(6 << 16),
      safari: Some(6 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        -webkit-animation: bar .2s ease-in-out;
        -moz-animation: bar .2s ease-in-out;
        animation: bar .2s ease-in-out;
      }
    "#, indoc! {r#"
      .foo {
        animation: bar .2s ease-in-out;
      }
    "#}, Browsers {
      firefox: Some(20 << 16),
      safari: Some(14 << 16),
      ..Browsers::default()
    });
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
    minify_test(".foo { transform: scale(calc(.1 + .2))", ".foo{transform:scale(.3)}");

    minify_test(".foo { -webkit-transform: scale(calc(10% + 20%))", ".foo{-webkit-transform:scale(.3)}");

    minify_test(".foo { translate: 1px 2px 3px }", ".foo{translate:1px 2px 3px}");
    minify_test(".foo { translate: 1px 0px 0px }", ".foo{translate:1px}");
    minify_test(".foo { translate: 1px 2px 0px }", ".foo{translate:1px 2px}");
    minify_test(".foo { translate: 1px 0px 2px }", ".foo{translate:1px 0 2px}");
    minify_test(".foo { translate: none }", ".foo{translate:0}");
    minify_test(".foo { rotate: 10deg }", ".foo{rotate:10deg}");
    minify_test(".foo { rotate: z 10deg }", ".foo{rotate:10deg}");
    minify_test(".foo { rotate: 0 0 1 10deg }", ".foo{rotate:10deg}");
    minify_test(".foo { rotate: x 10deg }", ".foo{rotate:x 10deg}");
    minify_test(".foo { rotate: 1 0 0 10deg }", ".foo{rotate:x 10deg}");
    minify_test(".foo { rotate: y 10deg }", ".foo{rotate:y 10deg}");
    minify_test(".foo { rotate: 0 1 0 10deg }", ".foo{rotate:y 10deg}");
    minify_test(".foo { rotate: 1 1 1 10deg }", ".foo{rotate:1 1 1 10deg}");
    minify_test(".foo { rotate: 0 0 1 0deg }", ".foo{rotate:none}");
    minify_test(".foo { rotate: none }", ".foo{rotate:none}");
    minify_test(".foo { scale: 1 }", ".foo{scale:1}");
    minify_test(".foo { scale: 1 1 }", ".foo{scale:1}");
    minify_test(".foo { scale: 1 1 1 }", ".foo{scale:1}");
    minify_test(".foo { scale: none }", ".foo{scale:1}");
    minify_test(".foo { scale: 1 0 }", ".foo{scale:1 0}");
    minify_test(".foo { scale: 1 0 1 }", ".foo{scale:1 0}");
    minify_test(".foo { scale: 1 0 0 }", ".foo{scale:1 0 0}");

    minify_test(".foo { transform: scale(3); scale: 0.5 }", ".foo{transform:scale(1.5)}");
    minify_test(".foo { scale: 0.5; transform: scale(3); }", ".foo{transform:scale(3)}");

    prefix_test(r#"
      .foo {
        transform: scale(0.5);
      }
    "#, indoc! {r#"
      .foo {
        -webkit-transform: scale(.5);
        -moz-transform: scale(.5);
        transform: scale(.5);
      }
    "#}, Browsers {
      firefox: Some(6 << 16),
      safari: Some(6 << 16),
      ..Browsers::default()
    });
  }

  #[test]
  pub fn test_gradients() {
    minify_test(
      ".foo { background: linear-gradient(yellow, blue) }",
      ".foo{background:linear-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: linear-gradient(to bottom, yellow, blue); }",
      ".foo{background:linear-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: linear-gradient(180deg, yellow, blue); }",
      ".foo{background:linear-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: linear-gradient(0.5turn, yellow, blue); }",
      ".foo{background:linear-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: linear-gradient(yellow 10%, blue 20%) }",
      ".foo{background:linear-gradient(#ff0 10%,#00f 20%)}"
    );
    minify_test(
      ".foo { background: linear-gradient(to top, blue, yellow); }",
      ".foo{background:linear-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: linear-gradient(to top, blue 10%, yellow 20%); }",
      ".foo{background:linear-gradient(#ff0 80%,#00f 90%)}"
    );
    minify_test(
      ".foo { background: linear-gradient(to top, blue 10px, yellow 20px); }",
      ".foo{background:linear-gradient(0deg,#00f 10px,#ff0 20px)}"
    );
    minify_test(
      ".foo { background: linear-gradient(135deg, yellow, blue); }",
      ".foo{background:linear-gradient(135deg,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: linear-gradient(yellow, blue 20%, #0f0); }",
      ".foo{background:linear-gradient(#ff0,#00f 20%,#0f0)}"
    );
    minify_test(
      ".foo { background: linear-gradient(to top right, red, white, blue) }",
      ".foo{background:linear-gradient(to top right,red,#fff,#00f)}"
    );
    minify_test(
      ".foo { background: linear-gradient(yellow, blue calc(10% * 2), #0f0); }",
      ".foo{background:linear-gradient(#ff0,#00f 20%,#0f0)}"
    );
    minify_test(
      ".foo { background: linear-gradient(yellow, 20%, blue); }",
      ".foo{background:linear-gradient(#ff0,20%,#00f)}"
    );
    minify_test(
      ".foo { background: linear-gradient(yellow, 50%, blue); }",
      ".foo{background:linear-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: linear-gradient(yellow, 20px, blue); }",
      ".foo{background:linear-gradient(#ff0,20px,#00f)}"
    );
    minify_test(
      ".foo { background: linear-gradient(yellow, 50px, blue); }",
      ".foo{background:linear-gradient(#ff0,50px,#00f)}"
    );
    minify_test(
      ".foo { background: linear-gradient(yellow, 50px, blue); }",
      ".foo{background:linear-gradient(#ff0,50px,#00f)}"
    );
    minify_test(
      ".foo { background: linear-gradient(yellow, red 30% 40%, blue); }",
      ".foo{background:linear-gradient(#ff0,red 30% 40%,#00f)}"
    );
    minify_test(
      ".foo { background: linear-gradient(yellow, red 30%, red 40%, blue); }",
      ".foo{background:linear-gradient(#ff0,red 30% 40%,#00f)}"
    );
    minify_test(
      ".foo { background: -webkit-linear-gradient(yellow, blue) }",
      ".foo{background:-webkit-linear-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: -webkit-linear-gradient(bottom, yellow, blue); }",
      ".foo{background:-webkit-linear-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: -webkit-linear-gradient(top right, red, white, blue) }",
      ".foo{background:-webkit-linear-gradient(top right,red,#fff,#00f)}"
    );
    minify_test(
      ".foo { background: -moz-linear-gradient(yellow, blue) }",
      ".foo{background:-moz-linear-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: -moz-linear-gradient(bottom, yellow, blue); }",
      ".foo{background:-moz-linear-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: -moz-linear-gradient(top right, red, white, blue) }",
      ".foo{background:-moz-linear-gradient(top right,red,#fff,#00f)}"
    );
    minify_test(
      ".foo { background: -o-linear-gradient(yellow, blue) }",
      ".foo{background:-o-linear-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: -o-linear-gradient(bottom, yellow, blue); }",
      ".foo{background:-o-linear-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: -o-linear-gradient(top right, red, white, blue) }",
      ".foo{background:-o-linear-gradient(top right,red,#fff,#00f)}"
    );
    minify_test(
      ".foo { background: -webkit-gradient(linear, left top, left bottom, from(blue), to(yellow)) }",
      ".foo{background:-webkit-gradient(linear,0 0,0 100%,from(#00f),to(#ff0))}"
    );
    minify_test(
      ".foo { background: -webkit-gradient(linear, left top, left bottom, from(blue), color-stop(50%, red), to(yellow)) }",
      ".foo{background:-webkit-gradient(linear,0 0,0 100%,from(#00f),color-stop(.5,red),to(#ff0))}"
    );
    minify_test(
      ".foo { background: -webkit-gradient(linear, left top, left bottom, color-stop(0%, blue), color-stop(50%, red), color-stop(100%, yellow)) }",
      ".foo{background:-webkit-gradient(linear,0 0,0 100%,from(#00f),color-stop(.5,red),to(#ff0))}"
    );
    minify_test(
      ".foo { background: repeating-linear-gradient(yellow 10px, blue 50px) }",
      ".foo{background:repeating-linear-gradient(#ff0 10px,#00f 50px)}"
    );
    minify_test(
      ".foo { background: -webkit-repeating-linear-gradient(yellow 10px, blue 50px) }",
      ".foo{background:-webkit-repeating-linear-gradient(#ff0 10px,#00f 50px)}"
    );
    minify_test(
      ".foo { background: -moz-repeating-linear-gradient(yellow 10px, blue 50px) }",
      ".foo{background:-moz-repeating-linear-gradient(#ff0 10px,#00f 50px)}"
    );
    minify_test(
      ".foo { background: -o-repeating-linear-gradient(yellow 10px, blue 50px) }",
      ".foo{background:-o-repeating-linear-gradient(#ff0 10px,#00f 50px)}"
    );
    minify_test(
      ".foo { background: radial-gradient(yellow, blue) }",
      ".foo{background:radial-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: radial-gradient(at top left, yellow, blue) }",
      ".foo{background:radial-gradient(at left top,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: radial-gradient(5em circle at top left, yellow, blue) }",
      ".foo{background:radial-gradient(5em at left top,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: radial-gradient(circle at 100%, #333, #333 50%, #eee 75%, #333 75%) }",
      ".foo{background:radial-gradient(circle at 100%,#333,#333 50%,#eee 75%,#333 75%)}"
    );
    minify_test(
      ".foo { background: radial-gradient(farthest-corner circle at 100% 50%, #333, #333 50%, #eee 75%, #333 75%) }",
      ".foo{background:radial-gradient(circle at 100%,#333,#333 50%,#eee 75%,#333 75%)}"
    );
    minify_test(
      ".foo { background: radial-gradient(farthest-corner circle at 50% 50%, #333, #333 50%, #eee 75%, #333 75%) }",
      ".foo{background:radial-gradient(circle,#333,#333 50%,#eee 75%,#333 75%)}"
    );
    minify_test(
      ".foo { background: radial-gradient(ellipse at top, #e66465, transparent) }",
      ".foo{background:radial-gradient(at top,#e66465,#0000)}"
    );
    minify_test(
      ".foo { background: radial-gradient(20px, yellow, blue) }",
      ".foo{background:radial-gradient(20px,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: radial-gradient(circle 20px, yellow, blue) }",
      ".foo{background:radial-gradient(20px,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: radial-gradient(20px 40px, yellow, blue) }",
      ".foo{background:radial-gradient(20px 40px,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: radial-gradient(ellipse 20px 40px, yellow, blue) }",
      ".foo{background:radial-gradient(20px 40px,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: radial-gradient(ellipse calc(20px + 10px) 40px, yellow, blue) }",
      ".foo{background:radial-gradient(30px 40px,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: radial-gradient(circle farthest-side, yellow, blue) }",
      ".foo{background:radial-gradient(circle farthest-side,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: radial-gradient(farthest-side circle, yellow, blue) }",
      ".foo{background:radial-gradient(circle farthest-side,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: radial-gradient(ellipse farthest-side, yellow, blue) }",
      ".foo{background:radial-gradient(farthest-side,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: radial-gradient(farthest-side ellipse, yellow, blue) }",
      ".foo{background:radial-gradient(farthest-side,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: -webkit-radial-gradient(yellow, blue) }",
      ".foo{background:-webkit-radial-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: -moz-radial-gradient(yellow, blue) }",
      ".foo{background:-moz-radial-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: -o-radial-gradient(yellow, blue) }",
      ".foo{background:-o-radial-gradient(#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: repeating-radial-gradient(circle 20px, yellow, blue) }",
      ".foo{background:repeating-radial-gradient(20px,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: -webkit-repeating-radial-gradient(circle 20px, yellow, blue) }",
      ".foo{background:-webkit-repeating-radial-gradient(20px,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: -moz-repeating-radial-gradient(circle 20px, yellow, blue) }",
      ".foo{background:-moz-repeating-radial-gradient(20px,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: -o-repeating-radial-gradient(circle 20px, yellow, blue) }",
      ".foo{background:-o-repeating-radial-gradient(20px,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: -webkit-gradient(radial, center center, 0, center center, 100, from(blue), to(yellow)) }",
      ".foo{background:-webkit-gradient(radial,50% 50%,0,50% 50%,100,from(#00f),to(#ff0))}"
    );
    minify_test(
      ".foo { background: conic-gradient(#f06, gold) }",
      ".foo{background:conic-gradient(#f06,gold)}"
    );
    minify_test(
      ".foo { background: conic-gradient(at 50% 50%, #f06, gold) }",
      ".foo{background:conic-gradient(#f06,gold)}"
    );
    minify_test(
      ".foo { background: conic-gradient(from 0deg, #f06, gold) }",
      ".foo{background:conic-gradient(#f06,gold)}"
    );
    minify_test(
      ".foo { background: conic-gradient(from 0deg at center, #f06, gold) }",
      ".foo{background:conic-gradient(#f06,gold)}"
    );
    minify_test(
      ".foo { background: conic-gradient(white -50%, black 150%) }",
      ".foo{background:conic-gradient(#fff -50%,#000 150%)}"
    );
    minify_test(
      ".foo { background: conic-gradient(white -180deg, black 540deg) }",
      ".foo{background:conic-gradient(#fff -180deg,#000 540deg)}"
    );
    minify_test(
      ".foo { background: conic-gradient(from 45deg, white, black, white) }",
      ".foo{background:conic-gradient(from 45deg,#fff,#000,#fff)}"
    );
    minify_test(
      ".foo { background: repeating-conic-gradient(from 45deg, white, black, white) }",
      ".foo{background:repeating-conic-gradient(from 45deg,#fff,#000,#fff)}"
    );
    minify_test(
      ".foo { background: repeating-conic-gradient(black 0deg 25%, white 0deg 50%) }",
      ".foo{background:repeating-conic-gradient(#000 0deg 25%,#fff 0deg 50%)}"
    );

    test(
      r#"
        .foo {
          background: -webkit-gradient(linear, left top, left bottom, from(red), to(blue));
          background: -webkit-linear-gradient(red, blue);
          background: -moz-linear-gradient(red, blue);
          background: -o-linear-gradient(red, blue);
          background: linear-gradient(red, blue);
        }
      "#,
      indoc! {r#"
        .foo {
          background: -webkit-gradient(linear, left top, left bottom, from(red), to(#00f));
          background: -webkit-linear-gradient(red, #00f);
          background: -moz-linear-gradient(red, #00f);
          background: -o-linear-gradient(red, #00f);
          background: linear-gradient(red, #00f);
        }
      "#}
    );

    prefix_test(
      r#"
      .foo {
        background: -webkit-gradient(linear, left top, left bottom, from(red), to(blue));
        background: -webkit-linear-gradient(red, blue);
        background: -moz-linear-gradient(red, blue);
        background: -o-linear-gradient(red, blue);
        background: linear-gradient(red, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background: linear-gradient(red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(95 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background: -webkit-gradient(linear, left top, left bottom, from(red), to(blue));
        background: -webkit-linear-gradient(red, blue);
        background: -moz-linear-gradient(red, blue);
        background: -o-linear-gradient(red, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background: -webkit-gradient(linear, left top, left bottom, from(red), to(#00f));
        background: -webkit-linear-gradient(red, #00f);
        background: -moz-linear-gradient(red, #00f);
        background: -o-linear-gradient(red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(95 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background-image: linear-gradient(red, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background-image: -webkit-gradient(linear, 0 0, 0 100%, from(red), to(#00f));
        background-image: -webkit-linear-gradient(red, #00f);
        background-image: linear-gradient(red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(8 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background-image: linear-gradient(to right, red, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background-image: -webkit-gradient(linear, 0 0, 100% 0, from(red), to(#00f));
        background-image: -webkit-linear-gradient(right, red, #00f);
        background-image: linear-gradient(to right, red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(8 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background-image: linear-gradient(to top, red, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background-image: -webkit-gradient(linear, 0 100%, 0 0, from(red), to(#00f));
        background-image: -webkit-linear-gradient(top, red, #00f);
        background-image: linear-gradient(to top, red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(8 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background-image: linear-gradient(to left, red, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background-image: -webkit-gradient(linear, 100% 0, 0 0, from(red), to(#00f));
        background-image: -webkit-linear-gradient(left, red, #00f);
        background-image: linear-gradient(to left, red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(8 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background-image: linear-gradient(to left bottom, red, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background-image: -webkit-gradient(linear, 100% 0, 0 100%, from(red), to(#00f));
        background-image: -webkit-linear-gradient(bottom left, red, #00f);
        background-image: linear-gradient(to bottom left, red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(8 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background-image: linear-gradient(to top right, red, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background-image: -webkit-gradient(linear, 0 100%, 100% 0, from(red), to(#00f));
        background-image: -webkit-linear-gradient(top right, red, #00f);
        background-image: linear-gradient(to top right, red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(8 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background-image: linear-gradient(90deg, red, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background-image: -webkit-gradient(linear, 0 0, 100% 0, from(red), to(#00f));
        background-image: -webkit-linear-gradient(90deg, red, #00f);
        background-image: linear-gradient(90deg, red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(8 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background-image: linear-gradient(45deg, red, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background-image: -webkit-linear-gradient(45deg, red, #00f);
        background-image: linear-gradient(45deg, red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(8 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background-image: linear-gradient(red, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background-image: -webkit-linear-gradient(red, #00f);
        background-image: linear-gradient(red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(10 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background-image: radial-gradient(20px, red, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background-image: -webkit-gradient(radial, center center, 0, center center, 20, from(red), to(#00f));
        background-image: -webkit-radial-gradient(20px, red, #00f);
        background-image: radial-gradient(20px, red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(8 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background-image: radial-gradient(20px at top left, red, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background-image: -webkit-gradient(radial, left top, 0, left top, 20, from(red), to(#00f));
        background-image: -webkit-radial-gradient(20px at left top, red, #00f);
        background-image: radial-gradient(20px at left top, red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(8 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background-image: radial-gradient(red, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background-image: -webkit-radial-gradient(red, #00f);
        background-image: radial-gradient(red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(8 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background-image: -webkit-gradient(radial, left top, 0, left top, 20, from(red), to(#00f));
        background-image: -webkit-radial-gradient(20px at left top, red, #00f);
        background-image: radial-gradient(20px at left top, red, #00f);
      }
      "#,
      indoc! {r#"
      .foo {
        background-image: radial-gradient(20px at left top, red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(30 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background: -webkit-gradient(radial, left top, 0, left top, 20, from(red), to(#00f));
        background: -webkit-radial-gradient(20px at left top, red, #00f);
        background: radial-gradient(20px at left top, red, #00f);
      }
      "#,
      indoc! {r#"
      .foo {
        background: radial-gradient(20px at left top, red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(30 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background: radial-gradient(red, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background: -webkit-radial-gradient(red, #00f);
        background: radial-gradient(red, #00f);
      }
      "#},
      Browsers {
        chrome: Some(8 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        background: radial-gradient(red, blue), linear-gradient(yellow, red), url(bg.jpg);
      }
      "#,
      indoc! {r#"
      .foo {
        background: -webkit-gradient(linear, 0 0, 0 100%, from(#ff0), to(red)), url(bg.jpg);
        background: -webkit-radial-gradient(red, #00f), -webkit-linear-gradient(#ff0, red), url(bg.jpg);
        background: -moz-radial-gradient(red, #00f), -moz-linear-gradient(#ff0, red), url(bg.jpg);
        background: -o-radial-gradient(red, #00f), -o-linear-gradient(#ff0, red), url(bg.jpg);
        background: radial-gradient(red, #00f), linear-gradient(#ff0, red), url(bg.jpg);
      }
      "#},
      Browsers {
        chrome: Some(8 << 16),
        firefox: Some(4 << 16),
        opera: Some(11 << 16 | 5 << 8),
        ..Browsers::default()
      }
    );
  }

  #[test]
  fn test_font_face() {
    minify_test(r#"@font-face {
      src: url("test.woff");
      font-family: "Helvetica";
      font-weight: bold;
      font-style: italic;
    }"#, "@font-face{src:url(test.woff);font-family:Helvetica;font-weight:700;font-style:italic}");
    minify_test("@font-face {src: url(test.woff);}", "@font-face{src:url(test.woff)}");
    minify_test("@font-face {src: local(\"Test\");}", "@font-face{src:local(Test)}");
    minify_test("@font-face {src: local(\"Foo Bar\");}", "@font-face{src:local(Foo Bar)}");
    minify_test("@font-face {src: local(Test);}", "@font-face{src:local(Test)}");
    minify_test("@font-face {src: local(Foo Bar);}", "@font-face{src:local(Foo Bar)}");
    minify_test("@font-face {src: url(\"test.woff\") format(woff);}", "@font-face{src:url(test.woff)format(woff)}");
    minify_test("@font-face {src: url(\"test.woff\") format(woff), url(test.ttf) format(truetype);}", "@font-face{src:url(test.woff)format(woff),url(test.ttf)format(truetype)}");
    minify_test("@font-face {src: url(\"test.woff\") format(woff supports features(opentype));}", "@font-face{src:url(test.woff)format(woff supports features(opentype))}");
    minify_test("@font-face {src: url(\"test.woff\") format(woff supports color(COLRv1));}", "@font-face{src:url(test.woff)format(woff supports color(colrv1))}");
    minify_test("@font-face {src: url(\"test.woff\") format(woff supports variations);}", "@font-face{src:url(test.woff)format(woff supports variations)}");
    minify_test("@font-face {src: url(\"test.woff\") format(woff supports palettes);}", "@font-face{src:url(test.woff)format(woff supports palettes)}");
    minify_test("@font-face {src: url(\"test.woff\") format(woff supports features(opentype) color(sbix));}", "@font-face{src:url(test.woff)format(woff supports features(opentype) color(sbix))}");
  }

  #[test]
  fn test_page_rule() {
    minify_test("@page {margin: 0.5cm}", "@page{margin:.5cm}");
    minify_test("@page :left {margin: 0.5cm}", "@page:left{margin:.5cm}");
    minify_test("@page :right {margin: 0.5cm}", "@page:right{margin:.5cm}");
    minify_test("@page LandscapeTable {margin: 0.5cm}", "@page LandscapeTable{margin:.5cm}");
    minify_test("@page CompanyLetterHead:first {margin: 0.5cm}", "@page CompanyLetterHead:first{margin:.5cm}");
    minify_test("@page:first {margin: 0.5cm}", "@page:first{margin:.5cm}");
    minify_test("@page :blank:first {margin: 0.5cm}", "@page:blank:first{margin:.5cm}");
    minify_test("@page toc, index {margin: 0.5cm}", "@page toc,index{margin:.5cm}");
  }

  #[test]
  fn test_prefixes() {
    prefix_test(
      r#"
      .foo {
        -webkit-transition: opacity 200ms;
        -moz-transition: opacity 200ms;
        transition: opacity 200ms;
      }
      "#,
      indoc! {r#"
      .foo {
        transition: opacity .2s;
      }
      "#},
      Browsers {
        chrome: Some(95 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      ".foo{transition:opacity 200ms}",
      indoc! {r#"
      .foo {
        -webkit-transition: opacity .2s;
        -moz-transition: opacity .2s;
        transition: opacity .2s;
      }
      "#},
      Browsers {
        safari: Some(5 << 16),
        firefox: Some(14 << 16),
        ..Browsers::default()
      }
    );
  }

  #[test]
  fn test_display() {
    minify_test(".foo { display: block }", ".foo{display:block}");
    minify_test(".foo { display: block flow }", ".foo{display:block}");
    minify_test(".foo { display: flow-root }", ".foo{display:flow-root}");
    minify_test(".foo { display: block flow-root }", ".foo{display:flow-root}");
    minify_test(".foo { display: inline }", ".foo{display:inline}");
    minify_test(".foo { display: inline flow }", ".foo{display:inline}");
    minify_test(".foo { display: inline-block }", ".foo{display:inline-block}");
    minify_test(".foo { display: inline flow-root }", ".foo{display:inline-block}");
    minify_test(".foo { display: run-in }", ".foo{display:run-in}");
    minify_test(".foo { display: run-in flow }", ".foo{display:run-in}");
    minify_test(".foo { display: list-item }", ".foo{display:list-item}");
    minify_test(".foo { display: block flow list-item }", ".foo{display:list-item}");
    minify_test(".foo { display: inline list-item }", ".foo{display:inline list-item}");
    minify_test(".foo { display: inline flow list-item }", ".foo{display:inline list-item}");
    minify_test(".foo { display: flex }", ".foo{display:flex}");
    minify_test(".foo { display: block flex }", ".foo{display:flex}");
    minify_test(".foo { display: inline-flex }", ".foo{display:inline-flex}");
    minify_test(".foo { display: inline flex }", ".foo{display:inline-flex}");
    minify_test(".foo { display: grid }", ".foo{display:grid}");
    minify_test(".foo { display: block grid }", ".foo{display:grid}");
    minify_test(".foo { display: inline-grid }", ".foo{display:inline-grid}");
    minify_test(".foo { display: inline grid }", ".foo{display:inline-grid}");
    minify_test(".foo { display: ruby }", ".foo{display:ruby}");
    minify_test(".foo { display: inline ruby }", ".foo{display:ruby}");
    minify_test(".foo { display: block ruby }", ".foo{display:block ruby}");
    minify_test(".foo { display: table }", ".foo{display:table}");
    minify_test(".foo { display: block table }", ".foo{display:table}");
    minify_test(".foo { display: inline-table }", ".foo{display:inline-table}");
    minify_test(".foo { display: inline table }", ".foo{display:inline-table}");
    minify_test(".foo { display: table-row-group }", ".foo{display:table-row-group}");
    minify_test(".foo { display: contents }", ".foo{display:contents}");
    minify_test(".foo { display: none }", ".foo{display:none}");
    minify_test(".foo { display: -webkit-flex }", ".foo{display:-webkit-flex}");
    minify_test(".foo { display: -ms-flexbox }", ".foo{display:-ms-flexbox}");
    minify_test(".foo { display: -webkit-box }", ".foo{display:-webkit-box}");
    minify_test(".foo { display: -moz-box }", ".foo{display:-moz-box}");
    minify_test(
      ".foo { display: -webkit-flex; display: -moz-box; display: flex }",
      ".foo{display:-webkit-flex;display:-moz-box;display:flex}"
    );
    minify_test(
      ".foo { display: -webkit-flex; display: flex; display: -moz-box }",
      ".foo{display:-webkit-flex;display:flex;display:-moz-box}"
    );
    minify_test(
      ".foo { display: flex; display: grid }",
      ".foo{display:grid}"
    );
    minify_test(
      ".foo { display: -webkit-inline-flex; display: -moz-inline-box; display: inline-flex }",
      ".foo{display:-webkit-inline-flex;display:-moz-inline-box;display:inline-flex}"
    );
    prefix_test(
      ".foo{ display: flex }",
      indoc! {r#"
      .foo {
        display: -webkit-box;
        display: -moz-box;
        display: -webkit-flex;
        display: -ms-flexbox;
        display: flex;
      }
      "#},
      Browsers {
        safari: Some(4 << 16),
        firefox: Some(14 << 16),
        ie: Some(10 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      ".foo{ display: flex; display: -webkit-box; }",
      indoc! {r#"
      .foo {
        display: -webkit-box;
      }
      "#},
      Browsers {
        safari: Some(4 << 16),
        firefox: Some(14 << 16),
        ie: Some(10 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      ".foo{ display: -webkit-box; display: flex; }",
      indoc! {r#"
      .foo {
        display: -webkit-box;
        display: -moz-box;
        display: -webkit-flex;
        display: -ms-flexbox;
        display: flex;
      }
      "#},
      Browsers {
        safari: Some(4 << 16),
        firefox: Some(14 << 16),
        ie: Some(10 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(r#"
      .foo {
        display: -webkit-box;
        display: -moz-box;
        display: -webkit-flex;
        display: -ms-flexbox;
        display: flex;
      }
      "#,
      indoc! {r#"
      .foo {
        display: flex;
      }
      "#},
      Browsers {
        safari: Some(14 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(r#"
      .foo {
        display: -webkit-box;
        display: flex;
        display: -moz-box;
        display: -webkit-flex;
        display: -ms-flexbox;
      }
      "#,
      indoc! {r#"
      .foo {
        display: -moz-box;
        display: -webkit-flex;
        display: -ms-flexbox;
      }
      "#},
      Browsers {
        safari: Some(14 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      ".foo{ display: inline-flex }",
      indoc! {r#"
      .foo {
        display: -webkit-inline-box;
        display: -moz-inline-box;
        display: -webkit-inline-flex;
        display: -ms-inline-flexbox;
        display: inline-flex;
      }
      "#},
      Browsers {
        safari: Some(4 << 16),
        firefox: Some(14 << 16),
        ie: Some(10 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(r#"
      .foo {
        display: -webkit-inline-box;
        display: -moz-inline-box;
        display: -webkit-inline-flex;
        display: -ms-inline-flexbox;
        display: inline-flex;
      }
      "#,
      indoc! {r#"
      .foo {
        display: inline-flex;
      }
      "#},
      Browsers {
        safari: Some(14 << 16),
        ..Browsers::default()
      }
    );
  }

  #[test]
  fn test_visibility() {
    minify_test(".foo { visibility: visible }", ".foo{visibility:visible}");
    minify_test(".foo { visibility: hidden }", ".foo{visibility:hidden}");
    minify_test(".foo { visibility: collapse }", ".foo{visibility:collapse}");
    minify_test(".foo { visibility: Visible }", ".foo{visibility:visible}");
  }

  #[test]
  fn test_text_transform() {
    minify_test(".foo { text-transform: uppercase }", ".foo{text-transform:uppercase}");
    minify_test(".foo { text-transform: lowercase }", ".foo{text-transform:lowercase}");
    minify_test(".foo { text-transform: capitalize }", ".foo{text-transform:capitalize}");
    minify_test(".foo { text-transform: none }", ".foo{text-transform:none}");
    minify_test(".foo { text-transform: full-width }", ".foo{text-transform:full-width}");
    minify_test(".foo { text-transform: full-size-kana }", ".foo{text-transform:full-size-kana}");
    minify_test(".foo { text-transform: uppercase full-width }", ".foo{text-transform:uppercase full-width}");
    minify_test(".foo { text-transform: full-width uppercase }", ".foo{text-transform:uppercase full-width}");
    minify_test(".foo { text-transform: uppercase full-width full-size-kana }", ".foo{text-transform:uppercase full-width full-size-kana}");
    minify_test(".foo { text-transform: full-width uppercase full-size-kana }", ".foo{text-transform:uppercase full-width full-size-kana}");
  }

  #[test]
  fn test_whitespace() {
    minify_test(".foo { white-space: normal }", ".foo{white-space:normal}");
    minify_test(".foo { white-space: pre }", ".foo{white-space:pre}");
    minify_test(".foo { white-space: nowrap }", ".foo{white-space:nowrap}");
    minify_test(".foo { white-space: pre-wrap }", ".foo{white-space:pre-wrap}");
    minify_test(".foo { white-space: break-spaces }", ".foo{white-space:break-spaces}");
    minify_test(".foo { white-space: pre-line }", ".foo{white-space:pre-line}");
    minify_test(".foo { white-space: NoWrAp }", ".foo{white-space:nowrap}");
  }

  #[test]
  fn test_tab_size() {
    minify_test(".foo { tab-size: 8 }", ".foo{tab-size:8}");
    minify_test(".foo { tab-size: 4px }", ".foo{tab-size:4px}");
    minify_test(".foo { -moz-tab-size: 4px }", ".foo{-moz-tab-size:4px}");
    minify_test(".foo { -o-tab-size: 4px }", ".foo{-o-tab-size:4px}");
    prefix_test(
      ".foo{ tab-size: 4 }",
      indoc! {r#"
      .foo {
        -moz-tab-size: 4;
        -o-tab-size: 4;
        tab-size: 4;
      }
      "#},
      Browsers {
        safari: Some(8 << 16),
        firefox: Some(50 << 16),
        opera: Some(12 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        -moz-tab-size: 4;
        -o-tab-size: 4;
        tab-size: 4;
      }
      "#,
      indoc! {r#"
      .foo {
        tab-size: 4;
      }
      "#},
      Browsers {
        safari: Some(8 << 16),
        firefox: Some(94 << 16),
        opera: Some(30 << 16),
        ..Browsers::default()
      }
    );
  }

  #[test]
  fn test_word_break() {
    minify_test(".foo { word-break: normal }", ".foo{word-break:normal}");
    minify_test(".foo { word-break: keep-all }", ".foo{word-break:keep-all}");
    minify_test(".foo { word-break: break-all }", ".foo{word-break:break-all}");
    minify_test(".foo { word-break: break-word }", ".foo{word-break:break-word}");
  }

  #[test]
  fn test_line_break() {
    minify_test(".foo { line-break: auto }", ".foo{line-break:auto}");
    minify_test(".foo { line-break: Loose }", ".foo{line-break:loose}");
    minify_test(".foo { line-break: anywhere }", ".foo{line-break:anywhere}");
  }

  #[test]
  fn test_wrap() {
    minify_test(".foo { overflow-wrap: nOrmal }", ".foo{overflow-wrap:normal}");
    minify_test(".foo { overflow-wrap: break-Word }", ".foo{overflow-wrap:break-word}");
    minify_test(".foo { overflow-wrap: Anywhere }", ".foo{overflow-wrap:anywhere}");
    minify_test(".foo { word-wrap: Normal }", ".foo{word-wrap:normal}");
    minify_test(".foo { word-wrap: Break-wOrd }", ".foo{word-wrap:break-word}");
    minify_test(".foo { word-wrap: Anywhere }", ".foo{word-wrap:anywhere}");
  }

  #[test]
  fn test_hyphens() {
    minify_test(".foo { hyphens: manual }", ".foo{hyphens:manual}");
    minify_test(".foo { hyphens: auto }", ".foo{hyphens:auto}");
    minify_test(".foo { hyphens: none }", ".foo{hyphens:none}");
    minify_test(".foo { -webkit-hyphens: manual }", ".foo{-webkit-hyphens:manual}");
    minify_test(".foo { -moz-hyphens: manual }", ".foo{-moz-hyphens:manual}");
    minify_test(".foo { -ms-hyphens: manual }", ".foo{-ms-hyphens:manual}");
    prefix_test(
      ".foo{ hyphens: manual }",
      indoc! {r#"
      .foo {
        -webkit-hyphens: manual;
        -moz-hyphens: manual;
        -ms-hyphens: manual;
        hyphens: manual;
      }
      "#},
      Browsers {
        safari: Some(14 << 16),
        firefox: Some(40 << 16),
        ie: Some(10 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        -webkit-hyphens: manual;
        -moz-hyphens: manual;
        -ms-hyphens: manual;
        hyphens: manual;
      }
      "#,
      indoc! {r#"
      .foo {
        -webkit-hyphens: manual;
        hyphens: manual;
      }
      "#},
      Browsers {
        safari: Some(14 << 16),
        chrome: Some(88 << 16),
        firefox: Some(88 << 16),
        edge: Some(79 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        -webkit-hyphens: manual;
        -moz-hyphens: manual;
        -ms-hyphens: manual;
        hyphens: manual;
      }
      "#,
      indoc! {r#"
      .foo {
        hyphens: manual;
      }
      "#},
      Browsers {
        chrome: Some(88 << 16),
        firefox: Some(88 << 16),
        edge: Some(79 << 16),
        ..Browsers::default()
      }
    );
  }

  #[test]
  fn test_text_align() {
    minify_test(".foo { text-align: left }", ".foo{text-align:left}");
    minify_test(".foo { text-align: Left }", ".foo{text-align:left}");
    minify_test(".foo { text-align: END }", ".foo{text-align:end}");
    minify_test(".foo { text-align: left }", ".foo{text-align:left}");
  }

  #[test]
  fn test_text_align_last() {
    minify_test(".foo { text-align-last: left }", ".foo{text-align-last:left}");
    minify_test(".foo { text-align-last: justify }", ".foo{text-align-last:justify}");
    prefix_test(
      ".foo{ text-align-last: left }",
      indoc! {r#"
      .foo {
        -moz-text-align-last: left;
        text-align-last: left;
      }
      "#},
      Browsers {
        firefox: Some(40 << 16),
        ..Browsers::default()
      }
    );
    prefix_test(
      r#"
      .foo {
        -moz-text-align-last: left;
        text-align-last: left;
      }
      "#,
      indoc! {r#"
      .foo {
        text-align-last: left;
      }
      "#},
      Browsers {
        firefox: Some(88 << 16),
        ..Browsers::default()
      }
    );
  }

  #[test]
  fn test_text_justify() {
    minify_test(".foo { text-justify: auto }", ".foo{text-justify:auto}");
    minify_test(".foo { text-justify: inter-word }", ".foo{text-justify:inter-word}");
  }

  #[test]
  fn test_word_spacing() {
    minify_test(".foo { word-spacing: normal }", ".foo{word-spacing:normal}");
    minify_test(".foo { word-spacing: 3px }", ".foo{word-spacing:3px}");
  }

  #[test]
  fn test_letter_spacing() {
    minify_test(".foo { letter-spacing: normal }", ".foo{letter-spacing:normal}");
    minify_test(".foo { letter-spacing: 3px }", ".foo{letter-spacing:3px}");
  }

  #[test]
  fn test_text_indent() {
    minify_test(".foo { text-indent: 20px }", ".foo{text-indent:20px}");
    minify_test(".foo { text-indent: 10% }", ".foo{text-indent:10%}");
    minify_test(".foo { text-indent: 3em hanging }", ".foo{text-indent:3em hanging}");
    minify_test(".foo { text-indent: 3em each-line }", ".foo{text-indent:3em each-line}");
    minify_test(".foo { text-indent: 3em hanging each-line }", ".foo{text-indent:3em hanging each-line}");
    minify_test(".foo { text-indent: 3em each-line hanging }", ".foo{text-indent:3em hanging each-line}");
    minify_test(".foo { text-indent: each-line 3em hanging }", ".foo{text-indent:3em hanging each-line}");
    minify_test(".foo { text-indent: each-line hanging 3em }", ".foo{text-indent:3em hanging each-line}");
  }

  #[test]
  fn test_text_decoration() {
    minify_test(".foo { text-decoration-line: none }", ".foo{text-decoration-line:none}");
    minify_test(".foo { text-decoration-line: underline }", ".foo{text-decoration-line:underline}");
    minify_test(".foo { text-decoration-line: overline }", ".foo{text-decoration-line:overline}");
    minify_test(".foo { text-decoration-line: line-through }", ".foo{text-decoration-line:line-through}");
    minify_test(".foo { text-decoration-line: blink }", ".foo{text-decoration-line:blink}");
    minify_test(".foo { text-decoration-line: underline overline }", ".foo{text-decoration-line:underline overline}");
    minify_test(".foo { text-decoration-line: overline underline }", ".foo{text-decoration-line:underline overline}");
    minify_test(".foo { text-decoration-line: overline line-through underline }", ".foo{text-decoration-line:underline overline line-through}");
    minify_test(".foo { text-decoration-line: spelling-error }", ".foo{text-decoration-line:spelling-error}");
    minify_test(".foo { text-decoration-line: grammar-error }", ".foo{text-decoration-line:grammar-error}");
    minify_test(".foo { -webkit-text-decoration-line: overline underline }", ".foo{-webkit-text-decoration-line:underline overline}");
    minify_test(".foo { -moz-text-decoration-line: overline underline }", ".foo{-moz-text-decoration-line:underline overline}");

    minify_test(".foo { text-decoration-style: solid }", ".foo{text-decoration-style:solid}");
    minify_test(".foo { text-decoration-style: dotted }", ".foo{text-decoration-style:dotted}");
    minify_test(".foo { -webkit-text-decoration-style: solid }", ".foo{-webkit-text-decoration-style:solid}");

    minify_test(".foo { text-decoration-color: yellow }", ".foo{text-decoration-color:#ff0}");
    minify_test(".foo { -webkit-text-decoration-color: yellow }", ".foo{-webkit-text-decoration-color:#ff0}");

    minify_test(".foo { text-decoration: none }", ".foo{text-decoration:none}");
    minify_test(".foo { text-decoration: underline dotted }", ".foo{text-decoration:underline dotted}");
    minify_test(".foo { text-decoration: underline dotted yellow }", ".foo{text-decoration:underline dotted #ff0}");
    minify_test(".foo { text-decoration: yellow dotted underline }", ".foo{text-decoration:underline dotted #ff0}");
    minify_test(".foo { text-decoration: underline overline dotted yellow }", ".foo{text-decoration:underline overline dotted #ff0}");
    minify_test(".foo { -webkit-text-decoration: yellow dotted underline }", ".foo{-webkit-text-decoration:underline dotted #ff0}");
    minify_test(".foo { -moz-text-decoration: yellow dotted underline }", ".foo{-moz-text-decoration:underline dotted #ff0}");

    test(r#"
      .foo {
        text-decoration-line: underline;
        text-decoration-style: dotted;
        text-decoration-color: yellow;
        text-decoration-thickness: 2px;
      }
    "#, indoc! {r#"
      .foo {
        text-decoration: underline 2px dotted #ff0;
      }
    "#});

    test(r#"
      .foo {
        text-decoration: underline;
        text-decoration-style: dotted;
      }
    "#, indoc! {r#"
      .foo {
        text-decoration: underline dotted;
      }
    "#});

    test(r#"
      .foo {
        -webkit-text-decoration: underline;
        -webkit-text-decoration-style: dotted;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-text-decoration: underline dotted;
      }
    "#});

    prefix_test(r#"
      .foo {
        text-decoration: underline dotted;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-text-decoration: underline dotted;
        -moz-text-decoration: underline dotted;
        text-decoration: underline dotted;
      }
    "#},
    Browsers {
      safari: Some(8 << 16),
      firefox: Some(30 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        text-decoration-line: underline;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-text-decoration-line: underline;
        -moz-text-decoration-line: underline;
        text-decoration-line: underline;
      }
    "#},
    Browsers {
      safari: Some(8 << 16),
      firefox: Some(30 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        text-decoration-style: dotted;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-text-decoration-style: dotted;
        -moz-text-decoration-style: dotted;
        text-decoration-style: dotted;
      }
    "#},
    Browsers {
      safari: Some(8 << 16),
      firefox: Some(30 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        text-decoration-color: yellow;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-text-decoration-color: #ff0;
        -moz-text-decoration-color: #ff0;
        text-decoration-color: #ff0;
      }
    "#},
    Browsers {
      safari: Some(8 << 16),
      firefox: Some(30 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        text-decoration: underline;
      }
    "#, indoc! {r#"
      .foo {
        text-decoration: underline;
      }
    "#},
    Browsers {
      safari: Some(8 << 16),
      firefox: Some(30 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        -webkit-text-decoration: underline dotted;
        -moz-text-decoration: underline dotted;
        text-decoration: underline dotted;
      }
    "#, indoc! {r#"
      .foo {
        text-decoration: underline dotted;
      }
    "#},
    Browsers {
      safari: Some(14 << 16),
      firefox: Some(45 << 16),
      ..Browsers::default()
    });

    minify_test(".foo { text-decoration-skip-ink: all }", ".foo{text-decoration-skip-ink:all}");
    minify_test(".foo { -webkit-text-decoration-skip-ink: all }", ".foo{-webkit-text-decoration-skip-ink:all}");
  }

  #[test]
  fn test_text_emphasis() {
    minify_test(".foo { text-emphasis-style: none }", ".foo{text-emphasis-style:none}");
    minify_test(".foo { text-emphasis-style: filled }", ".foo{text-emphasis-style:filled}");
    minify_test(".foo { text-emphasis-style: open }", ".foo{text-emphasis-style:open}");
    minify_test(".foo { text-emphasis-style: dot }", ".foo{text-emphasis-style:dot}");
    minify_test(".foo { text-emphasis-style: filled dot }", ".foo{text-emphasis-style:dot}");
    minify_test(".foo { text-emphasis-style: dot filled }", ".foo{text-emphasis-style:dot}");
    minify_test(".foo { text-emphasis-style: open dot }", ".foo{text-emphasis-style:open dot}");
    minify_test(".foo { text-emphasis-style: dot open }", ".foo{text-emphasis-style:open dot}");
    minify_test(".foo { text-emphasis-style: \"x\" }", ".foo{text-emphasis-style:\"x\"}");
    
    minify_test(".foo { text-emphasis-color: yellow }", ".foo{text-emphasis-color:#ff0}");

    minify_test(".foo { text-emphasis: none }", ".foo{text-emphasis:none}");
    minify_test(".foo { text-emphasis: filled }", ".foo{text-emphasis:filled}");
    minify_test(".foo { text-emphasis: filled yellow }", ".foo{text-emphasis:filled #ff0}");
    minify_test(".foo { text-emphasis: dot filled yellow }", ".foo{text-emphasis:dot #ff0}");

    test(r#"
      .foo {
        text-emphasis-style: filled;
        text-emphasis-color: yellow;
      }
    "#, indoc! {r#"
      .foo {
        text-emphasis: filled #ff0;
      }
    "#});

    prefix_test(r#"
      .foo {
        text-emphasis-style: filled;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-text-emphasis-style: filled;
        text-emphasis-style: filled;
      }
    "#},
    Browsers {
      safari: Some(10 << 16),
      chrome: Some(30 << 16),
      firefox: Some(45 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        -webkit-text-emphasis-style: filled;
        text-emphasis-style: filled;
      }
    "#, indoc! {r#"
      .foo {
        text-emphasis-style: filled;
      }
    "#},
    Browsers {
      safari: Some(10 << 16),
      firefox: Some(45 << 16),
      ..Browsers::default()
    });

    minify_test(".foo { text-emphasis-position: over }", ".foo{text-emphasis-position:over}");
    minify_test(".foo { text-emphasis-position: under }", ".foo{text-emphasis-position:under}");
    minify_test(".foo { text-emphasis-position: over right }", ".foo{text-emphasis-position:over}");
    minify_test(".foo { text-emphasis-position: over left }", ".foo{text-emphasis-position:over left}");

    prefix_test(r#"
      .foo {
        text-emphasis-position: over;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-text-emphasis-position: over;
        text-emphasis-position: over;
      }
    "#},
    Browsers {
      safari: Some(10 << 16),
      chrome: Some(30 << 16),
      firefox: Some(45 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        text-emphasis-position: over left;
      }
    "#, indoc! {r#"
      .foo {
        text-emphasis-position: over left;
      }
    "#},
    Browsers {
      safari: Some(10 << 16),
      chrome: Some(30 << 16),
      firefox: Some(45 << 16),
      ..Browsers::default()
    });
  }

  #[test]
  fn test_text_shadow() {
    minify_test(".foo { text-shadow: 1px 1px 2px yellow; }", ".foo{text-shadow:1px 1px 2px #ff0}");
    minify_test(".foo { text-shadow: 1px 1px 2px 3px yellow; }", ".foo{text-shadow:1px 1px 2px 3px #ff0}");
    minify_test(".foo { text-shadow: 1px 1px 0 yellow; }", ".foo{text-shadow:1px 1px #ff0}");
    minify_test(".foo { text-shadow: 1px 1px yellow; }", ".foo{text-shadow:1px 1px #ff0}");
    minify_test(".foo { text-shadow: 1px 1px yellow, 2px 3px red; }", ".foo{text-shadow:1px 1px #ff0,2px 3px red}");
  }

  #[test]
  fn test_position() {
    test(r#"
      .foo {
        position: relative;
        position: absolute;
      }
    "#, indoc! {r#"
      .foo {
        position: absolute;
      }
    "#});

    test(r#"
      .foo {
        position: -webkit-sticky;
        position: sticky;
      }
    "#, indoc! {r#"
      .foo {
        position: -webkit-sticky;
        position: sticky;
      }
    "#});

    prefix_test(r#"
      .foo {
        position: sticky;
      }
    "#, indoc! {r#"
      .foo {
        position: -webkit-sticky;
        position: sticky;
      }
    "#},
    Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        position: -webkit-sticky;
        position: sticky;
      }
    "#, indoc! {r#"
      .foo {
        position: sticky;
      }
    "#},
    Browsers {
      safari: Some(13 << 16),
      ..Browsers::default()
    });

    test(r#"
      .foo {
        top: 0;
        left: 0;
        bottom: 0;
        right: 0;
      }
    "#, indoc! {r#"
      .foo {
        inset: 0;
      }
    "#});

    test(r#"
      .foo {
        top: 2px;
        left: 4px;
        bottom: 2px;
        right: 4px;
      }
    "#, indoc! {r#"
      .foo {
        inset: 2px 4px;
      }
    "#});

    test(r#"
      .foo {
        top: 1px;
        left: 2px;
        bottom: 3px;
        right: 4px;
      }
    "#, indoc! {r#"
      .foo {
        inset: 1px 4px 3px 2px;
      }
    "#});

    test(r#"
      .foo {
        inset-block-start: 2px;
        inset-block-end: 2px;
        inset-inline-start: 4px;
        inset-inline-end: 4px;
      }
    "#, indoc! {r#"
      .foo {
        inset-block: 2px;
        inset-inline: 4px;
      }
    "#});

    test(r#"
      .foo {
        inset-block-start: 2px;
        inset-block-end: 3px;
        inset-inline-start: 4px;
        inset-inline-end: 5px;
      }
    "#, indoc! {r#"
      .foo {
        inset-block: 2px 3px;
        inset-inline: 4px 5px;
      }
    "#});

    test(r#"
      .foo {
        inset-block-start: 2px;
        inset-block-end: 3px;
        inset: 4px;
        inset-inline-start: 4px;
        inset-inline-end: 5px;
      }
    "#, indoc! {r#"
      .foo {
        inset: 4px;
        inset-inline: 4px 5px;
      }
    "#});
  }

  #[test]
  fn test_overflow() {
    minify_test(".foo { overflow: hidden }", ".foo{overflow:hidden}");
    minify_test(".foo { overflow: hidden hidden }", ".foo{overflow:hidden}");
    minify_test(".foo { overflow: hidden auto }", ".foo{overflow:hidden auto}");

    test(r#"
      .foo {
        overflow-x: hidden;
        overflow-y: auto;
      }
    "#, indoc! {r#"
      .foo {
        overflow: hidden auto;
      }
    "#});

    test(r#"
      .foo {
        overflow: hidden;
        overflow-y: auto;
      }
    "#, indoc! {r#"
      .foo {
        overflow: hidden auto;
      }
    "#});

    minify_test(".foo { text-overflow: ellipsis }", ".foo{text-overflow:ellipsis}");
    prefix_test(r#"
      .foo {
        text-overflow: ellipsis;
      }
    "#, indoc! {r#"
      .foo {
        -o-text-overflow: ellipsis;
        text-overflow: ellipsis;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      opera: Some(10 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        -o-text-overflow: ellipsis;
        text-overflow: ellipsis;
      }
    "#, indoc! {r#"
      .foo {
        text-overflow: ellipsis;
      }
    "#},
    Browsers {
      safari: Some(4 << 16),
      opera: Some(14 << 16),
      ..Browsers::default()
    });
  }

  #[test]
  fn test_ui() {
    minify_test(".foo { resize: both }", ".foo{resize:both}");
    minify_test(".foo { resize: Horizontal }", ".foo{resize:horizontal}");
    minify_test(".foo { cursor: ew-resize }", ".foo{cursor:ew-resize}");
    minify_test(".foo { cursor: url(\"test.cur\"), ew-resize }", ".foo{cursor:url(test.cur),ew-resize}");
    minify_test(".foo { cursor: url(\"test.cur\"), url(\"foo.cur\"), ew-resize }", ".foo{cursor:url(test.cur),url(foo.cur),ew-resize}");
    minify_test(".foo { caret-color: auto }", ".foo{caret-color:auto}");
    minify_test(".foo { caret-color: yellow }", ".foo{caret-color:#ff0}");
    minify_test(".foo { caret-shape: block }", ".foo{caret-shape:block}");
    minify_test(".foo { caret: yellow block }", ".foo{caret:#ff0 block}");
    minify_test(".foo { caret: block yellow }", ".foo{caret:#ff0 block}");
    minify_test(".foo { caret: block }", ".foo{caret:block}");
    minify_test(".foo { caret: yellow }", ".foo{caret:#ff0}");
    minify_test(".foo { caret: auto auto }", ".foo{caret:auto}");
    minify_test(".foo { caret: auto }", ".foo{caret:auto}");
    minify_test(".foo { caret: yellow auto }", ".foo{caret:#ff0}");
    minify_test(".foo { caret: auto block }", ".foo{caret:block}");
    minify_test(".foo { user-select: none }", ".foo{user-select:none}");
    minify_test(".foo { -webkit-user-select: none }", ".foo{-webkit-user-select:none}");
    minify_test(".foo { accent-color: auto }", ".foo{accent-color:auto}");
    minify_test(".foo { accent-color: yellow }", ".foo{accent-color:#ff0}");
    minify_test(".foo { appearance: None }", ".foo{appearance:none}");
    minify_test(".foo { -webkit-appearance: textfield }", ".foo{-webkit-appearance:textfield}");

    prefix_test(r#"
      .foo {
        user-select: none;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-user-select: none;
        -moz-user-select: none;
        -ms-user-select: none;
        user-select: none;
      }
    "#},
    Browsers {
      safari: Some(8 << 16),
      opera: Some(5 << 16),
      firefox: Some(10 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        -webkit-user-select: none;
        -moz-user-select: none;
        -ms-user-select: none;
        user-select: none;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-user-select: none;
        user-select: none;
      }
    "#},
    Browsers {
      safari: Some(8 << 16),
      opera: Some(80 << 16),
      firefox: Some(80 << 16),
      edge: Some(80 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        -webkit-user-select: none;
        -moz-user-select: none;
        -ms-user-select: none;
        user-select: none;
      }
    "#, indoc! {r#"
      .foo {
        user-select: none;
      }
    "#},
    Browsers {
      opera: Some(80 << 16),
      firefox: Some(80 << 16),
      edge: Some(80 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        appearance: none;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-appearance: none;
        -moz-appearance: none;
        -ms-appearance: none;
        appearance: none;
      }
    "#},
    Browsers {
      safari: Some(8 << 16),
      chrome: Some(80 << 16),
      firefox: Some(10 << 16),
      ie: Some(11 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        -webkit-appearance: none;
        -moz-appearance: none;
        -ms-appearance: none;
        appearance: none;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-appearance: none;
        appearance: none;
      }
    "#},
    Browsers {
      safari: Some(15 << 16),
      chrome: Some(85 << 16),
      firefox: Some(80 << 16),
      edge: Some(85 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        -webkit-appearance: none;
        -moz-appearance: none;
        -ms-appearance: none;
        appearance: none;
      }
    "#, indoc! {r#"
      .foo {
        appearance: none;
      }
    "#},
    Browsers {
      chrome: Some(85 << 16),
      firefox: Some(80 << 16),
      edge: Some(85 << 16),
      ..Browsers::default()
    });
  }
}
