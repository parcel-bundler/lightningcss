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
      parser::CssRule::Style(style) => {
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

        parser::CssRule::Style(style)
      },
      r => r
    };
    rule.to_css_string()
  }).collect::<Vec<String>>().join("\n\n");
  println!("{}", res);

  ctx.env.get_undefined()
}

#[module_exports]
fn init(mut exports: JsObject) -> napi::Result<()> {
  exports.create_named_method("transform", transform)?;

  Ok(())
}
