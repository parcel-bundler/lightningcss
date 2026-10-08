//! Test-only JSONL bridge for the WPT harness. No production parser behavior is changed.
use cssparser::{Parser, ParserInput};
use lightningcss::{
  properties::{
    custom::{CustomPropertyName, TokenList, TokenOrValue},
    Property, PropertyId,
  },
  stylesheet::{MinifyOptions, ParserOptions, PrinterOptions, StyleAttribute},
  targets::{Browsers, Targets},
};
use serde_json::{json, Value};
use std::io::{self, BufRead, Write};

fn deferred(tokens: &TokenList) -> bool {
  tokens.0.iter().any(|token| match token {
    TokenOrValue::Var(_) | TokenOrValue::Env(_) => true,
    TokenOrValue::Function(f) => deferred(&f.arguments),
    _ => false,
  })
}

fn run(request: &Value) -> Result<Value, String> {
  let name = request["property"].as_str().ok_or("missing property")?;
  let value = request["value"].as_str().ok_or("missing value")?;
  let mode = request["mode"].as_str().unwrap_or("print");
  let mut input = ParserInput::new(value);
  let mut parser = Parser::new(&mut input);
  // A CSSOM setter accepts one value, not a declaration list. In particular,
  // reject trailing semicolons/!important instead of accidentally injecting CSS.
  let property = Property::parse(PropertyId::from(name), &mut parser, &ParserOptions::default())
    .map_err(|e| format!("{e:?}"))?;
  parser.expect_exhausted().map_err(|e| format!("{e:?}"))?;
  let recognition = match &property {
    Property::Custom(p) => match p.name {
      CustomPropertyName::Custom(_) => "custom-property",
      CustomPropertyName::Unknown(_) => "unknown-property",
    },
    Property::Unparsed(p) if deferred(&p.value) => "deferred",
    Property::Unparsed(_) => {
      let value = property
        .value_to_css_string(PrinterOptions::default())
        .map_err(|e| e.to_string())?;
      if matches!(
        value.to_ascii_lowercase().as_str(),
        "initial" | "inherit" | "unset" | "revert" | "revert-layer"
      ) {
        "css-wide"
      } else {
        "unparsed"
      }
    }
    _ => "typed",
  };
  // Construct from the already parsed property, never interpolate a setter value.
  let mut style = StyleAttribute::parse("", ParserOptions::default()).map_err(|e| e.to_string())?;
  style.declarations.declarations.push(property);
  let targets = if mode.starts_with("lower") {
    Targets {
      browsers: Some(Browsers {
        chrome: Some(80 << 16),
        ..Browsers::default()
      }),
      ..Targets::default()
    }
  } else {
    Targets::default()
  };
  if mode != "print" {
    style.minify(MinifyOptions {
      targets,
      ..MinifyOptions::default()
    });
  }
  let code = style
    .to_css(PrinterOptions {
      minify: mode == "minify" || mode == "lower-minify",
      targets,
      ..PrinterOptions::default()
    })
    .map_err(|e| e.to_string())?
    .code;
  Ok(json!({"recognition": recognition, "code": code}))
}

fn main() {
  let stdout = io::stdout();
  let mut out = stdout.lock();
  for line in io::stdin().lock().lines() {
    let line = line.expect("read JSONL request");
    let result = std::panic::catch_unwind(|| {
      let request: Value = serde_json::from_str(&line).map_err(|e| e.to_string())?;
      run(&request)
    });
    let response = match result {
      Ok(Ok(value)) => value,
      Ok(Err(error)) => json!({"recognition": "error", "error": error}),
      Err(payload) => {
        let message = payload
          .downcast_ref::<String>()
          .map(String::as_str)
          .or_else(|| payload.downcast_ref::<&str>().copied())
          .unwrap_or("non-string panic payload");
        json!({"recognition": "panic", "error": message})
      }
    };
    writeln!(out, "{response}").unwrap();
    out.flush().unwrap();
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn recognition_distinguishes_preservation_from_grammar_support() {
    for (name, value, expected) in [
      ("color", "red", "typed"),
      ("color", "not-a-color", "unparsed"),
      ("future-property", "anything", "unknown-property"),
      ("--custom", "anything", "custom-property"),
      ("color", "inherit", "css-wide"),
      ("width", "calc(var(--width) * 2)", "deferred"),
    ] {
      let result = run(&json!({"property": name, "value": value})).unwrap();
      assert_eq!(result["recognition"], expected, "{name}: {value}");
    }
  }

  #[test]
  fn setter_values_cannot_inject_declarations_or_importance() {
    for value in ["red; background: blue", "red !important", "red;", "red}"] {
      assert!(run(&json!({"property": "color", "value": value})).is_err(), "{value}");
    }
  }

  #[test]
  fn optimization_modes_emit_css() {
    for mode in ["print", "minify", "lower", "lower-minify"] {
      let result = run(&json!({"property": "color", "value": "rgb(255, 0, 0)", "mode": mode})).unwrap();
      assert_eq!(result["recognition"], "typed");
      assert!(result["code"].as_str().unwrap().contains("red"));
    }
  }
}
