mod parser;
pub mod rules;
pub mod declaration;
pub mod media_query;
mod selector;
pub mod properties;
pub mod values;
mod printer;
mod traits;
mod macros;
pub mod stylesheet;
mod compat;
mod prefixes;
pub mod vendor_prefix;
pub mod targets;
pub mod css_modules;
pub mod dependencies;
pub mod error;
mod logical;
pub mod bundler;

#[cfg(test)]
mod tests {
  use crate::dependencies::Dependency;
  use crate::error::{Error, MinifyErrorKind, ErrorLocation, ParserError};
  use crate::properties::custom::Token;
  use crate::rules::CssRule;
  use crate::stylesheet::*;
  use crate::targets::Browsers;
  use crate::rules::Location;
  use indoc::indoc;
  use std::{collections::HashMap};
  use crate::css_modules::{CssModuleExports, CssModuleExport, CssModuleReference};

  fn test(source: &str, expected: &str) {
    let mut stylesheet = StyleSheet::parse("test.css".into(), &source, ParserOptions::default()).unwrap();
    stylesheet.minify(MinifyOptions::default()).unwrap();
    let res = stylesheet.to_css(PrinterOptions::default()).unwrap();
    assert_eq!(res.code, expected);
  }

  fn minify_test(source: &str, expected: &str) {
    let mut stylesheet = StyleSheet::parse("test.css".into(), &source, ParserOptions::default()).unwrap();
    stylesheet.minify(MinifyOptions::default()).unwrap();
    let res = stylesheet.to_css(PrinterOptions { minify: true, ..PrinterOptions::default() }).unwrap();
    assert_eq!(res.code, expected);
  }

  fn prefix_test(source: &str, expected: &str, targets: Browsers) {
    let mut stylesheet = StyleSheet::parse("test.css".into(), &source, ParserOptions::default()).unwrap();
    stylesheet.minify(MinifyOptions { targets: Some(targets), ..MinifyOptions::default() }).unwrap();
    let res = stylesheet.to_css(PrinterOptions { targets: Some(targets), ..PrinterOptions::default() }).unwrap();
    assert_eq!(res.code, expected);
  }

  fn attr_test(source: &str, expected: &str, minify: bool, targets: Option<Browsers>) {
    let mut attr = StyleAttribute::parse(source).unwrap();
    attr.minify(MinifyOptions { targets, ..MinifyOptions::default() });
    let res = attr.to_css(PrinterOptions { targets, minify, ..PrinterOptions::default() }).unwrap();
    assert_eq!(res.code, expected);
  }

  fn nesting_test(source: &str, expected: &str) {
    let targets = Some(Browsers {
      chrome: Some(95 << 16),
      ..Browsers::default()
    });
    let mut stylesheet = StyleSheet::parse("test.css".into(), &source, ParserOptions { nesting: true, ..ParserOptions::default() }).unwrap();
    stylesheet.minify(MinifyOptions { targets, ..MinifyOptions::default() }).unwrap();
    let res = stylesheet.to_css(PrinterOptions { targets, ..PrinterOptions::default() }).unwrap();
    assert_eq!(res.code, expected);
  }

  fn nesting_test_no_targets(source: &str, expected: &str) {
    let mut stylesheet = StyleSheet::parse("test.css".into(), &source, ParserOptions { nesting: true, ..ParserOptions::default() }).unwrap();
    stylesheet.minify(MinifyOptions::default()).unwrap();
    let res = stylesheet.to_css(PrinterOptions::default()).unwrap();
    assert_eq!(res.code, expected);
  }

  fn css_modules_test(source: &str, expected: &str, expected_exports: CssModuleExports) {
    let mut stylesheet = StyleSheet::parse("test.css".into(), &source, ParserOptions { css_modules: true, ..ParserOptions::default() }).unwrap();
    stylesheet.minify(MinifyOptions::default()).unwrap();
    let res = stylesheet.to_css(PrinterOptions::default()).unwrap();
    assert_eq!(res.code, expected);
    assert_eq!(res.exports.unwrap(), expected_exports);
  }

  fn custom_media_test(source: &str, expected: &str) {
    let mut stylesheet = StyleSheet::parse("test.css".into(), &source, ParserOptions { custom_media: true, ..ParserOptions::default() }).unwrap();
    stylesheet.minify(MinifyOptions {
      targets: Some(Browsers { chrome: Some(95 << 16), ..Browsers::default() }),
      ..MinifyOptions::default()
    }).unwrap();
    let res = stylesheet.to_css(PrinterOptions::default()).unwrap();
    assert_eq!(res.code, expected);
  }

  fn error_test(source: &str, error: ParserError) {
    let res = StyleSheet::parse("test.css".into(), &source, ParserOptions::default());
    match res {
      Ok(_) => unreachable!(),
      Err(e) => assert_eq!(e.kind, error)
    }
  }

  macro_rules! map(
    { $($key:expr => $name:literal $(referenced: $referenced: literal)? $($value:literal $(global: $global: literal)? $(from $from:literal)?)*),* } => {
      {
        #[allow(unused_mut)]
        let mut m = HashMap::new();
        $(
          #[allow(unused_mut)]
          let mut v = Vec::new();
          #[allow(unused_macros)]
          macro_rules! insert {
            ($local:literal from $specifier:literal) => {
              v.push(CssModuleReference::Dependency {
                name: $local.into(),
                specifier: $specifier.into()
              });
            };
            ($local:literal global: $is_global: literal) => {
              v.push(CssModuleReference::Global {
                name: $local.into()
              });
            };
            ($local:literal) => {
              v.push(CssModuleReference::Local {
                name: $local.into()
              });
            };
          }
          $(
            insert!($value $(global: $global)? $(from $from)?);
          )*

          macro_rules! is_referenced {
            ($ref: literal) => {
              $ref
            };
            () => {
              false
            };
          }

          m.insert($key.into(), CssModuleExport {
            name: $name.into(),
            composes: v,
            is_referenced: is_referenced!($($referenced)?)
          });
        )*
        m
      }
    };
  );

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

    minify_test(".foo { border-width: 0 0 1px; }", ".foo{border-width:0 0 1px}");
    test(r#"
      .foo {
        border-block-width: 1px;
        border-inline-width: 1px;
      }
    "#, indoc! {r#"
      .foo {
        border-width: 1px;
      }
    "#
    });
    test(r#"
      .foo {
        border-block-start-width: 1px;
        border-block-end-width: 1px;
        border-inline-start-width: 1px;
        border-inline-end-width: 1px;
      }
    "#, indoc! {r#"
      .foo {
        border-width: 1px;
      }
    "#
    });
    test(r#"
      .foo {
        border-block-start-width: 1px;
        border-block-end-width: 1px;
        border-inline-start-width: 2px;
        border-inline-end-width: 2px;
      }
    "#, indoc! {r#"
      .foo {
        border-block-width: 1px;
        border-inline-width: 2px;
      }
    "#
    });
    test(r#"
      .foo {
        border-block-start-width: 1px;
        border-block-end-width: 1px;
        border-inline-start-width: 2px;
        border-inline-end-width: 3px;
      }
    "#, indoc! {r#"
      .foo {
        border-block-width: 1px;
        border-inline-start-width: 2px;
        border-inline-end-width: 3px;
      }
    "#
    });

    minify_test(".foo { border-bottom: 1px solid var(--spectrum-global-color-gray-200)}", ".foo{border-bottom:1px solid var(--spectrum-global-color-gray-200)}");
    test(r#"
      .foo {
        border-width: 0;
        border-bottom: var(--test, 1px) solid;
      }
    "#, indoc! {r#"
      .foo {
        border-width: 0;
        border-bottom: var(--test, 1px) solid;
      }
    "#
    });

    test(r#"
      .foo {
        border: 1px solid black;
        border-width: 1px 1px 0 0;
      }
    "#, indoc! {r#"
      .foo {
        border: 1px solid #000;
        border-width: 1px 1px 0 0;
      }
    "#});

    test(r#"
      .foo {
        border-top: 1px solid black;
        border-bottom: 1px solid black;
        border-left: 2px solid black;
        border-right: 2px solid black;
      }
    "#, indoc! {r#"
      .foo {
        border: 1px solid #000;
        border-width: 1px 2px;
      }
    "#});

    test(r#"
      .foo {
        border-top: 1px solid black;
        border-bottom: 1px solid black;
        border-left: 2px solid black;
        border-right: 1px solid black;
      }
    "#, indoc! {r#"
      .foo {
        border: 1px solid #000;
        border-left-width: 2px;
      }
    "#});

    test(r#"
      .foo {
        border-top: 1px solid black;
        border-bottom: 1px solid black;
        border-left: 1px solid red;
        border-right: 1px solid red;
      }
    "#, indoc! {r#"
      .foo {
        border: 1px solid #000;
        border-color: #000 red;
      }
    "#});

    test(r#"
      .foo {
        border-block-start: 1px solid black;
        border-block-end: 1px solid black;
        border-inline-start: 1px solid red;
        border-inline-end: 1px solid red;
      }
    "#, indoc! {r#"
      .foo {
        border: 1px solid #000;
        border-inline-color: red;
      }
    "#});

    test(r#"
      .foo {
        border-block-start: 1px solid black;
        border-block-end: 1px solid black;
        border-inline-start: 2px solid black;
        border-inline-end: 2px solid black;
      }
    "#, indoc! {r#"
      .foo {
        border: 1px solid #000;
        border-inline-width: 2px;
      }
    "#});

    test(r#"
      .foo {
        border-block-start: 1px solid black;
        border-block-end: 1px solid black;
        border-inline-start: 2px solid red;
        border-inline-end: 2px solid red;
      }
    "#, indoc! {r#"
      .foo {
        border: 1px solid #000;
        border-inline: 2px solid red;
      }
    "#});

    test(r#"
      .foo {
        border-block-start: 1px solid black;
        border-block-end: 1px solid black;
        border-inline-start: 2px solid red;
        border-inline-end: 3px solid red;
      }
    "#, indoc! {r#"
      .foo {
        border: 1px solid #000;
        border-inline-start: 2px solid red;
        border-inline-end: 3px solid red;
      }
    "#});

    test(r#"
      .foo {
        border-block-start: 2px solid black;
        border-block-end: 1px solid black;
        border-inline-start: 2px solid red;
        border-inline-end: 2px solid red;
      }
    "#, indoc! {r#"
      .foo {
        border: 2px solid red;
        border-block-start-color: #000;
        border-block-end: 1px solid #000;
      }
    "#});

    test(r#"
      .foo {
        border-block-start: 2px solid red;
        border-block-end: 1px solid red;
        border-inline-start: 2px solid red;
        border-inline-end: 2px solid red;
      }
    "#, indoc! {r#"
      .foo {
        border: 2px solid red;
        border-block-end-width: 1px;
      }
    "#});

    test(r#"
      .foo {
        border-block-start: 2px solid red;
        border-block-end: 2px solid red;
        border-inline-start: 2px solid red;
        border-inline-end: 1px solid red;
      }
    "#, indoc! {r#"
      .foo {
        border: 2px solid red;
        border-inline-end-width: 1px;
      }
    "#});

    prefix_test(r#"
      .foo {
        border-block: 2px solid red;
      }
    "#, indoc! {r#"
      .foo {
        border-top: 2px solid red;
        border-bottom: 2px solid red;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-block-start: 2px solid red;
      }
    "#, indoc! {r#"
      .foo {
        border-top: 2px solid red;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-block-end: 2px solid red;
      }
    "#, indoc! {r#"
      .foo {
        border-bottom: 2px solid red;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-inline: 2px solid red;
      }
    "#, indoc! {r#"
      .foo {
        border-left: 2px solid red;
        border-right: 2px solid red;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-inline-start: 2px solid red;
      }
    "#, indoc! {r#"
      .foo {
        border-left: var(--ltr, 2px solid red);
        border-right: var(--rtl, 2px solid red);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-inline-start-width: 2px;
      }
    "#, indoc! {r#"
      .foo {
        border-left-width: var(--ltr, 2px);
        border-right-width: var(--rtl, 2px);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-inline-end: 2px solid red;
      }
    "#, indoc! {r#"
      .foo {
        border-right: var(--ltr, 2px solid red);
        border-left: var(--rtl, 2px solid red);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-inline-start: 2px solid red;
        border-inline-end: 5px solid green;
      }
    "#, indoc! {r#"
      .foo {
        border-left: var(--ltr, 2px solid red) var(--rtl, 5px solid green);
        border-right: var(--ltr, 5px solid green) var(--rtl, 2px solid red);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-inline-start: 2px solid red;
        border-inline-end: 5px solid green;
      }

      .bar {
        border-inline-start: 1px dotted gray;
        border-inline-end: 1px solid black;
      }
    "#, indoc! {r#"
      .foo {
        border-left: var(--ltr, 2px solid red) var(--rtl, 5px solid green);
        border-right: var(--ltr, 5px solid green) var(--rtl, 2px solid red);
      }

      .bar {
        border-left: var(--ltr, 1px dotted gray) var(--rtl, 1px solid #000);
        border-right: var(--ltr, 1px solid #000) var(--rtl, 1px dotted gray);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-inline-end: var(--test);
      }
    "#, indoc! {r#"
      .foo {
        border-right: var(--ltr, var(--test));
        border-left: var(--rtl, var(--test));
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-inline-start: var(--start);
        border-inline-end: var(--end);
      }
    "#, indoc! {r#"
      .foo {
        border-left: var(--ltr, var(--start)) var(--rtl, var(--end));
        border-right: var(--ltr, var(--end)) var(--rtl, var(--start));
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
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
        border-image: url(foo.png) 60;
        border-image-source: var(--test);
      }
    "#, indoc! {r#"
      .foo {
        border-image: url(foo.png) 60;
        border-image-source: var(--test);
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

    prefix_test(r#"
      .foo {
        border-image: var(--test) 60;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-border-image: var(--test) 60;
        -moz-border-image: var(--test) 60;
        -o-border-image: var(--test) 60;
        border-image: var(--test) 60;
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
        border-radius: 10px;
        border-top-left-radius: 20px;
      }
    "#, indoc! {r#"
      .foo {
        border-radius: 20px 10px 10px;
      }
    "#
    });

    test(r#"
      .foo {
        border-radius: 10px;
        border-top-left-radius: var(--test);
      }
    "#, indoc! {r#"
      .foo {
        border-radius: 10px;
        border-top-left-radius: var(--test);
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
    });

    prefix_test(r#"
      .foo {
        border-radius: var(--test);
      }
    "#, indoc! {r#"
      .foo {
        -webkit-border-radius: var(--test);
        -moz-border-radius: var(--test);
        border-radius: var(--test);
      }
    "#
    }, Browsers {
      safari: Some(4 << 16),
      firefox: Some(3 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-start-start-radius: 5px;
      }
    "#, indoc! {r#"
      .foo {
        border-top-left-radius: var(--ltr, 5px);
        border-top-right-radius: var(--rtl, 5px);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(12 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-start-start-radius: 5px;
        border-start-end-radius: 10px;
      }
    "#, indoc! {r#"
      .foo {
        border-top-left-radius: var(--ltr, 5px) var(--rtl, 10px);
        border-top-right-radius: var(--ltr, 10px) var(--rtl, 5px);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(12 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-end-start-radius: 5px;
        border-end-end-radius: 10px;
      }
    "#, indoc! {r#"
      .foo {
        border-bottom-left-radius: var(--ltr, 5px) var(--rtl, 10px);
        border-bottom-right-radius: var(--ltr, 10px) var(--rtl, 5px);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(12 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-start-start-radius: var(--radius);
      }
    "#, indoc! {r#"
      .foo {
        border-top-left-radius: var(--ltr, var(--radius));
        border-top-right-radius: var(--rtl, var(--radius));
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(12 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        border-start-start-radius: var(--start);
        border-start-end-radius: var(--end);
      }
    "#, indoc! {r#"
      .foo {
        border-top-left-radius: var(--ltr, var(--start)) var(--rtl, var(--end));
        border-top-right-radius: var(--ltr, var(--end)) var(--rtl, var(--start));
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(12 << 16),
      ..Browsers::default()
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

    test(r#"
      .foo {
        outline: 2px solid yellow;
        outline-color: var(--color);
      }
    "#, indoc! {r#"
      .foo {
        outline: 2px solid #ff0;
        outline-color: var(--color);
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

    test(r#"
      .foo {
        margin: 10px;
        margin-top: 20px;
      }
    "#, indoc! {r#"
      .foo {
        margin: 20px 10px 10px;
      }
    "#
    });

    test(r#"
      .foo {
        margin: 10px;
        margin-top: var(--top);
      }
    "#, indoc! {r#"
      .foo {
        margin: 10px;
        margin-top: var(--top);
      }
    "#
    });

    prefix_test(r#"
      .foo {
        margin-inline-start: 2px;
      }
    "#, indoc! {r#"
      .foo {
        margin-left: var(--ltr, 2px);
        margin-right: var(--rtl, 2px);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        margin-inline-start: 2px;
        margin-inline-end: 4px;
      }
    "#, indoc! {r#"
      .foo {
        margin-left: var(--ltr, 2px) var(--rtl, 4px);
        margin-right: var(--ltr, 4px) var(--rtl, 2px);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        margin-inline: 2px;
      }
    "#, indoc! {r#"
      .foo {
        margin-left: 2px;
        margin-right: 2px;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        margin-block-start: 2px;
      }
    "#, indoc! {r#"
      .foo {
        margin-top: 2px;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        margin-block-end: 2px;
      }
    "#, indoc! {r#"
      .foo {
        margin-bottom: 2px;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
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

    prefix_test(r#"
      .foo {
        padding-inline-start: 2px;
      }
    "#, indoc! {r#"
      .foo {
        padding-left: var(--ltr, 2px);
        padding-right: var(--rtl, 2px);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        padding-inline-start: 2px;
        padding-inline-end: 4px;
      }
    "#, indoc! {r#"
      .foo {
        padding-left: var(--ltr, 2px) var(--rtl, 4px);
        padding-right: var(--ltr, 4px) var(--rtl, 2px);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        padding-inline: 2px;
      }
    "#, indoc! {r#"
      .foo {
        padding-left: 2px;
        padding-right: 2px;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        padding-block-start: 2px;
      }
    "#, indoc! {r#"
      .foo {
        padding-top: 2px;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        padding-block-end: 2px;
      }
    "#, indoc! {r#"
      .foo {
        padding-bottom: 2px;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        padding-top: 1px;
        padding-left: 2px;
        padding-bottom: 3px;
        padding-right: 4px;
      }
    "#, indoc! {r#"
      .foo {
        padding: 1px 4px 3px 2px;
      }
    "#}, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });
  }

  #[test]
  fn test_scroll_padding() {
    prefix_test(r#"
      .foo {
        scroll-padding-inline: 2px;
      }
    "#, indoc! {r#"
      .foo {
        scroll-padding-inline: 2px;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });
  }

  #[test]
  fn test_size() {
    prefix_test(r#"
      .foo {
        block-size: 25px;
        inline-size: 25px;
        min-block-size: 25px;
        min-inline-size: 25px;
      }
    "#, indoc! {r#"
      .foo {
        height: 25px;
        width: 25px;
        min-height: 25px;
        min-width: 25px;
      }
    "#}, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        block-size: 25px;
        inline-size: 25px;
        min-block-size: 25px;
        min-inline-size: 25px;
      }
    "#, indoc! {r#"
      .foo {
        block-size: 25px;
        inline-size: 25px;
        min-block-size: 25px;
        min-inline-size: 25px;
      }
    "#}, Browsers {
      safari: Some(14 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        block-size: var(--size);
        inline-size: var(--size);
        min-block-size: var(--size);
        min-inline-size: var(--size);
      }
    "#, indoc! {r#"
      .foo {
        height: var(--size);
        width: var(--size);
        min-height: var(--size);
        min-width: var(--size);
      }
    "#}, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
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
    "#, indoc! {".foo{background-position:50%}"
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

    test(r#"
      .foo {
        background: url(img.png) gray;
        background-position: var(--pos);
      }
    "#, indoc! {r#"
      .foo {
        background: gray url(img.png);
        background-position: var(--pos);
      }
    "#
    });

    minify_test(".foo { background-position: bottom left }", ".foo{background-position:0 100%}");
    minify_test(".foo { background-position: left 10px center }", ".foo{background-position:10px 50%}");
    minify_test(".foo { background-position: right 10px center }", ".foo{background-position:right 10px center}");
    minify_test(".foo { background-position: right 10px top 20px }", ".foo{background-position:right 10px top 20px}");
    minify_test(".foo { background-position: left 10px top 20px }", ".foo{background-position:10px 20px}");
    minify_test(".foo { background-position: left 10px bottom 20px }", ".foo{background-position:left 10px bottom 20px}");
    minify_test(".foo { background-position: left 10px top }", ".foo{background-position:10px 0}");
    minify_test(".foo { background-position: bottom right }", ".foo{background-position:100% 100%}");

    minify_test(".foo { background: url('img-sprite.png') no-repeat bottom right }", ".foo{background:url(img-sprite.png) 100% 100% no-repeat}");
    minify_test(".foo { background: transparent }", ".foo{background:0 0}");

    minify_test(".foo { background: url(\"data:image/svg+xml,%3Csvg width='168' height='24' xmlns='http://www.w3.org/2000/svg'%3E%3C/svg%3E\") }", ".foo{background:url(\"data:image/svg+xml,%3Csvg width='168' height='24' xmlns='http://www.w3.org/2000/svg'%3E%3C/svg%3E\")}");
  
    test(r#"
      .foo {
        background: url(img.png);
        background-clip: text;
      }
    "#, indoc! {r#"
      .foo {
        background: url(img.png) text;
      }
    "#
    });

    prefix_test(r#"
      .foo {
        background: url(img.png);
        background-clip: text;
      }
    "#, indoc! {r#"
      .foo {
        background: url(img.png);
        -webkit-background-clip: text;
        background-clip: text;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        background: url(img.png);
        background-clip: text;
      }
    "#, indoc! {r#"
      .foo {
        background: url(img.png) text;
      }
    "#
    }, Browsers {
      safari: Some(14 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        background: url(img.png);
        background-clip: text;
      }
    "#, indoc! {r#"
      .foo {
        background: url(img.png);
        -webkit-background-clip: text;
        background-clip: text;
      }
    "#
    }, Browsers {
      safari: Some(14 << 16),
      chrome: Some(95 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        background-image: url(img.png);
        background-clip: text;
      }
    "#, indoc! {r#"
      .foo {
        background-image: url(img.png);
        -webkit-background-clip: text;
        background-clip: text;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        background-image: url(img.png);
        background-clip: text;
      }
    "#, indoc! {r#"
      .foo {
        background-image: url(img.png);
        background-clip: text;
      }
    "#
    }, Browsers {
      safari: Some(14 << 16),
      ..Browsers::default()
    });

    minify_test(".foo { background: none center }", ".foo{background:50%}");
    minify_test(".foo { background: none }", ".foo{background:0 0}");
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
        flex: 0 0;
        flex-grow: var(--grow);
      }
    "#, indoc! {r#"
      .foo {
        flex: 0 0;
        flex-grow: var(--grow);
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
        place-items: center;
        justify-items: var(--justify);
      }
    "#, indoc! {r#"
      .foo {
        place-items: center;
        justify-items: var(--justify);
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
        align-content: space-between;
        -webkit-justify-content: flex-end;
        justify-content: flex-end;
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
        place-content: space-between flex-end;
      }
    "#, indoc! {r#"
      .foo {
        align-content: space-between;
        justify-content: flex-end;
      }
    "#},
    Browsers {
      chrome: Some(30 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        place-content: space-between flex-end;
      }
    "#, indoc! {r#"
      .foo {
        place-content: space-between flex-end;
      }
    "#},
    Browsers {
      chrome: Some(60 << 16),
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
        align-self: center;
        justify-self: flex-end;
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
        place-self: center flex-end;
      }
    "#, indoc! {r#"
      .foo {
        align-self: center;
        justify-self: flex-end;
      }
    "#},
    Browsers {
      chrome: Some(57 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        place-self: center flex-end;
      }
    "#, indoc! {r#"
      .foo {
        place-self: center flex-end;
      }
    "#},
    Browsers {
      chrome: Some(59 << 16),
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
        align-items: flex-end;
        justify-items: center;
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
        place-items: flex-end center;
      }
    "#, indoc! {r#"
      .foo {
        align-items: flex-end;
        justify-items: center;
      }
    "#},
    Browsers {
      safari: Some(10 << 16),
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
    "#, indoc! {".foo{font:italic small-caps 700 125% 12px/1.2em Helvetica,Times New Roman,sans-serif}"
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

    test(r#"
      .foo {
        font: 12px "Helvetica", "Times New Roman", sans-serif;
        line-height: var(--lh);
      }
    "#, indoc! {r#"
      .foo {
        font: 12px Helvetica, Times New Roman, sans-serif;
        line-height: var(--lh);
      }
    "#
    });

    minify_test(r#"
      .foo {
        font-family: "Helvetica", "Times New Roman", sans-serif;
        font-size: 12px;
        font-stretch: expanded;
      }
    "#, indoc! {".foo{font-family:Helvetica,Times New Roman,sans-serif;font-size:12px;font-stretch:125%}"
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

    minify_test(".foo { font: normal normal 600 9px/normal Charcoal; }", ".foo{font:600 9px Charcoal}");
    minify_test(".foo { font: normal normal 500 medium/normal Charcoal; }", ".foo{font:500 medium Charcoal}");
    minify_test(".foo { font: normal normal 400 medium Charcoal; }", ".foo{font:400 medium Charcoal}");
    minify_test(".foo { font: normal normal 500 medium/10px Charcoal; }", ".foo{font:500 medium/10px Charcoal}");
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
    minify_test(".custom-range::-webkit-slider-thumb:active {}", ".custom-range::-webkit-slider-thumb:active{}");
    minify_test(".test:not(.foo, .bar) {}", ".test:not(.foo,.bar){}");
    minify_test(".test:is(.foo, .bar) {}", ".test:is(.foo,.bar){}");
    minify_test(".test:where(.foo, .bar) {}", ".test:where(.foo,.bar){}");
    minify_test(".test:where(.foo, .bar) {}", ".test:where(.foo,.bar){}");
    minify_test(":host {}", ":host{}");
    minify_test(":host(.foo) {}", ":host(.foo){}");
    minify_test("::slotted(span) {}", "::slotted(span){}");
    minify_test("custom-element::part(foo) {}", "custom-element::part(foo){}");
    minify_test(".sm\\:text-5xl { font-size: 3rem }", ".sm\\:text-5xl{font-size:3rem}");
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

    minify_test(".foo { top: calc(-1 * clamp(1.75rem, 8vw, 4rem)) }", ".foo{top:calc(-1*clamp(1.75rem,8vw,4rem))}");
    minify_test(".foo { top: calc(-1 * min(1.75rem, 8vw, 4rem)) }", ".foo{top:calc(-1*min(1.75rem,8vw))}");
    minify_test(".foo { top: calc(-1 * max(1.75rem, 8vw, 4rem)) }", ".foo{top:calc(-1*max(4rem,8vw))}");
    minify_test(".foo { top: calc(clamp(1.75rem, 8vw, 4rem) * -1) }", ".foo{top:calc(-1*clamp(1.75rem,8vw,4rem))}");
    minify_test(".foo { top: calc(min(1.75rem, 8vw, 4rem) * -1) }", ".foo{top:calc(-1*min(1.75rem,8vw))}");
    minify_test(".foo { top: calc(max(1.75rem, 8vw, 4rem) * -1) }", ".foo{top:calc(-1*max(4rem,8vw))}");
    minify_test(".foo { top: calc(clamp(1.75rem, 8vw, 4rem) / 2) }", ".foo{top:calc(clamp(1.75rem,8vw,4rem)/2)}");
    minify_test(".foo { top: calc(min(1.75rem, 8vw, 4rem) / 2) }", ".foo{top:calc(min(1.75rem,8vw)/2)}");
    minify_test(".foo { top: calc(max(1.75rem, 8vw, 4rem) / 2) }", ".foo{top:calc(max(4rem,8vw)/2)}");
    minify_test(".foo { top: calc(0.5 * clamp(1.75rem, 8vw, 4rem)) }", ".foo{top:calc(clamp(1.75rem,8vw,4rem)/2)}");
    minify_test(".foo { top: calc(1 * clamp(1.75rem, 8vw, 4rem)) }", ".foo{top:calc(clamp(1.75rem,8vw,4rem))}");
    minify_test(".foo { top: calc(2 * clamp(1.75rem, 8vw, 4rem) / 2) }", ".foo{top:calc(clamp(1.75rem,8vw,4rem))}");

    minify_test(".foo { width: max(0px, 1vw) }", ".foo{width:max(0px,1vw)}");

    prefix_test(
      ".foo { border-width: clamp(1em, 2px, 4vh) }",
      indoc! { r#"
        .foo {
          border-width: max(1em, min(2px, 4vh));
        }
      "#},
      Browsers {
        safari: Some(12 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      ".foo { border-width: clamp(1em, 2px, 4vh) }",
      indoc! { r#"
        .foo {
          border-width: clamp(1em, 2px, 4vh);
        }
      "#},
      Browsers {
        safari: Some(14 << 16),
        ..Browsers::default()
      }
    );
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
    minify_test("@media (min-width: 240px) { .foo { color: chartreuse }}", "@media (min-width:240px){.foo{color:#7fff00}}");
    minify_test("@media (width < 240px) { .foo { color: chartreuse }}", "@media (width<240px){.foo{color:#7fff00}}");
    minify_test("@media (width <= 240px) { .foo { color: chartreuse }}", "@media (width<=240px){.foo{color:#7fff00}}");
    minify_test("@media (width > 240px) { .foo { color: chartreuse }}", "@media (width>240px){.foo{color:#7fff00}}");
    minify_test("@media (width >= 240px) { .foo { color: chartreuse }}", "@media (width>=240px){.foo{color:#7fff00}}");
    minify_test("@media (240px < width) { .foo { color: chartreuse }}", "@media (width>240px){.foo{color:#7fff00}}");
    minify_test("@media (240px <= width) { .foo { color: chartreuse }}", "@media (width>=240px){.foo{color:#7fff00}}");
    minify_test("@media (240px > width) { .foo { color: chartreuse }}", "@media (width<240px){.foo{color:#7fff00}}");
    minify_test("@media (240px >= width) { .foo { color: chartreuse }}", "@media (width<=240px){.foo{color:#7fff00}}");
    minify_test("@media (100px < width < 200px) { .foo { color: chartreuse }}", "@media (100px<width<200px){.foo{color:#7fff00}}");
    minify_test("@media (100px <= width <= 200px) { .foo { color: chartreuse }}", "@media (100px<=width<=200px){.foo{color:#7fff00}}");
    minify_test("@media (min-width: 30em) and (max-width: 50em) { .foo { color: chartreuse }}", "@media (min-width:30em) and (max-width:50em){.foo{color:#7fff00}}");
    minify_test("@media screen, print { .foo { color: chartreuse }}", "@media screen,print{.foo{color:#7fff00}}");
    minify_test("@media (hover: hover) { .foo { color: chartreuse }}", "@media (hover:hover){.foo{color:#7fff00}}");
    minify_test("@media (hover) { .foo { color: chartreuse }}", "@media (hover){.foo{color:#7fff00}}");
    minify_test("@media (aspect-ratio: 11/5) { .foo { color: chartreuse }}", "@media (aspect-ratio:11/5){.foo{color:#7fff00}}");
    minify_test("@media (aspect-ratio: 2/1) { .foo { color: chartreuse }}", "@media (aspect-ratio:2){.foo{color:#7fff00}}");
    minify_test("@media (aspect-ratio: 2) { .foo { color: chartreuse }}", "@media (aspect-ratio:2){.foo{color:#7fff00}}");
    minify_test("@media not screen and (color) { .foo { color: chartreuse }}", "@media not screen and (color){.foo{color:#7fff00}}");
    minify_test("@media only screen and (color) { .foo { color: chartreuse }}", "@media only screen and (color){.foo{color:#7fff00}}");
    minify_test("@media (update: slow) or (hover: none) { .foo { color: chartreuse }}", "@media (update:slow) or (hover:none){.foo{color:#7fff00}}");
    minify_test("@media (width < 600px) and (height < 600px) { .foo { color: chartreuse }}", "@media (width<600px) and (height<600px){.foo{color:#7fff00}}");
    minify_test("@media (not (color)) or (hover) { .foo { color: chartreuse }}", "@media (not (color)) or (hover){.foo{color:#7fff00}}");
    error_test("@media (example, all,), speech { .foo { color: chartreuse }}", ParserError::UnexpectedToken(Token::Comma));
    error_test("@media &test, speech { .foo { color: chartreuse }}", ParserError::UnexpectedToken(Token::Delim('&')));
    error_test("@media &test { .foo { color: chartreuse }}", ParserError::UnexpectedToken(Token::Delim('&')));
    minify_test("@media (min-width: calc(200px + 40px)) { .foo { color: chartreuse }}", "@media (min-width:240px){.foo{color:#7fff00}}");
    minify_test("@media (min-width: calc(1em + 5px)) { .foo { color: chartreuse }}", "@media (min-width:calc(1em + 5px)){.foo{color:#7fff00}}");
    minify_test("@media { .foo { color: chartreuse }}", ".foo{color:#7fff00}");
    minify_test("@media all { .foo { color: chartreuse }}", ".foo{color:#7fff00}");

    prefix_test(
      r#"
        @media (width >= 240px) {
          .foo {
            color: chartreuse;
          }
        }
      "#,
      indoc! { r#"
        @media (min-width: 240px) {
          .foo {
            color: #7fff00;
          }
        }
      "#},
      Browsers {
        firefox: Some(60 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      r#"
        @media (width >= 240px) {
          .foo {
            color: chartreuse;
          }
        }
      "#,
      indoc! { r#"
        @media (width >= 240px) {
          .foo {
            color: #7fff00;
          }
        }
      "#},
      Browsers {
        firefox: Some(64 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      r#"
        @media (width > 240px) {
          .foo {
            color: chartreuse;
          }
        }
      "#,
      indoc! { r#"
        @media (min-width: 240.001px) {
          .foo {
            color: #7fff00;
          }
        }
      "#},
      Browsers {
        firefox: Some(60 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      r#"
        @media (width <= 240px) {
          .foo {
            color: chartreuse;
          }
        }
      "#,
      indoc! { r#"
        @media (max-width: 240px) {
          .foo {
            color: #7fff00;
          }
        }
      "#},
      Browsers {
        firefox: Some(60 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      r#"
        @media (width <= 240px) {
          .foo {
            color: chartreuse;
          }
        }
      "#,
      indoc! { r#"
        @media (width <= 240px) {
          .foo {
            color: #7fff00;
          }
        }
      "#},
      Browsers {
        firefox: Some(64 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      r#"
        @media (width < 240px) {
          .foo {
            color: chartreuse;
          }
        }
      "#,
      indoc! { r#"
        @media (max-width: 239.999px) {
          .foo {
            color: #7fff00;
          }
        }
      "#},
      Browsers {
        firefox: Some(60 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      r#"
        @media (100px <= width <= 200px) {
          .foo {
            color: chartreuse;
          }
        }
      "#,
      indoc! { r#"
        @media (min-width: 100px) and (max-width: 200px) {
          .foo {
            color: #7fff00;
          }
        }
      "#},
      Browsers {
        firefox: Some(85 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      r#"
        @media (100px < width < 200px) {
          .foo {
            color: chartreuse;
          }
        }
      "#,
      indoc! { r#"
        @media (min-width: 100.001px) and (max-width: 199.999px) {
          .foo {
            color: #7fff00;
          }
        }
      "#},
      Browsers {
        firefox: Some(85 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      r#"
        @media (200px >= width >= 100px) {
          .foo {
            color: chartreuse;
          }
        }
      "#,
      indoc! { r#"
        @media (max-width: 200px) and (min-width: 100px) {
          .foo {
            color: #7fff00;
          }
        }
      "#},
      Browsers {
        firefox: Some(85 << 16),
        ..Browsers::default()
      }
    );
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
        color: red;
      }
      .foo {
        background: green !important;
      }
    "#, indoc! {r#"
      .foo {
        color: red;
        background: green !important;
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

    test(r#"
      [foo="bar"] {
        color: red;
      }
      .bar {
        color: red;
      }
    "#, indoc! {r#"
      [foo="bar"] {
        color: red;
      }

      .bar {
        color: red;
      }
    "#});

    prefix_test(r#"
      [foo="bar"] {
        color: red;
      }
      .bar {
        color: red;
      }
    "#, indoc! {r#"
      [foo="bar"] {
        color: red;
      }
      
      .bar {
        color: red;
      }
    "#}, Browsers {
      ie: Some(6 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      [foo="bar"] {
        color: red;
      }
      .bar {
        color: red;
      }
    "#, indoc! {r#"
      [foo="bar"], .bar {
        color: red;
      }
    "#}, Browsers {
      ie: Some(10 << 16),
      ..Browsers::default()
    });

    test(r#"
      .foo:-moz-read-only {
        color: red;
      }
    "#, indoc! {r#"
      .foo:-moz-read-only {
        color: red;
      }
    "#});

    test(r#"
      .foo:-moz-read-only {
        color: red;
      }
      
      .foo:read-only {
        color: red;
      }
    "#, indoc! {r#"
      .foo:-moz-read-only {
        color: red;
      }
      
      .foo:read-only {
        color: red;
      }
    "#});

    prefix_test(r#"
      .foo:-moz-read-only {
        color: red;
      }
      
      .foo:read-only {
        color: red;
      }
    "#, indoc! {r#"
      .foo:read-only {
        color: red;
      }
    "#}, Browsers {
      firefox: Some(85 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo:-moz-read-only {
        color: red;
      }

      .bar {
        color: yellow;
      }
      
      .foo:read-only {
        color: red;
      }
    "#, indoc! {r#"
      .foo:-moz-read-only {
        color: red;
      }

      .bar {
        color: #ff0;
      }
      
      .foo:read-only {
        color: red;
      }
    "#}, Browsers {
      firefox: Some(85 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo:-moz-read-only {
        color: red;
      }
      
      .foo:read-only {
        color: red;
      }
    "#, indoc! {r#"
      .foo:-moz-read-only {
        color: red;
      }
      
      .foo:read-only {
        color: red;
      }
    "#}, Browsers {
      firefox: Some(36 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo:read-only {
        color: red;
      }
    "#, indoc! {r#"
      .foo:-moz-read-only {
        color: red;
      }
      
      .foo:read-only {
        color: red;
      }
    "#}, Browsers {
      firefox: Some(36 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo:-webkit-full-screen {
        color: red;
      }
      .foo:-moz-full-screen {
        color: red;
      }
      .foo:-ms-fullscreen {
        color: red;
      }
      .foo:fullscreen {
        color: red;
      }
    "#, indoc! {r#"
      .foo:fullscreen {
        color: red;
      }
    "#}, Browsers {
      chrome: Some(96 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo:fullscreen {
        color: red;
      }
    "#, indoc! {r#"
      .foo:-webkit-full-screen {
        color: red;
      }

      .foo:-moz-full-screen {
        color: red;
      }

      .foo:-ms-fullscreen {
        color: red;
      }

      .foo:fullscreen {
        color: red;
      }
    "#}, Browsers {
      chrome: Some(45 << 16),
      firefox: Some(45 << 16),
      ie: Some(11 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo::placeholder {
        color: red;
      }
    "#, indoc! {r#"
      .foo::-webkit-input-placeholder {
        color: red;
      }

      .foo::-moz-placeholder {
        color: red;
      }

      .foo::-ms-input-placeholder {
        color: red;
      }

      .foo::placeholder {
        color: red;
      }
    "#}, Browsers {
      chrome: Some(45 << 16),
      firefox: Some(45 << 16),
      ie: Some(11 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo::file-selector-button {
        color: red;
      }
    "#, indoc! {r#"
      .foo::-webkit-file-upload-button {
        color: red;
      }

      .foo::-ms-browse {
        color: red;
      }

      .foo::file-selector-button {
        color: red;
      }
    "#}, Browsers {
      chrome: Some(84 << 16),
      ie: Some(10 << 16),
      ..Browsers::default()
    });
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
        transition: opacity 500ms;
        transition-timing-function: var(--ease);
      }
    "#, indoc! {r#"
      .foo {
        transition: opacity .5s;
        transition-timing-function: var(--ease);
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

    prefix_test(r#"
      .foo {
        transition-property: margin-inline-start;
      }
    "#, indoc! {r#"
      .foo {
        transition-property: var(--ltr, margin-left) var(--rtl, margin-right);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        transition-property: margin-inline-start, padding-inline-start;
      }
    "#, indoc! {r#"
      .foo {
        transition-property: var(--ltr, margin-left, padding-left) var(--rtl, margin-right, padding-right);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        transition-property: margin-inline-start, opacity, padding-inline-start, color;
      }
    "#, indoc! {r#"
      .foo {
        transition-property: var(--ltr, margin-left, opacity, padding-left, color) var(--rtl, margin-right, opacity, padding-right, color);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        transition-property: margin-block;
      }
    "#, indoc! {r#"
      .foo {
        transition-property: margin-top, margin-bottom;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        transition: margin-inline-start 2s;
      }
    "#, indoc! {r#"
      .foo {
        transition: var(--ltr, margin-left 2s) var(--rtl, margin-right 2s);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        transition: margin-inline-start 2s, padding-inline-start 2s;
      }
    "#, indoc! {r#"
      .foo {
        transition: var(--ltr, margin-left 2s, padding-left 2s) var(--rtl, margin-right 2s, padding-right 2s);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        transition: margin-block-start 2s;
      }
    "#, indoc! {r#"
      .foo {
        transition: margin-top 2s;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        transition: transform;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-transition: -webkit-transform, transform;
        transition: -webkit-transform, transform;
      }
    "#
    }, Browsers {
      safari: Some(6 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        transition: border-start-start-radius;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-transition: var(--ltr, border-top-left-radius) var(--rtl, border-top-right-radius);
        transition: var(--ltr, border-top-left-radius) var(--rtl, border-top-right-radius);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(4 << 16),
      ..Browsers::default()
    });

    test(r#"
      .foo {
        -webkit-transition: background 200ms;
        -moz-transition: background 200ms;
        transition: background 230ms;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-transition: background .2s;
        -moz-transition: background .2s;
        transition: background .23s;
      }
    "#});

    prefix_test(r#"
      .foo {
        -webkit-transition: background 200ms;
        -moz-transition: background 200ms;
        transition: background 230ms;
      }
    "#, indoc! {r#"
      .foo {
        -webkit-transition: background .2s;
        -moz-transition: background .2s;
        transition: background .23s;
      }
    "#}, Browsers {
      chrome: Some(95 << 16),
      ..Browsers::default()
    });
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
        animation: bar 200ms;
        animation-timing-function: var(--ease);
      }
    "#, indoc! {r#"
      .foo {
        animation: bar .2s;
        animation-timing-function: var(--ease);
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
    prefix_test(r#"
      .foo {
        animation: bar 200ms var(--ease);
      }
    "#, indoc! {r#"
      .foo {
        -webkit-animation: bar 200ms var(--ease);
        -moz-animation: bar 200ms var(--ease);
        animation: bar 200ms var(--ease);
      }
    "#}, Browsers {
      firefox: Some(6 << 16),
      safari: Some(6 << 16),
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

    prefix_test(r#"
      .foo {
        transform: var(--transform);
      }
    "#, indoc! {r#"
      .foo {
        -webkit-transform: var(--transform);
        -moz-transform: var(--transform);
        transform: var(--transform);
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
      ".foo{background:radial-gradient(at 0 0,#ff0,#00f)}"
    );
    minify_test(
      ".foo { background: radial-gradient(5em circle at top left, yellow, blue) }",
      ".foo{background:radial-gradient(5em at 0 0,#ff0,#00f)}"
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
        background-image: -webkit-radial-gradient(20px at 0 0, red, #00f);
        background-image: radial-gradient(20px at 0 0, red, #00f);
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
        background-image: -webkit-radial-gradient(20px at 0% 0%, red, #00f);
        background-image: radial-gradient(20px at 0% 0%, red, #00f);
      }
      "#,
      indoc! {r#"
      .foo {
        background-image: radial-gradient(20px at 0 0, red, #00f);
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
        background: -webkit-radial-gradient(20px at 0% 0%, red, #00f);
        background: radial-gradient(20px at 0% 0%, red, #00f);
      }
      "#,
      indoc! {r#"
      .foo {
        background: radial-gradient(20px at 0 0, red, #00f);
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

    prefix_test(
      r#"
      .foo {
        background: linear-gradient(yellow, red 30% 40%, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background: linear-gradient(#ff0, red 30%, red 40%, #00f);
      }
      "#},
      Browsers {
        chrome: Some(70 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      r#"
      .foo {
        background: linear-gradient(yellow, red 30% 40%, blue);
      }
      "#,
      indoc! {r#"
      .foo {
        background: linear-gradient(#ff0, red 30% 40%, #00f);
      }
      "#},
      Browsers {
        chrome: Some(71 << 16),
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
    minify_test("@font-face {src: url(\"test.woff\") format(woff);}", "@font-face{src:url(test.woff)format(\"woff\")}");
    minify_test("@font-face {src: url(\"test.woff\") format(woff), url(test.ttf) format(truetype);}", "@font-face{src:url(test.woff)format(\"woff\"),url(test.ttf)format(\"truetype\")}");
    minify_test("@font-face {src: url(\"test.woff\") format(woff supports features(opentype));}", "@font-face{src:url(test.woff)format(\"woff\" supports features(opentype))}");
    minify_test("@font-face {src: url(\"test.woff\") format(woff supports color(COLRv1));}", "@font-face{src:url(test.woff)format(\"woff\" supports color(colrv1))}");
    minify_test("@font-face {src: url(\"test.woff\") format(woff supports variations);}", "@font-face{src:url(test.woff)format(\"woff\" supports variations)}");
    minify_test("@font-face {src: url(\"test.woff\") format(woff supports palettes);}", "@font-face{src:url(test.woff)format(\"woff\" supports palettes)}");
    minify_test("@font-face {src: url(\"test.woff\") format(woff supports features(opentype) color(sbix));}", "@font-face{src:url(test.woff)format(\"woff\" supports features(opentype) color(sbix))}");
    minify_test("@font-face {font-weight: 200 400}", "@font-face{font-weight:200 400}");
    minify_test("@font-face {font-weight: 400 400}", "@font-face{font-weight:400}");
    minify_test("@font-face {font-stretch: 50% 200%}", "@font-face{font-stretch:50% 200%}");
    minify_test("@font-face {font-stretch: 50% 50%}", "@font-face{font-stretch:50%}");
    minify_test("@font-face {unicode-range: U+26;}", "@font-face{unicode-range:U+26}");
    minify_test("@font-face {unicode-range: u+26;}", "@font-face{unicode-range:U+26}");
    minify_test("@font-face {unicode-range: U+0-7F;}", "@font-face{unicode-range:U+0-7F}");
    minify_test("@font-face {unicode-range: U+0025-00FF;}", "@font-face{unicode-range:U+25-FF}");
    minify_test("@font-face {unicode-range: U+4??;}", "@font-face{unicode-range:U+4??}");
    minify_test("@font-face {unicode-range: U+400-4FF;}", "@font-face{unicode-range:U+4??}");
    minify_test("@font-face {unicode-range: U+0025-00FF, U+4??;}", "@font-face{unicode-range:U+25-FF,U+4??}");
    minify_test("@font-face {unicode-range: U+A5, U+4E00-9FFF, U+30??, U+FF00-FF9F;}", "@font-face{unicode-range:U+A5,U+4E00-9FFF,U+30??,U+FF00-FF9F}");
    minify_test("@font-face {unicode-range: U+????;}", "@font-face{unicode-range:U+????}");
    minify_test("@font-face {unicode-range: U+0000-FFFF;}", "@font-face{unicode-range:U+????}");
    minify_test("@font-face {unicode-range: U+10????;}", "@font-face{unicode-range:U+10????}");
    minify_test("@font-face {unicode-range: U+100000-10FFFF;}", "@font-face{unicode-range:U+10????}");
    minify_test("@font-face {unicode-range: U+1e1e?;}", "@font-face{unicode-range:U+1E1E?}");
    minify_test("@font-face {unicode-range: u+????, U+1????, U+10????;}", "@font-face{unicode-range:U+????,U+1????,U+10????}");
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
  fn test_supports_rule() {
    test(r#"
      @supports (foo: bar) {
        .test {
          foo: bar;
        }
      }
    "#, indoc! { r#"
      @supports (foo: bar) {
        .test {
          foo: bar;
        }
      }
    "#});
    test(r#"
      @supports not (foo: bar) {
        .test {
          foo: bar;
        }
      }
    "#, indoc! { r#"
      @supports not (foo: bar) {
        .test {
          foo: bar;
        }
      }
    "#});
    test(r#"
      @supports (foo: bar) or (bar: baz) {
        .test {
          foo: bar;
        }
      }
    "#, indoc! { r#"
      @supports (foo: bar) or (bar: baz) {
        .test {
          foo: bar;
        }
      }
    "#});
    test(r#"
      @supports (foo: bar) and (bar: baz) {
        .test {
          foo: bar;
        }
      }
    "#, indoc! { r#"
      @supports (foo: bar) and (bar: baz) {
        .test {
          foo: bar;
        }
      }
    "#});
    test(r#"
      @supports selector(a > b) {
        .test {
          foo: bar;
        }
      }
    "#, indoc! { r#"
      @supports selector(a > b) {
        .test {
          foo: bar;
        }
      }
    "#});
    test(r#"
      @supports unknown(test) {
        .test {
          foo: bar;
        }
      }
    "#, indoc! { r#"
      @supports unknown(test) {
        .test {
          foo: bar;
        }
      }
    "#});
    test(r#"
      @supports (unknown) {
        .test {
          foo: bar;
        }
      }
    "#, indoc! { r#"
      @supports (unknown) {
        .test {
          foo: bar;
        }
      }
    "#});
    test(r#"
      @supports (display: grid) and (not (display: inline-grid)) {
        .test {
          foo: bar;
        }
      }
    "#, indoc! { r#"
      @supports (display: grid) and (not (display: inline-grid)) {
        .test {
          foo: bar;
        }
      }
    "#});
  }

  #[test]
  fn test_counter_style() {
    test(r#"
      @counter-style circled-alpha {
        system: fixed;
        symbols: Ⓐ Ⓑ Ⓒ;
        suffix: " ";
      }
    "#, indoc! { r#"
      @counter-style circled-alpha {
        system: fixed;
        symbols: Ⓐ Ⓑ Ⓒ;
        suffix: " ";
      }
    "#});
  }

  #[test]
  fn test_namespace() {
    minify_test("@namespace url(http://toto.example.org);", "@namespace \"http://toto.example.org\";");
    minify_test("@namespace \"http://toto.example.org\";", "@namespace \"http://toto.example.org\";");
    minify_test("@namespace toto \"http://toto.example.org\";", "@namespace toto \"http://toto.example.org\";");
    minify_test("@namespace toto url(http://toto.example.org);", "@namespace toto \"http://toto.example.org\";");

    test(r#"
      @namespace "http://example.com/foo";

      x {
        color: red;
      }
    "#, indoc! {r#"
      @namespace "http://example.com/foo";

      x {
        color: red;
      }
    "#});

    test(r#"
      @namespace toto "http://toto.example.org";

      toto|x {
        color: red;
      }

      [toto|att=val] {
        color: blue
      }
    "#, indoc! {r#"
      @namespace toto "http://toto.example.org";
      
      toto|x {
        color: red;
      }

      [toto|att="val"] {
        color: #00f;
      }
    "#});

    test(r#"
      @namespace "http://example.com/foo";

      |x {
        color: red;
      }

      [|att=val] {
        color: blue
      }
    "#, indoc! {r#"
      @namespace "http://example.com/foo";
      
      |x {
        color: red;
      }

      [att="val"] {
        color: #00f;
      }
    "#});

    test(r#"
      @namespace "http://example.com/foo";

      *|x {
        color: red;
      }

      [*|att=val] {
        color: blue
      }
    "#, indoc! {r#"
      @namespace "http://example.com/foo";
      
      *|x {
        color: red;
      }

      [*|att="val"] {
        color: #00f;
      }
    "#});

    error_test(".foo { color: red } @namespace \"http://example.com/foo\";", ParserError::UnexpectedNamespaceRule);
  }

  #[test]
  fn test_import() {
    minify_test("@import url(foo.css);", "@import \"foo.css\";");
    minify_test("@import \"foo.css\";", "@import \"foo.css\";");
    minify_test("@import url(foo.css) print;", "@import \"foo.css\" print;");
    minify_test("@import \"foo.css\" print;", "@import \"foo.css\" print;");
    minify_test("@import \"foo.css\" screen and (orientation: landscape);", "@import \"foo.css\" screen and (orientation:landscape);");
    minify_test("@import url(foo.css) supports(display: flex);", "@import \"foo.css\" supports(display: flex);");
    minify_test("@import url(foo.css) supports(display: flex) print;", "@import \"foo.css\" supports(display: flex) print;");
    minify_test("@import url(foo.css) supports(not (display: flex));", "@import \"foo.css\" supports(not (display: flex));");
    minify_test("@import url(foo.css) supports((display: flex));", "@import \"foo.css\" supports(display: flex);");
    minify_test("@charset \"UTF-8\"; @import url(foo.css);", "@import \"foo.css\";");
    minify_test("@layer foo; @import url(foo.css);", "@layer foo;@import \"foo.css\";");
    error_test(".foo { color: red } @import url(bar.css);", ParserError::UnexpectedImportRule);
    error_test("@namespace \"http://example.com/foo\"; @import url(bar.css);", ParserError::UnexpectedImportRule);
    error_test("@media print { .foo { color: red }} @import url(bar.css);", ParserError::UnexpectedImportRule);
    error_test("@layer foo; @import url(foo.css); @layer bar; @import url(bar.css)", ParserError::UnexpectedImportRule);
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
    minify_test(
      ".foo { display: flex; display: var(--grid); }",
      ".foo{display:flex;display:var(--grid)}"
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

    prefix_test(r#"
      .foo {
        text-align: start;
      }
    "#, indoc! {r#"
      .foo {
        text-align: var(--ltr, left) var(--rtl, right);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(2 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        text-align: end;
      }
    "#, indoc! {r#"
      .foo {
        text-align: var(--ltr, right) var(--rtl, left);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(2 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        text-align: start;
      }
    "#, indoc! {r#"
      .foo {
        text-align: start;
      }
    "#
    }, Browsers {
      safari: Some(14 << 16),
      ..Browsers::default()
    });
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
        text-decoration: underline;
        text-decoration-style: var(--style);
      }
    "#, indoc! {r#"
      .foo {
        text-decoration: underline;
        text-decoration-style: var(--style);
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

    prefix_test(r#"
      .foo {
        text-decoration: var(--test);
      }
    "#, indoc! {r#"
      .foo {
        -webkit-text-decoration: var(--test);
        -moz-text-decoration: var(--test);
        text-decoration: var(--test);
      }
    "#},
    Browsers {
      safari: Some(8 << 16),
      firefox: Some(30 << 16),
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

    test(r#"
      .foo {
        text-emphasis: filled red;
        text-emphasis-color: yellow;
      }
    "#, indoc! {r#"
      .foo {
        text-emphasis: filled #ff0;
      }
    "#});

    test(r#"
      .foo {
        text-emphasis: filled yellow;
        text-emphasis-color: var(--color);
      }
    "#, indoc! {r#"
      .foo {
        text-emphasis: filled #ff0;
        text-emphasis-color: var(--color);
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

    prefix_test(r#"
      .foo {
        text-emphasis-position: var(--test);
      }
    "#, indoc! {r#"
      .foo {
        -webkit-text-emphasis-position: var(--test);
        text-emphasis-position: var(--test);
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

    prefix_test(r#"
      .foo {
        inset-inline-start: 2px;
      }
    "#, indoc! {r#"
      .foo {
        left: var(--ltr, 2px);
        right: var(--rtl, 2px);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        inset-inline-start: 2px;
        inset-inline-end: 4px;
      }
    "#, indoc! {r#"
      .foo {
        left: var(--ltr, 2px) var(--rtl, 4px);
        right: var(--ltr, 4px) var(--rtl, 2px);
      }

      [dir="ltr"] {
        --ltr: initial;
        --rtl: ;
      }

      [dir="rtl"] {
        --ltr: ;
        --rtl: initial;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        inset-inline: 2px;
      }
    "#, indoc! {r#"
      .foo {
        left: 2px;
        right: 2px;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        inset-block-start: 2px;
      }
    "#, indoc! {r#"
      .foo {
        top: 2px;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        inset-block-end: 2px;
      }
    "#, indoc! {r#"
      .foo {
        bottom: 2px;
      }
    "#
    }, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        top: 1px;
        left: 2px;
        bottom: 3px;
        right: 4px;
      }
    "#, indoc! {r#"
      .foo {
        top: 1px;
        bottom: 3px;
        left: 2px;
        right: 4px;
      }
    "#}, Browsers {
      safari: Some(8 << 16),
      ..Browsers::default()
    });
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
    test(r#"
      .foo {
        overflow: hidden;
        overflow-y: var(--y);
      }
    "#, indoc! {r#"
      .foo {
        overflow: hidden;
        overflow-y: var(--y);
      }
    "#});
    prefix_test(r#"
      .foo {
        overflow: hidden auto;
      }
    "#, indoc! {r#"
      .foo {
        overflow-x: hidden;
        overflow-y: auto;
      }
    "#},
    Browsers {
      chrome: Some(67 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        overflow: hidden hidden;
      }
    "#, indoc! {r#"
      .foo {
        overflow: hidden;
      }
    "#},
    Browsers {
      chrome: Some(67 << 16),
      ..Browsers::default()
    });
    prefix_test(r#"
      .foo {
        overflow: hidden auto;
      }
    "#, indoc! {r#"
      .foo {
        overflow: hidden auto;
      }
    "#},
    Browsers {
      chrome: Some(68 << 16),
      ..Browsers::default()
    });

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

  #[test]
  fn test_list() {
    minify_test(".foo { list-style-type: disc; }", ".foo{list-style-type:disc}");
    minify_test(".foo { list-style-type: \"★\"; }", ".foo{list-style-type:\"★\"}");
    minify_test(".foo { list-style-type: symbols(cyclic '○' '●'); }", ".foo{list-style-type:symbols(cyclic \"○\" \"●\")}");
    minify_test(".foo { list-style-type: symbols('○' '●'); }", ".foo{list-style-type:symbols(\"○\" \"●\")}");
    minify_test(".foo { list-style-type: symbols(symbolic '○' '●'); }", ".foo{list-style-type:symbols(\"○\" \"●\")}");
    minify_test(".foo { list-style-type: symbols(symbolic url('ellipse.png')); }", ".foo{list-style-type:symbols(url(ellipse.png))}");
    minify_test(".foo { list-style-image: url('ellipse.png'); }", ".foo{list-style-image:url(ellipse.png)}");
    minify_test(".foo { list-style-position: outside; }", ".foo{list-style-position:outside}");
    minify_test(".foo { list-style: \"★\" url(ellipse.png) outside; }", ".foo{list-style:\"★\" url(ellipse.png)}");

    test(r#"
      .foo {
        list-style-type: disc;
        list-style-image: url(ellipse.png);
        list-style-position: outside;
      }
    "#, indoc! {r#"
      .foo {
        list-style: url(ellipse.png);
      }
    "#});

    test(r#"
      .foo {
        list-style: \"★\" url(ellipse.png) outside;
        list-style-image: none;
      }
    "#, indoc! {r#"
      .foo {
        list-style: \"★\";
      }
    "#});

    test(r#"
      .foo {
        list-style: \"★\" url(ellipse.png) outside;
        list-style-image: var(--img);
      }
    "#, indoc! {r#"
      .foo {
        list-style: \"★\" url(ellipse.png);
        list-style-image: var(--img);
      }
    "#});
  }

  #[test]
  fn test_image_set() {
    minify_test(".foo { background: image-set(\"foo.png\" 2x, url(bar.png) 1x) }", ".foo{background:image-set(\"foo.png\" 2x,\"bar.png\")}");
    minify_test(".foo { background: image-set('foo.webp' type('webp'), url(foo.jpg)) }", ".foo{background:image-set(\"foo.webp\" type(\"webp\"),\"foo.jpg\")}");
    minify_test(".foo { background: -webkit-image-set(url(\"foo.png\") 2x, url(bar.png) 1x) }", ".foo{background:-webkit-image-set(url(foo.png) 2x,url(bar.png))}");
  
    test(r#"
      .foo {
        background: -webkit-image-set(url("foo.png") 2x, url(bar.png) 1x);
        background: image-set(url("foo.png") 2x, url(bar.png) 1x);
      }
    "#, indoc! {r#"
      .foo {
        background: -webkit-image-set(url(foo.png) 2x, url(bar.png));
        background: image-set("foo.png" 2x, "bar.png");
      }
    "#});

    prefix_test(r#"
      .foo {
        background: image-set(url("foo.png") 2x, url(bar.png) 1x);
      }
    "#, indoc! {r#"
      .foo {
        background: -webkit-image-set(url(foo.png) 2x, url(bar.png));
        background: image-set("foo.png" 2x, "bar.png");
      }
    "#},
    Browsers {
      chrome: Some(85 << 16),
      firefox: Some(80 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        background: -webkit-image-set(url("foo.png") 2x, url(bar.png) 1x);
        background: image-set(url("foo.png") 2x, url(bar.png) 1x);
      }
    "#, indoc! {r#"
      .foo {
        background: image-set("foo.png" 2x, "bar.png");
      }
    "#},
    Browsers {
      firefox: Some(80 << 16),
      ..Browsers::default()
    });

    prefix_test(r#"
      .foo {
        background: -webkit-image-set(url("foo.png") 2x, url(bar.png) 1x);
      }
    "#, indoc! {r#"
      .foo {
        background: -webkit-image-set(url(foo.png) 2x, url(bar.png));
      }
    "#},
    Browsers {
      chrome: Some(95 << 16),
      ..Browsers::default()
    });
  }

  #[test]
  fn test_color() {
    minify_test(".foo { color: yellow }", ".foo{color:#ff0}");
    minify_test(".foo { color: rgb(255, 255, 0) }", ".foo{color:#ff0}");
    minify_test(".foo { color: rgba(255, 255, 0, 1) }", ".foo{color:#ff0}");
    minify_test(".foo { color: rgba(255, 255, 0, 0.8) }", ".foo{color:#ff0c}");
    minify_test(".foo { color: rgb(128, 128, 128) }", ".foo{color:gray}");
    minify_test(".foo { color: rgb(123, 255, 255) }", ".foo{color:#7bffff}");
    minify_test(".foo { color: rgba(123, 255, 255, 0.5) }", ".foo{color:#7bffff80}");
    minify_test(".foo { color: rgb(123 255 255) }", ".foo{color:#7bffff}");
    minify_test(".foo { color: rgb(123 255 255 / .5) }", ".foo{color:#7bffff80}");
    minify_test(".foo { color: rgb(123 255 255 / 50%) }", ".foo{color:#7bffff80}");
    minify_test(".foo { color: rgb(48% 100% 100% / 50%) }", ".foo{color:#7affff80}");
    minify_test(".foo { color: hsl(100deg, 100%, 50%) }", ".foo{color:#5f0}");
    minify_test(".foo { color: hsl(100, 100%, 50%) }", ".foo{color:#5f0}");
    minify_test(".foo { color: hsl(100 100% 50%) }", ".foo{color:#5f0}");
    minify_test(".foo { color: hsl(100, 100%, 50%, .8) }", ".foo{color:#5f0c}");
    minify_test(".foo { color: hsl(100 100% 50% / .8) }", ".foo{color:#5f0c}");
    minify_test(".foo { color: hsla(100, 100%, 50%, .8) }", ".foo{color:#5f0c}");
    minify_test(".foo { color: hsla(100 100% 50% / .8) }", ".foo{color:#5f0c}");
    minify_test(".foo { color: transparent }", ".foo{color:#0000}");
    minify_test(".foo { color: currentColor }", ".foo{color:currentColor}");
    minify_test(".foo { color: hwb(194 0% 0%) }", ".foo{color:#00c4ff}");
    minify_test(".foo { color: hwb(194 0% 0% / 50%) }", ".foo{color:#00c4ff80}");
    minify_test(".foo { color: hwb(194 0% 50%) }", ".foo{color:#006280}");
    minify_test(".foo { color: hwb(194 50% 0%) }", ".foo{color:#80e1ff}");
    minify_test(".foo { color: hwb(194 50% 50%) }", ".foo{color:gray}");
    // minify_test(".foo { color: ActiveText }", ".foo{color:ActiveTet}");
    minify_test(".foo { color: lab(29.2345% 39.3825 20.0664); }", ".foo{color:lab(29.2345% 39.3825 20.0664)}");
    minify_test(".foo { color: lab(29.2345% 39.3825 20.0664 / 100%); }", ".foo{color:lab(29.2345% 39.3825 20.0664)}");
    minify_test(".foo { color: lab(29.2345% 39.3825 20.0664 / 50%); }", ".foo{color:lab(29.2345% 39.3825 20.0664/.5)}");
    minify_test(".foo { color: lch(29.2345% 44.2 27); }", ".foo{color:lch(29.2345% 44.2 27)}");
    minify_test(".foo { color: lch(29.2345% 44.2 45deg); }", ".foo{color:lch(29.2345% 44.2 45)}");
    minify_test(".foo { color: lch(29.2345% 44.2 .5turn); }", ".foo{color:lch(29.2345% 44.2 180)}");
    minify_test(".foo { color: lch(29.2345% 44.2 27 / 100%); }", ".foo{color:lch(29.2345% 44.2 27)}");
    minify_test(".foo { color: lch(29.2345% 44.2 27 / 50%); }", ".foo{color:lch(29.2345% 44.2 27/.5)}");
    minify_test(".foo { color: oklab(40.101% 0.1147 0.0453); }", ".foo{color:oklab(40.101% .1147 .0453)}");
    minify_test(".foo { color: oklch(40.101% 0.12332 21.555); }", ".foo{color:oklch(40.101% .12332 21.555)}");
    minify_test(".foo { color: oklch(40.101% 0.12332 .5turn); }", ".foo{color:oklch(40.101% .12332 180)}");

    prefix_test(
      ".foo { color: rgba(123, 456, 789, 0.5) }",
      indoc! { r#"
        .foo {
          color: #7bffff80;
        }
      "#},
      Browsers {
        chrome: Some(95 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      ".foo { color: rgba(123, 255, 255, 0.5) }",
      indoc! { r#"
        .foo {
          color: rgba(123, 255, 255, .5);
        }
      "#},
      Browsers {
        ie: Some(11 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      ".foo { color: #7bffff80 }",
      indoc! { r#"
        .foo {
          color: rgba(123, 255, 255, .5);
        }
      "#},
      Browsers {
        ie: Some(11 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      ".foo { color: rgba(123, 456, 789, 0.5) }",
      indoc! { r#"
        .foo {
          color: rgba(123, 255, 255, .5);
        }
      "#},
      Browsers {
        firefox: Some(48 << 16),
        safari: Some(10 << 16),
        ios_saf: Some(9 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      ".foo { color: rgba(123, 456, 789, 0.5) }",
      indoc! { r#"
        .foo {
          color: #7bffff80;
        }
      "#},
      Browsers {
        firefox: Some(49 << 16),
        safari: Some(10 << 16),
        ios_saf: Some(10 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      ".foo { background-color: lab(40% 56.6 39) }",
      indoc! { r#"
        .foo {
          background-color: #b32323;
          background-color: lab(40% 56.6 39);
        }
      "#},
      Browsers {
        chrome: Some(90 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      ".foo { background-color: lch(40% 68.735435 34.568626) }",
      indoc! { r#"
        .foo {
          background-color: #b32323;
          background-color: lch(40% 68.7354 34.5686);
        }
      "#},
      Browsers {
        chrome: Some(90 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      ".foo { background-color: oklab(40% 0.001236 0.0039) }",
      indoc! { r#"
        .foo {
          background-color: #494745;
          background-color: oklab(40% .001236 .0039);
        }
      "#},
      Browsers {
        chrome: Some(90 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      ".foo { background-color: oklch(40% 0.1268735435 34.568626) }",
      indoc! { r#"
        .foo {
          background-color: #7e250f;
          background-color: oklch(40% .126874 34.5686);
        }
      "#},
      Browsers {
        chrome: Some(90 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      ".foo { background-color: lab(40% 56.6 39) }",
      indoc! { r#"
        .foo {
          background-color: lab(40% 56.6 39);
        }
      "#},
      Browsers {
        safari: Some(15 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      ".foo { background-color: oklab(40% 0.001236 0.0039) }",
      indoc! { r#"
        .foo {
          background-color: #494745;
          background-color: lab(30.4045% .415295 1.4957);
          background-color: oklab(40% .001236 .0039);
        }
      "#},
      Browsers {
        chrome: Some(90 << 16),
        safari: Some(15 << 16),
        ..Browsers::default()
      }
    );

    prefix_test(
      ".foo { background-color: oklch(40% 0.1268735435 34.568626) }",
      indoc! { r#"
        .foo {
          background-color: #7e250f;
          background-color: lab(29.2661% 38.2437 35.3889);
          background-color: oklch(40% .126874 34.5686);
        }
      "#},
      Browsers {
        chrome: Some(90 << 16),
        safari: Some(15 << 16),
        ..Browsers::default()
      }
    );
  }

  #[cfg(feature = "grid")]
  #[test]
  fn test_grid() {
    minify_test(".foo { grid-template-columns: [first nav-start]  150px [main-start] 1fr [last]; }", ".foo{grid-template-columns:[first nav-start]150px[main-start]1fr[last]}");
    minify_test(".foo { grid-template-columns: 150px 1fr; }", ".foo{grid-template-columns:150px 1fr}");
    minify_test(".foo { grid-template-columns: repeat(4, 1fr); }", ".foo{grid-template-columns:repeat(4,1fr)}");
    minify_test(".foo { grid-template-columns: repeat(2, [e] 40px); }", ".foo{grid-template-columns:repeat(2,[e]40px)}");
    minify_test(".foo { grid-template-columns: repeat(4, [col-start] 250px [col-end]); }", ".foo{grid-template-columns:repeat(4,[col-start]250px[col-end])}");
    minify_test(".foo { grid-template-columns: repeat(4, [col-start] 60% [col-end]); }", ".foo{grid-template-columns:repeat(4,[col-start]60%[col-end])}");
    minify_test(".foo { grid-template-columns: repeat(4, [col-start] 1fr [col-end]); }", ".foo{grid-template-columns:repeat(4,[col-start]1fr[col-end])}");
    minify_test(".foo { grid-template-columns: repeat(4, [col-start] min-content [col-end]); }", ".foo{grid-template-columns:repeat(4,[col-start]min-content[col-end])}");
    minify_test(".foo { grid-template-columns: repeat(4, [col-start] max-content [col-end]); }", ".foo{grid-template-columns:repeat(4,[col-start]max-content[col-end])}");
    minify_test(".foo { grid-template-columns: repeat(4, [col-start] auto [col-end]); }", ".foo{grid-template-columns:repeat(4,[col-start]auto[col-end])}");
    minify_test(".foo { grid-template-columns: repeat(4, [col-start] minmax(100px, 1fr) [col-end]); }", ".foo{grid-template-columns:repeat(4,[col-start]minmax(100px,1fr)[col-end])}");
    minify_test(".foo { grid-template-columns: repeat(4, [col-start] fit-content(200px) [col-end]); }", ".foo{grid-template-columns:repeat(4,[col-start]fit-content(200px)[col-end])}");
    minify_test(".foo { grid-template-columns: repeat(4, 10px [col-start] 30% [col-middle] auto [col-end]); }", ".foo{grid-template-columns:repeat(4,10px[col-start]30%[col-middle]auto[col-end])}");
    minify_test(".foo { grid-template-columns: repeat(5, auto); }", ".foo{grid-template-columns:repeat(5,auto)}");
    minify_test(".foo { grid-template-columns: repeat(auto-fill, 250px); }", ".foo{grid-template-columns:repeat(auto-fill,250px)}");
    minify_test(".foo { grid-template-columns: repeat(auto-fit, 250px); }", ".foo{grid-template-columns:repeat(auto-fit,250px)}");
    minify_test(".foo { grid-template-columns: repeat(auto-fill, [col-start] 250px [col-end]); }", ".foo{grid-template-columns:repeat(auto-fill,[col-start]250px[col-end])}");
    minify_test(".foo { grid-template-columns: repeat(auto-fill, [col-start] minmax(100px, 1fr) [col-end]); }", ".foo{grid-template-columns:repeat(auto-fill,[col-start]minmax(100px,1fr)[col-end])}");
    minify_test(".foo { grid-template-columns: minmax(min-content, 1fr); }", ".foo{grid-template-columns:minmax(min-content,1fr)}");
    minify_test(".foo { grid-template-columns: 200px repeat(auto-fill, 100px) 300px; }", ".foo{grid-template-columns:200px repeat(auto-fill,100px) 300px}");
    minify_test(".foo { grid-template-columns: [linename1 linename2] 100px repeat(auto-fit, [linename1] 300px) [linename3]; }", ".foo{grid-template-columns:[linename1 linename2]100px repeat(auto-fit,[linename1]300px)[linename3]}");
    minify_test(".foo { grid-template-rows: [linename1 linename2] 100px repeat(auto-fit, [linename1] 300px) [linename3]; }", ".foo{grid-template-rows:[linename1 linename2]100px repeat(auto-fit,[linename1]300px)[linename3]}");

    minify_test(".foo { grid-auto-rows: auto; }", ".foo{grid-auto-rows:auto}");
    minify_test(".foo { grid-auto-rows: 1fr; }", ".foo{grid-auto-rows:1fr}");
    minify_test(".foo { grid-auto-rows: 100px; }", ".foo{grid-auto-rows:100px}");
    minify_test(".foo { grid-auto-rows: min-content; }", ".foo{grid-auto-rows:min-content}");
    minify_test(".foo { grid-auto-rows: max-content; }", ".foo{grid-auto-rows:max-content}");
    minify_test(".foo { grid-auto-rows: minmax(100px,auto); }", ".foo{grid-auto-rows:minmax(100px,auto)}");
    minify_test(".foo { grid-auto-rows: fit-content(20%); }", ".foo{grid-auto-rows:fit-content(20%)}");
    minify_test(".foo { grid-auto-rows: 100px minmax(100px, auto) 10% 0.5fr fit-content(400px); }", ".foo{grid-auto-rows:100px minmax(100px,auto) 10% .5fr fit-content(400px)}");
    minify_test(".foo { grid-auto-columns: 100px minmax(100px, auto) 10% 0.5fr fit-content(400px); }", ".foo{grid-auto-columns:100px minmax(100px,auto) 10% .5fr fit-content(400px)}");
  
    minify_test(r#"
      .foo {
        grid-template-areas: "head head"
                             "nav  main"
                             "foot ....";
      }
    "#, ".foo{grid-template-areas:\"head head\"\"nav main\"\"foot.\"}");
    minify_test(r#"
      .foo {
        grid-template-areas: "head head"
                             "nav  main"
                             ".... foot";
      }
    "#, ".foo{grid-template-areas:\"head head\"\"nav main\"\".foot\"}");
    minify_test(r#"
      .foo {
        grid-template-areas: "head head"
                             "nav  main"
                             ".... ....";
      }
    "#, ".foo{grid-template-areas:\"head head\"\"nav main\"\". .\"}");

    test(r#"
      .foo {
        grid-template-areas: "head head" "nav  main" "foot ....";
      }
    "#, indoc! { r#"
      .foo {
        grid-template-areas: "head head"
                             "nav main"
                             "foot .";
      }
    "#});

    minify_test(r#"
      .foo {
        grid-template: [header-top] "a   a   a"     [header-bottom]
                       [main-top] "b   b   b" 1fr [main-bottom];
      }
    "#, ".foo{grid-template:[header-top]\"a a a\"[header-bottom main-top]\"b b b\"1fr[main-bottom]}");
    minify_test(r#"
      .foo {
        grid-template: "head head"
                       "nav  main" 1fr
                       "foot ....";
      }
    "#, ".foo{grid-template:\"head head\"\"nav main\"1fr\"foot.\"}");
    minify_test(r#"
      .foo {
        grid-template: [header-top] "a   a   a"     [header-bottom]
                         [main-top] "b   b   b" 1fr [main-bottom]
                                  / auto 1fr auto;
      }
    "#, ".foo{grid-template:[header-top]\"a a a\"[header-bottom main-top]\"b b b\"1fr[main-bottom]/auto 1fr auto}");

    minify_test(".foo { grid-template: auto 1fr / auto 1fr auto; }", ".foo{grid-template:auto 1fr/auto 1fr auto}");
    minify_test(
      ".foo { grid-template: [linename1 linename2] 100px repeat(auto-fit, [linename1] 300px) [linename3] / [linename1 linename2] 100px repeat(auto-fit, [linename1] 300px) [linename3]; }",
      ".foo{grid-template:[linename1 linename2]100px repeat(auto-fit,[linename1]300px)[linename3]/[linename1 linename2]100px repeat(auto-fit,[linename1]300px)[linename3]}"
    );

    test(
      ".foo{grid-template:[header-top]\"a a a\"[header-bottom main-top]\"b b b\"1fr[main-bottom]/auto 1fr auto}",
      indoc!{r#"
        .foo {
          grid-template: [header-top] "a a a" [header-bottom]
                         [main-top] "b b b" 1fr [main-bottom]
                         / auto 1fr auto;
        }
      "#}
    );
    test(
      ".foo{grid-template:[header-top]\"a a a\"[main-top]\"b b b\"1fr/auto 1fr auto}",
      indoc!{r#"
        .foo {
          grid-template: [header-top] "a a a"
                         [main-top] "b b b" 1fr
                         / auto 1fr auto;
        }
      "#}
    );

    minify_test(".foo { grid-auto-flow: row }", ".foo{grid-auto-flow:row}");
    minify_test(".foo { grid-auto-flow: column }", ".foo{grid-auto-flow:column}");
    minify_test(".foo { grid-auto-flow: row dense }", ".foo{grid-auto-flow:dense}");
    minify_test(".foo { grid-auto-flow: dense row }", ".foo{grid-auto-flow:dense}");
    minify_test(".foo { grid-auto-flow: column dense }", ".foo{grid-auto-flow:column dense}");
    minify_test(".foo { grid-auto-flow: dense column }", ".foo{grid-auto-flow:column dense}");

    minify_test(".foo { grid: none }", ".foo{grid:none}");
    minify_test(".foo { grid: \"a\" 100px \"b\" 1fr }", ".foo{grid:\"a\"100px\"b\"1fr}");
    minify_test(".foo { grid: [linename1] \"a\" 100px [linename2] }", ".foo{grid:[linename1]\"a\"100px[linename2]}");
    minify_test(".foo { grid: \"a\" 200px \"b\" min-content }", ".foo{grid:\"a\"200px\"b\"min-content}");
    minify_test(".foo { grid: \"a\" minmax(100px, max-content) \"b\" 20% }", ".foo{grid:\"a\"minmax(100px,max-content)\"b\"20%}");
    minify_test(".foo { grid: 100px / 200px }", ".foo{grid:100px/200px}");
    minify_test(".foo { grid: minmax(400px, min-content) / repeat(auto-fill, 50px) }", ".foo{grid:minmax(400px,min-content)/repeat(auto-fill,50px)}");
    
    minify_test(".foo { grid: 200px / auto-flow }", ".foo{grid:200px/auto-flow}");
    minify_test(".foo { grid: 30% / auto-flow dense }", ".foo{grid:30%/auto-flow dense}");
    minify_test(".foo { grid: 30% / dense auto-flow }", ".foo{grid:30%/auto-flow dense}");
    minify_test(".foo { grid: repeat(3, [line1 line2 line3] 200px) / auto-flow 300px }", ".foo{grid:repeat(3,[line1 line2 line3]200px)/auto-flow 300px}");
    minify_test(".foo { grid: [line1] minmax(20em, max-content) / auto-flow dense 40% }", ".foo{grid:[line1]minmax(20em,max-content)/auto-flow dense 40%}");
    minify_test(".foo { grid: none / auto-flow 1fr }", ".foo{grid:none/auto-flow 1fr}");

    minify_test(".foo { grid: auto-flow / 200px }", ".foo{grid:none/200px}");
    minify_test(".foo { grid: auto-flow dense / 30% }", ".foo{grid:auto-flow dense/30%}");
    minify_test(".foo { grid: dense auto-flow / 30% }", ".foo{grid:auto-flow dense/30%}");
    minify_test(".foo { grid: auto-flow 300px / repeat(3, [line1 line2 line3] 200px) }", ".foo{grid:auto-flow 300px/repeat(3,[line1 line2 line3]200px)}");
    minify_test(".foo { grid: auto-flow dense 40% / [line1] minmax(20em, max-content) }", ".foo{grid:auto-flow dense 40%/[line1]minmax(20em,max-content)}");

    minify_test(".foo { grid-row-start: auto }", ".foo{grid-row-start:auto}");
    minify_test(".foo { grid-row-start: some-area }", ".foo{grid-row-start:some-area}");
    minify_test(".foo { grid-row-start: 2 }", ".foo{grid-row-start:2}");
    minify_test(".foo { grid-row-start: 2 some-line }", ".foo{grid-row-start:2 some-line}");
    minify_test(".foo { grid-row-start: some-line 2 }", ".foo{grid-row-start:2 some-line}");
    minify_test(".foo { grid-row-start: span 3 }", ".foo{grid-row-start:span 3}");
    minify_test(".foo { grid-row-start: span some-line }", ".foo{grid-row-start:span some-line}");
    minify_test(".foo { grid-row-start: span some-line 1 }", ".foo{grid-row-start:span some-line}");
    minify_test(".foo { grid-row-start: span 1 some-line }", ".foo{grid-row-start:span some-line}");
    minify_test(".foo { grid-row-start: span 5 some-line }", ".foo{grid-row-start:span 5 some-line}");
    minify_test(".foo { grid-row-start: span some-line 5 }", ".foo{grid-row-start:span 5 some-line}");

    minify_test(".foo { grid-row-end: span 1 some-line }", ".foo{grid-row-end:span some-line}");
    minify_test(".foo { grid-column-start: span 1 some-line }", ".foo{grid-column-start:span some-line}");
    minify_test(".foo { grid-column-end: span 1 some-line }", ".foo{grid-column-end:span some-line}");

    minify_test(".foo { grid-row: 1 }", ".foo{grid-row:1}");
    minify_test(".foo { grid-row: 1 / auto }", ".foo{grid-row:1}");
    minify_test(".foo { grid-row: 1 / 1 }", ".foo{grid-row:1/1}");
    minify_test(".foo { grid-row: 1 / 3 }", ".foo{grid-row:1/3}");
    minify_test(".foo { grid-row: 1 / span 2 }", ".foo{grid-row:1/span 2}");
    minify_test(".foo { grid-row: main-start }", ".foo{grid-row:main-start}");
    minify_test(".foo { grid-row: main-start / main-end }", ".foo{grid-row:main-start/main-end}");
    minify_test(".foo { grid-row: main-start / main-start }", ".foo{grid-row:main-start}");
    minify_test(".foo { grid-column: 1 / auto }", ".foo{grid-column:1}");

    minify_test(".foo { grid-area: a }", ".foo{grid-area:a}");
    minify_test(".foo { grid-area: a / a / a / a }", ".foo{grid-area:a}");
    minify_test(".foo { grid-area: a / b / a / b }", ".foo{grid-area:a/b}");
    minify_test(".foo { grid-area: a / b / c / b }", ".foo{grid-area:a/b/c}");
    minify_test(".foo { grid-area: a / b / c / d }", ".foo{grid-area:a/b/c/d}");

    minify_test(".foo { grid-area: auto / auto / auto / auto }", ".foo{grid-area:auto}");
    minify_test(".foo { grid-area: 1 / auto }", ".foo{grid-area:1}");
    minify_test(".foo { grid-area: 1 / 2 / 3 / 4 }", ".foo{grid-area:1/2/3/4}");
    minify_test(".foo { grid-area: 1 / 1 / 1 / 1 }", ".foo{grid-area:1/1/1/1}");

    test(
      r#"
        .foo{
          grid-template-rows: auto 1fr;
          grid-template-columns: auto 1fr auto;
          grid-template-areas: none;
        }
      "#,
      indoc!{r#"
        .foo {
          grid-template: auto 1fr / auto 1fr auto;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-areas: "a a a"
                               "b b b";
          grid-template-rows: [header-top] auto [header-bottom main-top] 1fr [main-bottom];
          grid-template-columns: auto 1fr auto;
        }
      "#,
      indoc!{r#"
        .foo {
          grid-template: [header-top] "a a a" [header-bottom]
                         [main-top] "b b b" 1fr [main-bottom]
                         / auto 1fr auto;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-areas: "a a a"
                               "b b b";
          grid-template-columns: repeat(3, 1fr);
          grid-template-rows: auto 1fr;
        }
      "#,
      indoc!{r#"
        .foo {
          grid-template-rows: auto 1fr;
          grid-template-columns: repeat(3, 1fr);
          grid-template-areas: "a a a"
                               "b b b";
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-areas: "a a a"
                               "b b b";
          grid-template-columns: auto 1fr auto;
          grid-template-rows: repeat(2, 1fr);
        }
      "#,
      indoc!{r#"
        .foo {
          grid-template-rows: repeat(2, 1fr);
          grid-template-columns: auto 1fr auto;
          grid-template-areas: "a a a"
                               "b b b";
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-areas: ". a a ."
                               ". b b .";
          grid-template-rows: auto 1fr;
          grid-template-columns: 10px 1fr 1fr 10px;
        }
      "#,
      indoc!{r#"
        .foo {
          grid-template: ". a a ."
                         ". b b ." 1fr
                         / 10px 1fr 1fr 10px;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-areas: none;
          grid-template-columns: auto 1fr auto;
          grid-template-rows: repeat(2, 1fr);
        }
      "#,
      indoc!{r#"
        .foo {
          grid-template: repeat(2, 1fr) / auto 1fr auto;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-areas: none;
          grid-template-columns: none;
          grid-template-rows: none;
        }
      "#,
      indoc!{r#"
        .foo {
          grid-template: none;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-areas: "a a a"
                               "b b b";
          grid-template-rows: [header-top] auto [header-bottom main-top] 1fr [main-bottom];
          grid-template-columns: auto 1fr auto;
          grid-auto-flow: row;
          grid-auto-rows: auto;
          grid-auto-columns: auto;
        }
      "#,
      indoc!{r#"
        .foo {
          grid: [header-top] "a a a" [header-bottom]
                [main-top] "b b b" 1fr [main-bottom]
                / auto 1fr auto;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-areas: none;
          grid-template-columns: auto 1fr auto;
          grid-template-rows: repeat(2, 1fr);
          grid-auto-flow: row;
          grid-auto-rows: auto;
          grid-auto-columns: auto;
        }
      "#,
      indoc!{r#"
        .foo {
          grid: repeat(2, 1fr) / auto 1fr auto;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-areas: none;
          grid-template-columns: none;
          grid-template-rows: none;
          grid-auto-flow: row;
          grid-auto-rows: auto;
          grid-auto-columns: auto;
        }
      "#,
      indoc!{r#"
        .foo {
          grid: none;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-areas: "a a a"
                               "b b b";
          grid-template-rows: [header-top] auto [header-bottom main-top] 1fr [main-bottom];
          grid-template-columns: auto 1fr auto;
          grid-auto-flow: column;
          grid-auto-rows: 1fr;
          grid-auto-columns: 1fr;
        }
      "#,
      indoc!{r#"
        .foo {
          grid-template: [header-top] "a a a" [header-bottom]
                         [main-top] "b b b" 1fr [main-bottom]
                         / auto 1fr auto;
          grid-auto-rows: 1fr;
          grid-auto-columns: 1fr;
          grid-auto-flow: column;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-rows: auto 1fr;
          grid-template-columns: auto 1fr auto;
          grid-template-areas: none;
          grid-auto-flow: row;
          grid-auto-rows: auto;
          grid-auto-columns: auto;
        }
      "#,
      indoc!{r#"
        .foo {
          grid: auto 1fr / auto 1fr auto;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-rows: auto 1fr;
          grid-template-columns: auto 1fr auto;
          grid-template-areas: none;
          grid-auto-flow: column;
          grid-auto-rows: 1fr;
          grid-auto-columns: 1fr;
        }
      "#,
      indoc!{r#"
        .foo {
          grid-template: auto 1fr / auto 1fr auto;
          grid-auto-rows: 1fr;
          grid-auto-columns: 1fr;
          grid-auto-flow: column;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-rows: none;
          grid-template-columns: auto 1fr auto;
          grid-template-areas: none;
          grid-auto-flow: column;
          grid-auto-rows: 1fr;
          grid-auto-columns: 1fr;
        }
      "#,
      indoc!{r#"
        .foo {
          grid-template: none / auto 1fr auto;
          grid-auto-rows: 1fr;
          grid-auto-columns: 1fr;
          grid-auto-flow: column;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-rows: none;
          grid-template-columns: auto 1fr auto;
          grid-template-areas: none;
          grid-auto-flow: row;
          grid-auto-rows: 1fr;
          grid-auto-columns: auto;
        }
      "#,
      indoc!{r#"
        .foo {
          grid: auto-flow 1fr / auto 1fr auto;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-rows: none;
          grid-template-columns: auto 1fr auto;
          grid-template-areas: none;
          grid-auto-flow: row dense;
          grid-auto-rows: 1fr;
          grid-auto-columns: auto;
        }
      "#,
      indoc!{r#"
        .foo {
          grid: auto-flow dense 1fr / auto 1fr auto;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-rows: auto 1fr auto;
          grid-template-columns: none;
          grid-template-areas: none;
          grid-auto-flow: column;
          grid-auto-rows: auto;
          grid-auto-columns: 1fr;
        }
      "#,
      indoc!{r#"
        .foo {
          grid: auto 1fr auto / auto-flow 1fr;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-rows: auto 1fr auto;
          grid-template-columns: none;
          grid-template-areas: none;
          grid-auto-flow: column dense;
          grid-auto-rows: auto;
          grid-auto-columns: 1fr;
        }
      "#,
      indoc!{r#"
        .foo {
          grid: auto 1fr auto / auto-flow dense 1fr;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-template-rows: auto 1fr auto;
          grid-template-columns: none;
          grid-template-areas: none;
          grid-auto-flow: var(--auto-flow);
          grid-auto-rows: auto;
          grid-auto-columns: 1fr;
        }
      "#,
      indoc!{r#"
        .foo {
          grid-template: auto 1fr auto / none;
          grid-auto-flow: var(--auto-flow);
          grid-auto-rows: auto;
          grid-auto-columns: 1fr;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid: auto 1fr auto / auto-flow dense 1fr;
          grid-template-rows: 1fr 1fr 1fr;
        }
      "#,
      indoc!{r#"
        .foo {
          grid: 1fr 1fr 1fr / auto-flow dense 1fr;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-row-start: a;
          grid-row-end: a;
          grid-column-start: a;
          grid-column-end: a;
        }
      "#,
      indoc!{r#"
        .foo {
          grid-area: a;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-row-start: 1;
          grid-row-end: 2;
          grid-column-start: 3;
          grid-column-end: 4;
        }
      "#,
      indoc!{r#"
        .foo {
          grid-area: 1 / 3 / 2 / 4;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-row-start: a;
          grid-row-end: a;
        }
      "#,
      indoc!{r#"
        .foo {
          grid-row: a;
        }
      "#}
    );

    test(
      r#"
        .foo{
          grid-column-start: a;
          grid-column-end: a;
        }
      "#,
      indoc!{r#"
        .foo {
          grid-column: a;
        }
      "#}
    );
  }

  #[test]
  fn test_moz_document() {
    minify_test(r#"
      @-moz-document url-prefix() {
        h1 {
          color: yellow;
        }
      }
    "#, "@-moz-document url-prefix(){h1{color:#ff0}}");
  }

  #[test]
  fn test_custom_properties() {
    minify_test(".foo { --test: ; }", ".foo{--test: }");
    minify_test(".foo { --test:  ; }", ".foo{--test: }");
    minify_test(".foo { --test: foo; }", ".foo{--test:foo}");
    minify_test(".foo { --test:  foo; }", ".foo{--test:foo}");
    minify_test(".foo { --test: foo ; }", ".foo{--test:foo}");
    minify_test(".foo { --test: foo  ; }", ".foo{--test:foo}");
    minify_test(".foo { --test:foo; }", ".foo{--test:foo}");
    minify_test(".foo { --test:foo ; }", ".foo{--test:foo}");
    minify_test(".foo { --test: var(--foo, 20px); }", ".foo{--test:var(--foo,20px)}");
    minify_test(".foo { transition: var(--foo, 20px),\nvar(--bar, 40px); }", ".foo{transition:var(--foo,20px),var(--bar,40px)}");
    minify_test(".foo { background: var(--color) var(--image); }", ".foo{background:var(--color)var(--image)}");
    minify_test(".foo { height: calc(var(--spectrum-global-dimension-size-300) / 2);", ".foo{height:calc(var(--spectrum-global-dimension-size-300)/2)}");
    minify_test(".foo { color: var(--color, rgb(255, 255, 0)); }", ".foo{color:var(--color,#ff0)}");
    minify_test(".foo { color: var(--color, #ffff00); }", ".foo{color:var(--color,#ff0)}");
    minify_test(".foo { color: var(--color, rgb(var(--red), var(--green), 0)); }", ".foo{color:var(--color,rgb(var(--red),var(--green),0))}");
    minify_test(".foo { --test: .5s; }", ".foo{--test:.5s}");
  }

  #[test]
  fn test_charset() {
    test(r#"
      @charset "UTF-8";

      .foo {
        color: red;
      }

      @charset "UTF-8";

      .bar {
        color: yellow;
      }
    "#, indoc! { r#"
      .foo {
        color: red;
      }

      .bar {
        color: #ff0;
      }
    "#})
  }

  #[test]
  fn test_style_attr() {
    attr_test("color: yellow; flex: 1 1 auto", "color: #ff0; flex: auto", false, None);
    attr_test("color: yellow; flex: 1 1 auto", "color:#ff0;flex:auto", true, None);
    attr_test("border-inline-start: 2px solid red", "border-inline-start: 2px solid red", false, Some(Browsers { safari: Some(12 << 16), ..Browsers::default() }));
  }

  #[test]
  fn test_nesting() {
    nesting_test(
      r#"
        .foo {
          color: blue;
          & > .bar { color: red; }
        }
      "#,
      indoc!{r#"
        .foo {
          color: #00f;
        }

        .foo > .bar {
          color: red;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          color: blue;
          &.bar { color: red; }
        }
      "#,
      indoc!{r#"
        .foo {
          color: #00f;
        }

        .foo.bar {
          color: red;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo, .bar {
          color: blue;
          & + .baz, &.qux { color: red; }
        }
      "#,
      indoc!{r#"
        .foo, .bar {
          color: #00f;
        }

        :is(.foo, .bar) + .baz, :is(.foo, .bar).qux {
          color: red;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          color: blue;
          & .bar & .baz & .qux { color: red; }
        }
      "#,
      indoc!{r#"
        .foo {
          color: #00f;
        }

        .foo .bar .foo .baz .foo .qux {
          color: red;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          color: blue;
          & { padding: 2ch; }
        }
      "#,
      indoc!{r#"
        .foo {
          color: #00f;
        }

        .foo {
          padding: 2ch;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          color: blue;
          && { padding: 2ch; }
        }
      "#,
      indoc!{r#"
        .foo {
          color: #00f;
        }

        .foo.foo {
          padding: 2ch;
        }
      "#}
    );

    nesting_test(
      r#"
        .error, .invalid {
          &:hover > .baz { color: red; }
        }
      "#,
      indoc!{r#"
        :is(.error, .invalid):hover > .baz {
          color: red;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          &:is(.bar, &.baz) { color: red; }
        }
      "#,
      indoc!{r#"
        .foo:is(.bar, .foo.baz) {
          color: red;
        }
      "#}
    );

    nesting_test(
      r#"
        figure {
          margin: 0;
        
          & > figcaption {
            background: hsl(0 0% 0% / 50%);
        
            & > p {
              font-size: .9rem;
            }
          }
        }
      "#,
      indoc!{r#"
        figure {
          margin: 0;
        }

        figure > figcaption {
          background: #00000080;
        }

        figure > figcaption > p {
          font-size: .9rem;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          color: red;
          @nest & > .bar {
            color: blue;
          }
        }
      "#,
      indoc!{r#"
        .foo {
          color: red;
        }

        .foo > .bar {
          color: #00f;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          color: red;
          @nest .parent & {
            color: blue;
          }
        }
      "#,
      indoc!{r#"
        .foo {
          color: red;
        }

        .parent .foo {
          color: #00f;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          color: red;
          @nest :not(&) {
            color: blue;
          }
        }
      "#,
      indoc!{r#"
        .foo {
          color: red;
        }

        :not(.foo) {
          color: #00f;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          color: blue;
          @nest .bar & {
            color: red;
            &.baz {
              color: green;
            }
          }
        }
      "#,
      indoc!{r#"
        .foo {
          color: #00f;
        }

        .bar .foo {
          color: red;
        }

        .bar .foo.baz {
          color: green;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          display: grid;
        
          @media (orientation: landscape) {
            grid-auto-flow: column;
          }
        }
      "#,
      indoc!{r#"
        .foo {
          display: grid;
        }

        @media (orientation: landscape) {
          .foo {
            grid-auto-flow: column;
          }
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          display: grid;
        
          @media (orientation: landscape) {
            grid-auto-flow: column;
        
            @media (width > 1024px) {
              max-inline-size: 1024px;
            }
          }
        }
      "#,
      indoc!{r#"
        .foo {
          display: grid;
        }

        @media (orientation: landscape) {
          .foo {
            grid-auto-flow: column;
          }

          @media (min-width: 1024px) {
            .foo {
              max-inline-size: 1024px;
            }
          }
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          display: grid;
        
          @supports (foo: bar) {
            grid-auto-flow: column;
          }
        }
      "#,
      indoc!{r#"
        .foo {
          display: grid;
        }

        @supports (foo: bar) {
          .foo {
            grid-auto-flow: column;
          }
        }
      "#}
    );

    nesting_test(
      r#"
        @namespace "http://example.com/foo";
        @namespace toto "http://toto.example.org";

        .foo {
          &div {
            color: red;
          }

          &* {
            color: green;
          }

          &|x {
            color: red;
          }

          &*|x {
            color: green;
          }

          &toto|x {
            color: red;
          }
        }
      "#,
      indoc!{r#"
        @namespace "http://example.com/foo";
        @namespace toto "http://toto.example.org";

        div.foo {
          color: red;
        }

        *.foo {
          color: green;
        }

        |x.foo {
          color: red;
        }

        *|x.foo {
          color: green;
        }

        toto|x.foo {
          color: red;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          &article > figure {
            color: red;
          }
        }
      "#,
      indoc!{r#"
        article.foo > figure {
          color: red;
        }
      "#}
    );

    nesting_test(
      r#"
        @namespace "http://example.com/foo";
        @namespace toto "http://toto.example.org";

        div {
          @nest .foo& {
            color: red;
          }
        }

        * {
          @nest .foo& {
            color: red;
          }
        }

        |x {
          @nest .foo& {
            color: red;
          }
        }

        *|x {
          @nest .foo& {
            color: red;
          }
        }

        toto|x {
          @nest .foo& {
            color: red;
          }
        }
      "#,
      indoc!{r#"
        @namespace "http://example.com/foo";
        @namespace toto "http://toto.example.org";

        .foo:is(div) {
          color: red;
        }

        .foo:is(*) {
          color: red;
        }

        .foo:is(|x) {
          color: red;
        }

        .foo:is(*|x) {
          color: red;
        }

        .foo:is(toto|x) {
          color: red;
        }
      "#}
    );

    nesting_test(
      r#"
        div {
          &.bar {
            background: green;
          }
        }
      "#,
      indoc!{r#"
        div.bar {
          background: green;
        }
      "#}
    );

    nesting_test(
      r#"
        div > .foo {
          &span {
            background: green;
          }
        }
      "#,
      indoc!{r#"
        span:is(div > .foo) {
          background: green;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          & h1 {
            background: green;
          }
        }
      "#,
      indoc!{r#"
        .foo h1 {
          background: green;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          @nest :not(&) {
            color: red;
          }

          & h1 {
            background: green;
          }
        }
      "#,
      indoc!{r#"
        :not(.foo) {
          color: red;
        }

        .foo h1 {
          background: green;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo {
          & h1 {
            background: green;
          }

          @nest :not(&) {
            color: red;
          }
        }
      "#,
      indoc!{r#"
        .foo h1 {
          background: green;
        }

        :not(.foo) {
          color: red;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo .bar {
          &h1 {
            background: green;
          }
        }
      "#,
      indoc!{r#"
        h1:is(.foo .bar) {
          background: green;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo .bar {
          @nest h1& {
            background: green;
          }
        }
      "#,
      indoc!{r#"
        h1:is(.foo .bar) {
          background: green;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo.bar {
          &h1 {
            background: green;
          }
        }
      "#,
      indoc!{r#"
        h1.foo.bar {
          background: green;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo .bar {
          &h1 .baz {
            background: green;
          }
        }
      "#,
      indoc!{r#"
        h1:is(.foo .bar) .baz {
          background: green;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo .bar {
          @nest h1 .baz& {
            background: green;
          }
        }
      "#,
      indoc!{r#"
        h1 .baz:is(.foo .bar) {
          background: green;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo .bar {
          &.baz {
            background: green;
          }
        }
      "#,
      indoc!{r#"
        .foo .bar.baz {
          background: green;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo .bar {
          @nest .baz& {
            background: green;
          }
        }
      "#,
      indoc!{r#"
        .baz:is(.foo .bar) {
          background: green;
        }
      "#}
    );

    nesting_test(
      r#"
        .foo .bar {
          @nest .baz & {
            background: green;
          }
        }
      "#,
      indoc!{r#"
        .baz .foo .bar {
          background: green;
        }
      "#}
    );

    nesting_test_no_targets(
      r#"
        .foo {
          color: blue;
          @nest .bar & {
            color: red;
            &.baz {
              color: green;
            }
          }
        }
      "#,
      indoc!{r#"
        .foo {
          color: #00f;

          @nest .bar & {
            color: red;

            &.baz {
              color: green;
            }
          }
        }
      "#}
    );

    nesting_test_no_targets(
      r#"
        .foo {
          color: blue;
          &div {
            color: red;
          }

          &span {
            color: purple;
          }
        }
      "#,
      indoc!{r#"
        .foo {
          color: #00f;

          &div {
            color: red;
          }

          &span {
            color: purple;
          }
        }
      "#}
    );

    nesting_test_no_targets(
      r#"
        .error, .invalid {
          &:hover > .baz { color: red; }
        }
      "#,
      indoc!{r#"
        .error, .invalid {
          &:hover > .baz {
            color: red;
          }
        }
      "#}
    );
  }

  #[test]
  fn test_css_modules() {
    css_modules_test(r#"
      .foo {
        color: red;
      }
      
      #id {
        animation: 2s test;
      }

      @keyframes test {
        from { color: red }
        to { color: yellow }
      }

      @counter-style circles {
        symbols: Ⓐ Ⓑ Ⓒ;
      }

      ul {
        list-style: circles;
      }

      ol {
        list-style-type: none;
      }

      @keyframes fade {
        from { opacity: 0 }
        to { opacity: 1 }
      }
    "#, indoc!{r#"
      .foo_EgL3uq {
        color: red;
      }

      #id_EgL3uq {
        animation: test_EgL3uq 2s;
      }

      @keyframes test_EgL3uq {
        from {
          color: red;
        }

        to {
          color: #ff0;
        }
      }

      @counter-style circles_EgL3uq {
        symbols: Ⓐ Ⓑ Ⓒ;
      }

      ul {
        list-style: circles_EgL3uq;
      }

      ol {
        list-style-type: none;
      }

      @keyframes fade_EgL3uq {
        from {
          opacity: 0;
        }

        to {
          opacity: 1;
        }
      }
    "#}, map! {
      "foo" => "foo_EgL3uq",
      "id" => "id_EgL3uq",
      "test" => "test_EgL3uq" referenced: true,
      "circles" => "circles_EgL3uq" referenced: true,
      "fade" => "fade_EgL3uq"
    });

    #[cfg(feature = "grid")]
    css_modules_test(r#"
      body {
        grid: [header-top] "a a a" [header-bottom]
              [main-top] "b b b" 1fr [main-bottom]
              / auto 1fr auto;
      }

      header {
        grid-area: a;
      }

      main {
        grid-row: main-top / main-bottom;
      }
    "#, indoc!{r#"
      body {
        grid: [header-top_EgL3uq] "a_EgL3uq a_EgL3uq a_EgL3uq" [header-bottom_EgL3uq]
              [main-top_EgL3uq] "b_EgL3uq b_EgL3uq b_EgL3uq" 1fr [main-bottom_EgL3uq]
              / auto 1fr auto;
      }

      header {
        grid-area: a_EgL3uq;
      }

      main {
        grid-row: main-top_EgL3uq / main-bottom_EgL3uq;
      }
    "#}, map! {
      "header-top" => "header-top_EgL3uq",
      "header-bottom" => "header-bottom_EgL3uq",
      "main-top" => "main-top_EgL3uq",
      "main-bottom" => "main-bottom_EgL3uq",
      "a" => "a_EgL3uq",
      "b" => "b_EgL3uq"
    });

    css_modules_test(r#"
      test {
        transition-property: opacity;
      }
    "#, indoc!{r#"
      test {
        transition-property: opacity;
      }
    "#}, map! {});

    css_modules_test(r#"
      :global(.foo) {
        color: red;
      }

      :local(.bar) {
        color: yellow;
      }

      .bar :global(.baz) {
        color: purple;
      }
    "#, indoc!{r#"
      .foo {
        color: red;
      }

      .bar_EgL3uq {
        color: #ff0;
      }

      .bar_EgL3uq .baz {
        color: purple;
      }
    "#}, map! {
      "bar" => "bar_EgL3uq"
    });


    // :global(:local(.hi)) {
    //   color: green;
    // }


    css_modules_test(r#"
      .test {
        composes: foo;
        background: white;
      }

      .foo {
        color: red;
      }
    "#, indoc!{r#"
      .test_EgL3uq {
        background: #fff;
      }

      .foo_EgL3uq {
        color: red;
      }
    "#}, map! {
      "test" => "test_EgL3uq" "foo_EgL3uq",
      "foo" => "foo_EgL3uq"
    });

    css_modules_test(r#"
      .a, .b {
        composes: foo;
        background: white;
      }

      .foo {
        color: red;
      }
    "#, indoc!{r#"
      .a_EgL3uq, .b_EgL3uq {
        background: #fff;
      }

      .foo_EgL3uq {
        color: red;
      }
    "#}, map! {
      "a" => "a_EgL3uq" "foo_EgL3uq",
      "b" => "b_EgL3uq" "foo_EgL3uq",
      "foo" => "foo_EgL3uq"
    });

    css_modules_test(r#"
      .test {
        composes: foo bar;
        background: white;
      }

      .foo {
        color: red;
      }

      .bar {
        color: yellow;
      }
    "#, indoc!{r#"
      .test_EgL3uq {
        background: #fff;
      }

      .foo_EgL3uq {
        color: red;
      }

      .bar_EgL3uq {
        color: #ff0;
      }
    "#}, map! {
      "test" => "test_EgL3uq" "foo_EgL3uq" "bar_EgL3uq",
      "foo" => "foo_EgL3uq",
      "bar" => "bar_EgL3uq"
    });

    css_modules_test(r#"
      .test {
        composes: foo from global;
        background: white;
      }
    "#, indoc!{r#"
      .test_EgL3uq {
        background: #fff;
      }
    "#}, map! {
      "test" => "test_EgL3uq" "foo" global: true
    });

    css_modules_test(r#"
      .test {
        composes: foo bar from global;
        background: white;
      }
    "#, indoc!{r#"
      .test_EgL3uq {
        background: #fff;
      }
    "#}, map! {
      "test" => "test_EgL3uq" "foo" global: true "bar" global: true
    });

    css_modules_test(r#"
      .test {
        composes: foo from "foo.css";
        background: white;
      }
    "#, indoc!{r#"
      .test_EgL3uq {
        background: #fff;
      }
    "#}, map! {
      "test" => "test_EgL3uq" "foo" from "foo.css"
    });

    css_modules_test(r#"
      .test {
        composes: foo bar from "foo.css";
        background: white;
      }
    "#, indoc!{r#"
      .test_EgL3uq {
        background: #fff;
      }
    "#}, map! {
      "test" => "test_EgL3uq" "foo" from "foo.css" "bar" from "foo.css"
    });

    css_modules_test(r#"
      .test {
        composes: foo;
        composes: foo from "foo.css";
        composes: bar from "bar.css";
        background: white;
      }

      .foo {
        color: red;
      }
    "#, indoc!{r#"
      .test_EgL3uq {
        background: #fff;
      }

      .foo_EgL3uq {
        color: red;
      }
    "#}, map! {
      "test" => "test_EgL3uq" "foo_EgL3uq" "foo" from "foo.css" "bar" from "bar.css",
      "foo" => "foo_EgL3uq"
    });
  }

  #[test]
  fn test_pseudo_replacement() {
    let source = r#"
      .foo:hover {
        color: red;
      }

      .foo:active {
        color: yellow;
      }

      .foo:focus-visible {
        color: purple;
      }
    "#;

    let expected = indoc! { r#"
      .foo.is-hovered {
        color: red;
      }

      .foo.is-active {
        color: #ff0;
      }

      .foo.focus-visible {
        color: purple;
      }
    "#};

    let stylesheet = StyleSheet::parse("test.css".into(), &source, ParserOptions::default()).unwrap();
    let res = stylesheet.to_css(PrinterOptions {
      pseudo_classes: Some(PseudoClasses {
        hover: Some("is-hovered"),
        active: Some("is-active"),
        focus_visible: Some("focus-visible"),
        ..PseudoClasses::default()
      }),
      ..PrinterOptions::default()
    }).unwrap();
    assert_eq!(res.code, expected);

    let source = r#"
      .foo:hover {
        color: red;
      }
    "#;

    let expected = indoc! { r#"
      .foo_EgL3uq.is-hovered_EgL3uq {
        color: red;
      }
    "#};

    let stylesheet = StyleSheet::parse("test.css".into(), &source, ParserOptions { css_modules: true, ..ParserOptions::default() }).unwrap();
    let res = stylesheet.to_css(PrinterOptions {
      pseudo_classes: Some(PseudoClasses {
        hover: Some("is-hovered"),
        ..PseudoClasses::default()
      }),
      ..PrinterOptions::default()
    }).unwrap();
    assert_eq!(res.code, expected);
  }

  #[test]
  fn test_unused_symbols() {
    let source = r#"
      .foo {
        color: red;
      }

      .bar {
        color: green;
      }

      .bar:hover {
        color: purple;
      }

      .bar .baz {
        background: red;
      }

      .baz:is(.bar) {
        background: green;
      }
      
      #id {
        animation: 2s test;
      }

      #other_id {
        color: red;
      }

      @keyframes test {
        from { color: red }
        to { color: yellow }
      }

      @counter-style circles {
        symbols: Ⓐ Ⓑ Ⓒ;
      }

      @keyframes fade {
        from { opacity: 0 }
        to { opacity: 1 }
      }
    "#;

    let expected = indoc!{r#"
      .foo {
        color: red;
      }
      
      #id {
        animation: test 2s;
      }

      @keyframes test {
        from {
          color: red;
        }

        to {
          color: #ff0;
        }
      }
    "#};

    let mut stylesheet = StyleSheet::parse("test.css".into(), &source, ParserOptions::default()).unwrap();
    stylesheet.minify(MinifyOptions {
      unused_symbols: vec!["bar", "other_id", "fade", "circles"].iter().map(|s| String::from(*s)).collect(),
      ..MinifyOptions::default()
    }).unwrap();
    let res = stylesheet.to_css(PrinterOptions::default()).unwrap();
    assert_eq!(res.code, expected);

    let source = r#"
      .foo {
        color: red;

        &.bar {
          color: green;
        }
      }
    "#;

    let expected = indoc!{r#"
      .foo {
        color: red;
      }
    "#};

    let mut stylesheet = StyleSheet::parse("test.css".into(), &source, ParserOptions { nesting: true, ..ParserOptions::default() }).unwrap();
    stylesheet.minify(MinifyOptions {
      unused_symbols: vec!["bar"].iter().map(|s| String::from(*s)).collect(),
      ..MinifyOptions::default()
    }).unwrap();
    let res = stylesheet.to_css(PrinterOptions::default()).unwrap();
    assert_eq!(res.code, expected);

    let source = r#"
      .foo {
        color: red;

        &.bar {
          color: purple;
        }

        @nest &.bar {
          color: orange;
        }

        @nest :not(&) {
          color: green;
        }

        @media (orientation: portrait) {
          color: brown;
        }
      }

      .x {
        color: purple;

        &.y {
          color: green;
        }
      }
    "#;

    let expected = indoc!{r#"
      :not(.foo) {
        color: green;
      }
    "#};

    let mut stylesheet = StyleSheet::parse("test.css".into(), &source, ParserOptions { nesting: true, ..ParserOptions::default() }).unwrap();
    stylesheet.minify(MinifyOptions {
      unused_symbols: vec!["foo", "x"].iter().map(|s| String::from(*s)).collect(),
      ..MinifyOptions::default()
    }).unwrap();
    let res = stylesheet.to_css(PrinterOptions {
      targets: Some(Browsers { chrome: Some(95 << 16), ..Browsers::default() }),
      ..PrinterOptions::default()
    }).unwrap();
    assert_eq!(res.code, expected);
  }

  #[test]
  fn test_svg() {
    minify_test(".foo { fill: yellow; }", ".foo{fill:#ff0}");
    minify_test(".foo { fill: url(#foo); }", ".foo{fill:url(#foo)}");
    minify_test(".foo { fill: url(#foo) none; }", ".foo{fill:url(#foo) none}");
    minify_test(".foo { fill: url(#foo) yellow; }", ".foo{fill:url(#foo) #ff0}");
    minify_test(".foo { fill: none; }", ".foo{fill:none}");
    minify_test(".foo { fill: context-fill; }", ".foo{fill:context-fill}");
    minify_test(".foo { fill: context-stroke; }", ".foo{fill:context-stroke}");

    minify_test(".foo { stroke: yellow; }", ".foo{stroke:#ff0}");
    minify_test(".foo { stroke: url(#foo); }", ".foo{stroke:url(#foo)}");
    minify_test(".foo { stroke: url(#foo) none; }", ".foo{stroke:url(#foo) none}");
    minify_test(".foo { stroke: url(#foo) yellow; }", ".foo{stroke:url(#foo) #ff0}");
    minify_test(".foo { stroke: none; }", ".foo{stroke:none}");
    minify_test(".foo { stroke: context-fill; }", ".foo{stroke:context-fill}");
    minify_test(".foo { stroke: context-stroke; }", ".foo{stroke:context-stroke}");

    minify_test(".foo { marker-start: url(#foo); }", ".foo{marker-start:url(#foo)}");

    minify_test(".foo { stroke-dasharray: 4 1 2; }", ".foo{stroke-dasharray:4 1 2}");
    minify_test(".foo { stroke-dasharray: 4,1,2; }", ".foo{stroke-dasharray:4 1 2}");
    minify_test(".foo { stroke-dasharray: 4, 1, 2; }", ".foo{stroke-dasharray:4 1 2}");
    minify_test(".foo { stroke-dasharray: 4px, 1px, 2px; }", ".foo{stroke-dasharray:4 1 2}");

    minify_test(".foo { mask: url('foo.svg'); }", ".foo{mask:url(foo.svg)}");
    minify_test(".foo { mask: url(masks.svg#star) luminance }", ".foo{mask:url(masks.svg#star) luminance}");
    minify_test(".foo { mask: url(masks.svg#star) 40px 20px }", ".foo{mask:url(masks.svg#star) 40px 20px}");
    minify_test(".foo { mask: url(masks.svg#star) 0 0 / 50px 50px }", ".foo{mask:url(masks.svg#star) 0 0/50px 50px}");
    minify_test(".foo { mask: url(masks.svg#star) repeat-x }", ".foo{mask:url(masks.svg#star) repeat-x}");
    minify_test(".foo { mask: url(masks.svg#star) stroke-box }", ".foo{mask:url(masks.svg#star) stroke-box}");
    minify_test(".foo { mask: url(masks.svg#star) stroke-box stroke-box }", ".foo{mask:url(masks.svg#star) stroke-box}");
    minify_test(".foo { mask: url(masks.svg#star) border-box }", ".foo{mask:url(masks.svg#star)}");
    minify_test(".foo { mask: url(masks.svg#star) left / 16px repeat-y, url(masks.svg#circle) right / 16px repeat-y }", ".foo{mask:url(masks.svg#star) 0/16px repeat-y,url(masks.svg#circle) 100%/16px repeat-y}");

    minify_test(".foo { mask-border: url('border-mask.png') 25; }", ".foo{mask-border:url(border-mask.png) 25}");
    minify_test(".foo { mask-border: url('border-mask.png') 25 / 35px / 12px space alpha; }", ".foo{mask-border:url(border-mask.png) 25/35px/12px space}");
    minify_test(".foo { mask-border: url('border-mask.png') 25 / 35px / 12px space luminance; }", ".foo{mask-border:url(border-mask.png) 25/35px/12px space luminance}");
    minify_test(".foo { mask-border: url('border-mask.png') luminance 25 / 35px / 12px space; }", ".foo{mask-border:url(border-mask.png) 25/35px/12px space luminance}");

    minify_test(".foo { clip-path: url('clip.svg#star'); }", ".foo{clip-path:url(clip.svg#star)}");
    minify_test(".foo { clip-path: margin-box; }", ".foo{clip-path:margin-box}");
    minify_test(".foo { clip-path: inset(100px 50px); }", ".foo{clip-path:inset(100px 50px)}");
    minify_test(".foo { clip-path: inset(100px 50px round 5px); }", ".foo{clip-path:inset(100px 50px round 5px)}");
    minify_test(".foo { clip-path: inset(100px 50px round 5px 5px 5px 5px); }", ".foo{clip-path:inset(100px 50px round 5px)}");
    minify_test(".foo { clip-path: circle(50px); }", ".foo{clip-path:circle(50px)}");
    minify_test(".foo { clip-path: circle(50px at center center); }", ".foo{clip-path:circle(50px)}");
    minify_test(".foo { clip-path: circle(50px at 50% 50%); }", ".foo{clip-path:circle(50px)}");
    minify_test(".foo { clip-path: circle(50px at 0 100px); }", ".foo{clip-path:circle(50px at 0 100px)}");
    minify_test(".foo { clip-path: circle(closest-side at 0 100px); }", ".foo{clip-path:circle(at 0 100px)}");
    minify_test(".foo { clip-path: circle(farthest-side at 0 100px); }", ".foo{clip-path:circle(farthest-side at 0 100px)}");
    minify_test(".foo { clip-path: circle(closest-side at 50% 50%); }", ".foo{clip-path:circle()}");
    minify_test(".foo { clip-path: ellipse(50px 60px at 0 10% 20%); }", ".foo{clip-path:ellipse(50px 60px at 0 10% 20%)}");
    minify_test(".foo { clip-path: ellipse(50px 60px at center center); }", ".foo{clip-path:ellipse(50px 60px)}");
    minify_test(".foo { clip-path: ellipse(closest-side closest-side at 50% 50%); }", ".foo{clip-path:ellipse()}");
    minify_test(".foo { clip-path: ellipse(closest-side closest-side at 10% 20%); }", ".foo{clip-path:ellipse(at 10% 20%)}");
    minify_test(".foo { clip-path: ellipse(farthest-side closest-side at 10% 20%); }", ".foo{clip-path:ellipse(farthest-side closest-side at 10% 20%)}");
    minify_test(".foo { clip-path: polygon(50% 0%, 100% 50%, 50% 100%, 0% 50%); }", ".foo{clip-path:polygon(50% 0%,100% 50%,50% 100%,0% 50%)}");
    minify_test(".foo { clip-path: polygon(nonzero, 50% 0%, 100% 50%, 50% 100%, 0% 50%); }", ".foo{clip-path:polygon(50% 0%,100% 50%,50% 100%,0% 50%)}");
    minify_test(".foo { clip-path: polygon(evenodd, 50% 0%, 100% 50%, 50% 100%, 0% 50%); }", ".foo{clip-path:polygon(evenodd,50% 0%,100% 50%,50% 100%,0% 50%)}");
    minify_test(".foo { clip-path: padding-box circle(50px at 0 100px); }", ".foo{clip-path:circle(50px at 0 100px) padding-box}");
    minify_test(".foo { clip-path: circle(50px at 0 100px) padding-box; }", ".foo{clip-path:circle(50px at 0 100px) padding-box}");
    minify_test(".foo { clip-path: circle(50px at 0 100px) border-box; }", ".foo{clip-path:circle(50px at 0 100px)}");
  }

  #[test]
  fn test_filter() {
    minify_test(".foo { filter: url('filters.svg#filter-id'); }", ".foo{filter:url(filters.svg#filter-id)}");
    minify_test(".foo { filter: blur(5px); }", ".foo{filter:blur(5px)}");
    minify_test(".foo { filter: blur(0px); }", ".foo{filter:blur()}");
    minify_test(".foo { filter: brightness(10%); }", ".foo{filter:brightness(10%)}");
    minify_test(".foo { filter: brightness(100%); }", ".foo{filter:brightness()}");
    minify_test(".foo { filter: drop-shadow(16px 16px 20px yellow); }", ".foo{filter:drop-shadow(16px 16px 20px #ff0)}");
    minify_test(".foo { filter: contrast(175%) brightness(3%); }", ".foo{filter:contrast(175%)brightness(3%)}");
  }

  #[test]
  fn test_viewport() {
    minify_test(r#"
    @viewport {
      width: 100vw;
    }"#, "@viewport{width:100vw}");
    minify_test(r#"
    @-ms-viewport {
      width: device-width;
    }"#, "@-ms-viewport{width:device-width}");
  }

  #[test]
  fn test_custom_media() {
    custom_media_test(
      r#"
      @custom-media --modern (color), (hover);

      @media (--modern) and (width > 1024px) {
        .a {
          color: green;
        }
      }
      "#,
      indoc! {r#"
      @media ((color) or (hover)) and (width > 1024px) {
        .a {
          color: green;
        }
      }
      "#}
    );

    custom_media_test(
      r#"
      @custom-media --color (color);

      @media (--color) and (width > 1024px) {
        .a {
          color: green;
        }
      }
      "#,
      indoc! {r#"
      @media (color) and (width > 1024px) {
        .a {
          color: green;
        }
      }
      "#}
    );

    custom_media_test(
      r#"
      @custom-media --a (color);
      @custom-media --b (--a);

      @media (--b) and (width > 1024px) {
        .a {
          color: green;
        }
      }
      "#,
      indoc! {r#"
      @media (color) and (width > 1024px) {
        .a {
          color: green;
        }
      }
      "#}
    );

    custom_media_test(
      r#"
      @custom-media --not-color not (color);

      @media not (--not-color) {
        .a {
          color: green;
        }
      }
      "#,
      indoc! {r#"
      @media (color) {
        .a {
          color: green;
        }
      }
      "#}
    );

    custom_media_test(
      r#"
      @custom-media --color-print print and (color);

      @media (--color-print) {
        .a {
          color: green;
        }
      }
      "#,
      indoc! {r#"
      @media print and (color) {
        .a {
          color: green;
        }
      }
      "#}
    );

    custom_media_test(
      r#"
      @custom-media --color-print print and (color);

      @media print and (--color-print) {
        .a {
          color: green;
        }
      }
      "#,
      indoc! {r#"
      @media print and (color) {
        .a {
          color: green;
        }
      }
      "#}
    );

    custom_media_test(
      r#"
      @custom-media --not-color-print not print and (color);

      @media not print and (--not-color-print) {
        .a {
          color: green;
        }
      }
      "#,
      indoc! {r#"
      @media not print and (color) {
        .a {
          color: green;
        }
      }
      "#}
    );

    custom_media_test(
      r#"
      @custom-media --print print;

      @media (--print) {
        .a {
          color: green;
        }
      }
      "#,
      indoc! {r#"
      @media print {
        .a {
          color: green;
        }
      }
      "#}
    );

    custom_media_test(
      r#"
      @custom-media --print print;

      @media not (--print) {
        .a {
          color: green;
        }
      }
      "#,
      indoc! {r#"
      @media not print {
        .a {
          color: green;
        }
      }
      "#}
    );

    custom_media_test(
      r#"
      @custom-media --print not print;

      @media not (--print) {
        .a {
          color: green;
        }
      }
      "#,
      indoc! {r#"
      @media print {
        .a {
          color: green;
        }
      }
      "#}
    );

    custom_media_test(
      r#"
      @custom-media --print print;

      @media ((--print)) {
        .a {
          color: green;
        }
      }
      "#,
      indoc! {r#"
      @media print {
        .a {
          color: green;
        }
      }
      "#}
    );

    custom_media_test(
      r#"
      @custom-media --color (color);
      @custom-media --print print;

      @media (--print) and (--color) {
        .a {
          color: green;
        }
      }
      "#,
      indoc! {r#"
      @media print and (color) {
        .a {
          color: green;
        }
      }
      "#}
    );

    custom_media_test(
      r#"
      @custom-media --color (color);
      @custom-media --not-print not print;

      @media (--not-print) and (--color) {
        .a {
          color: green;
        }
      }
      "#,
      indoc! {r#"
      @media not print and (color) {
        .a {
          color: green;
        }
      }
      "#}
    );

    custom_media_test(
      r#"
      @custom-media --color (color);
      @custom-media --screen screen;
      @custom-media --print print;

      @media (--print) and (--color), (--screen) and (--color) {
        .a {
          color: green;
        }
      }
      "#,
      indoc! {r#"
      @media print and (color), screen and (color) {
        .a {
          color: green;
        }
      }
      "#}
    );

    custom_media_test(
      r#"
      @custom-media --color print and (color), print and (script);

      @media (--color) {
        .a {
          color: green;
        }
      }
      "#,
      indoc! {r#"
      @media print and ((color) or (script)) {
        .a {
          color: green;
        }
      }
      "#}
    );

    custom_media_test(
      r#"
      @custom-media --color (color);
      @custom-media --not-color not all and (--color);

      @media (--not-color) {
        .a {
          color: green;
        }
      }
      "#,
      "\n"
    );

    custom_media_test(
      r#"
      @custom-media --color (color);

      @media not all and (--color) {
        .a {
          color: green;
        }
      }
      "#,
      "\n"
    );

    custom_media_test(
      r#"
      @media (--print) {
        .a {
          color: green;
        }
      }

      @custom-media --print print;
      "#,
      indoc! {r#"
      @media print {
        .a {
          color: green;
        }
      }
      "#}
    );

    fn custom_media_error_test(source: &str, err: Error<MinifyErrorKind>) {
      let mut stylesheet = StyleSheet::parse("test.css".into(), &source, ParserOptions { custom_media: true, ..ParserOptions::default() }).unwrap();
      let res = stylesheet.minify(MinifyOptions {
        targets: Some(Browsers { chrome: Some(95 << 16), ..Browsers::default() }),
        ..MinifyOptions::default()
      });
      assert_eq!(res, Err(err))
    }

    custom_media_error_test(
      r#"
      @custom-media --color-print print and (color);

      @media screen and (--color-print) {
        .a {
          color: green;
        }
      }
      "#,
      Error {
        kind: MinifyErrorKind::UnsupportedCustomMediaBooleanLogic {
          custom_media_loc: Location {
            source_index: 0,
            line: 1,
            column: 7
          }
        },
        loc: Some(ErrorLocation {
          filename: "test.css".into(),
          line: 3,
          column: 7
        }),
      }
    );

    custom_media_error_test(
      r#"
      @custom-media --color-print print and (color);

      @media not print and (--color-print) {
        .a {
          color: green;
        }
      }
      "#,
      Error {
        kind: MinifyErrorKind::UnsupportedCustomMediaBooleanLogic {
          custom_media_loc: Location {
            source_index: 0,
            line: 1,
            column: 7
          }
        },
        loc: Some(ErrorLocation {
          filename: "test.css".into(),
          line: 3,
          column: 7
        }),
      }
    );

    custom_media_error_test(
      r#"
      @custom-media --color-print print and (color);
      @custom-media --color-screen screen and (color);

      @media (--color-print) or (--color-screen) {}
      "#,
      Error {
        kind: MinifyErrorKind::UnsupportedCustomMediaBooleanLogic {
          custom_media_loc: Location {
            source_index: 0,
            line: 2,
            column: 7
          }
        },
        loc: Some(ErrorLocation {
          filename: "test.css".into(),
          line: 4,
          column: 7
        }),
      }
    );

    custom_media_error_test(
      r#"
      @custom-media --color-print print and (color);
      @custom-media --color-screen screen and (color);

      @media (--color-print) and (--color-screen) {}
      "#,
      Error {
        kind: MinifyErrorKind::UnsupportedCustomMediaBooleanLogic {
          custom_media_loc: Location {
            source_index: 0,
            line: 2,
            column: 7
          }
        },
        loc: Some(ErrorLocation {
          filename: "test.css".into(),
          line: 4,
          column: 7
        }),
      }
    );

    custom_media_error_test(
      r#"
      @custom-media --screen screen;
      @custom-media --print print;

      @media (--print) and (--screen) {}
      "#,
      Error {
        kind: MinifyErrorKind::UnsupportedCustomMediaBooleanLogic {
          custom_media_loc: Location {
            source_index: 0,
            line: 1,
            column: 7
          }
        },
        loc: Some(ErrorLocation {
          filename: "test.css".into(),
          line: 4,
          column: 7
        }),
      }
    );

    custom_media_error_test(
      r#"
      @custom-media --not-print not print and (color);
      @custom-media --not-screen not screen and (color);

      @media ((script) or ((--not-print) and (--not-screen))) {
        .a {
          color: green;
        }
      }
      "#,
      Error {
        kind: MinifyErrorKind::UnsupportedCustomMediaBooleanLogic {
          custom_media_loc: Location {
            source_index: 0,
            line: 2,
            column: 7
          }
        },
        loc: Some(ErrorLocation {
          filename: "test.css".into(),
          line: 4,
          column: 7
        }),
      }
    );

    custom_media_error_test(
      r#"
      @custom-media --color screen and (color), print and (color);

      @media (--color) {
        .a {
          color: green;
        }
      }
      "#,
      Error {
        kind: MinifyErrorKind::UnsupportedCustomMediaBooleanLogic {
          custom_media_loc: Location {
            source_index: 0,
            line: 1,
            column: 7
          }
        },
        loc: Some(ErrorLocation {
          filename: "test.css".into(),
          line: 3,
          column: 7
        }),
      }
    );

    custom_media_error_test(
      r#"
      @media (--not-defined) {
        .a {
          color: green;
        }
      }
      "#,
      Error {
        kind: MinifyErrorKind::CustomMediaNotDefined {
          name: "--not-defined".into(),
        },
        loc: Some(ErrorLocation {
          filename: "test.css".into(),
          line: 1,
          column: 7
        }),
      }
    );

    custom_media_error_test(
      r#"
      @custom-media --circular-mq-a (--circular-mq-b);
      @custom-media --circular-mq-b (--circular-mq-a);

      @media (--circular-mq-a) {
        body {
          order: 3;
        }
      }
      "#,
      Error {
        kind: MinifyErrorKind::CircularCustomMedia {
          name: "--circular-mq-a".into(),
        },
        loc: Some(ErrorLocation {
          filename: "test.css".into(),
          line: 4,
          column: 7
        }),
      }
    );
  }

  #[test]
  fn test_dependencies() {
    fn dep_test(source: &str, expected: &str, deps: Vec<(&str, &str)>) {
      let mut stylesheet = StyleSheet::parse("test.css".into(), &source, ParserOptions::default()).unwrap();
      stylesheet.minify(MinifyOptions::default()).unwrap();
      let res = stylesheet.to_css(PrinterOptions {
        analyze_dependencies: true,
        minify: true,
        ..PrinterOptions::default()
      }).unwrap();
      assert_eq!(res.code, expected);
      let dependencies = res.dependencies.unwrap();
      assert_eq!(dependencies.len(), deps.len());
      for (i, (url, placeholder)) in deps.into_iter().enumerate() {
        match &dependencies[i] {
          Dependency::Url(dep) => {
            assert_eq!(dep.url, url);
            assert_eq!(dep.placeholder, placeholder);    
          }
          _ => unreachable!()
        }
      }
    }

    dep_test(
      ".foo { background: image-set('./img12x.png', './img21x.png' 2x)}",
      ".foo{background:image-set(\"hXFI8W\",\"5TkpBa\" 2x)}",
      vec![
        ("./img12x.png", "hXFI8W"),
        ("./img21x.png", "5TkpBa")
      ]
    );

    dep_test(
      ".foo { background: image-set(url(./img12x.png), url('./img21x.png') 2x)}",
      ".foo{background:image-set(\"hXFI8W\",\"5TkpBa\" 2x)}",
      vec![
        ("./img12x.png", "hXFI8W"),
        ("./img21x.png", "5TkpBa")
      ]
    );
  }

  #[test]
  fn test_api() {
    let stylesheet = StyleSheet::parse("test.css".into(), ".foo:hover { color: red }", ParserOptions::default()).unwrap();
    match &stylesheet.rules.0[0] {
      CssRule::Style(s) => {
        assert_eq!(&s.selectors.to_string(), ".foo:hover");
      },
      _ => unreachable!()
    }
  }

  #[test]
  fn test_layer() {
    minify_test("@layer foo;", "@layer foo;");
    minify_test("@layer foo, bar;", "@layer foo,bar;");
    minify_test("@layer foo.bar;", "@layer foo.bar;");
    minify_test("@layer foo.bar, baz;", "@layer foo.bar,baz;");

    minify_test(r#"
      @layer foo {
        .bar {
          color: red;
        }
      }
    "#, "@layer foo{.bar{color:red}}");
    minify_test(r#"
      @layer foo.bar {
        .bar {
          color: red;
        }
      }
    "#, "@layer foo.bar{.bar{color:red}}");
    minify_test(r#"
      @layer base {
        p { max-width: 70ch; }
      }
      
      @layer framework {
        @layer base {
          p { margin-block: 0.75em; }
        }
      
        @layer theme {
          p { color: #222; }
        }
      }
    "#, "@layer base{p{max-width:70ch}}@layer framework{@layer base{p{margin-block:.75em}}@layer theme{p{color:#222}}}");
    minify_test(r#"
      @layer {
        .bar {
          color: red;
        }
      }
    "#, "@layer{.bar{color:red}}");
    error_test("@layer;", ParserError::UnexpectedToken(Token::Semicolon));
    error_test("@layer foo, bar {};", ParserError::AtRuleBodyInvalid);
    minify_test("@import 'test.css' layer;", "@import \"test.css\" layer;");
    minify_test("@import 'test.css' layer(foo);", "@import \"test.css\" layer(foo);");
    minify_test("@import 'test.css' layer(foo.bar);", "@import \"test.css\" layer(foo.bar);");
    error_test("@import 'test.css' layer(foo, bar) {};", ParserError::UnexpectedToken(Token::Comma));
  }

  #[test]
  fn test_property() {
    minify_test(r#"
      @property --property-name {
        syntax: '<color>';
        inherits: false;
        initial-value: yellow;
      }
    "#, "@property --property-name{syntax:\"<color>\";inherits:false;initial-value:#ff0}");

    minify_test(r#"
      @property --property-name {
        syntax: '<length>';
        inherits: true;
        initial-value: 25px;
      }
    "#, "@property --property-name{syntax:\"<length>\";inherits:true;initial-value:25px}");
    
    error_test(r#"
      @property --property-name {
        syntax: '<color>';
        inherits: false;
        initial-value: 25px;
      }
    "#, ParserError::UnexpectedToken(crate::properties::custom::Token::Dimension { has_sign: false, value: 25.0, int_value: Some(25), unit: "px".into() }));

    error_test(r#"
      @property --property-name {
        syntax: '<length>';
        inherits: false;
        initial-value: var(--some-value);
      }
    "#, ParserError::UnexpectedToken(crate::properties::custom::Token::Function("var".into())));

    error_test(r#"
      @property --property-name {
        syntax: '<color>';
        inherits: false;
      }
    "#, ParserError::AtRuleBodyInvalid);

    minify_test(r#"
      @property --property-name {
        syntax: '*';
        inherits: false;
      }
    "#, "@property --property-name{syntax:\"*\";inherits:false}");

    error_test(r#"
      @property --property-name {
        syntax: '*';
      }
    "#, ParserError::AtRuleBodyInvalid);

    error_test(r#"
      @property --property-name {
        inherits: false;
      }
    "#, ParserError::AtRuleBodyInvalid);

    error_test(r#"
      @property property-name {
        syntax: '*';
        inherits: false;
      }
    "#, ParserError::UnexpectedToken(crate::properties::custom::Token::Ident("property-name".into())));

    minify_test(r#"
      @property --property-name {
        syntax: 'custom | <color>';
        inherits: false;
        initial-value: yellow;
      }
    "#, "@property --property-name{syntax:\"custom|<color>\";inherits:false;initial-value:#ff0}");

    minify_test(r#"
      @property --property-name {
        syntax: '<transform-list>';
        inherits: false;
        initial-value: translate(200px,300px) translate(100px,200px) scale(2);
      }
    "#, "@property --property-name{syntax:\"<transform-list>\";inherits:false;initial-value:matrix(2,0,0,2,300,500)}");

    minify_test(r#"
      @property --property-name {
        syntax: '<time>';
        inherits: false;
        initial-value: 1000ms;
      }
    "#, "@property --property-name{syntax:\"<time>\";inherits:false;initial-value:1s}");

    minify_test(r#"
      @property --property-name {
        syntax: '<url>';
        inherits: false;
        initial-value: url("foo.png");
      }
    "#, "@property --property-name{syntax:\"<url>\";inherits:false;initial-value:url(foo.png)}");

    minify_test(r#"
      @property --property-name {
        syntax: '<image>';
        inherits: false;
        initial-value: linear-gradient(yellow, blue);
      }
    "#, "@property --property-name{syntax:\"<image>\";inherits:false;initial-value:linear-gradient(#ff0,#00f)}");

    minify_test(r#"
      @property --property-name {
        initial-value: linear-gradient(yellow, blue);
        inherits: false;
        syntax: '<image>';
      }
    "#, "@property --property-name{syntax:\"<image>\";inherits:false;initial-value:linear-gradient(#ff0,#00f)}");

    test(r#"
      @property --property-name {
        syntax: '<length>|none';
        inherits: false;
        initial-value: none;
      }
    "#, indoc!{r#"
      @property --property-name {
        syntax: "<length> | none";
        inherits: false;
        initial-value: none;
      }
    "#});

    minify_test(r#"
      @property --property-name {
        syntax: '<color>#';
        inherits: false;
        initial-value: yellow, blue;
      }
    "#, "@property --property-name{syntax:\"<color>#\";inherits:false;initial-value:#ff0,#00f}");
    minify_test(r#"
      @property --property-name {
        syntax: '<color>+';
        inherits: false;
        initial-value: yellow blue;
      }
    "#, "@property --property-name{syntax:\"<color>+\";inherits:false;initial-value:#ff0 #00f}");
  }
}
