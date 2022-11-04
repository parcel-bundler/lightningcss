use lightningcss::{
  declaration::DeclarationBlock,
  properties::{Property, PropertyId},
  stylesheet::{ParserOptions, PrinterOptions},
  traits::ToCss,
  vendor_prefix::VendorPrefix,
};

fn get_test(decls: &str, property_id: PropertyId, expected: Option<(&str, bool)>) {
  let decls = DeclarationBlock::parse_string(decls, ParserOptions::default()).unwrap();
  let v = decls.get(&property_id);
  if let Some((expected, important)) = expected {
    let (value, is_important) = v.unwrap();
    assert_eq!(
      *value,
      Property::parse_string(property_id, expected, ParserOptions::default()).unwrap()
    );
    assert_eq!(is_important, important);
  } else {
    assert_eq!(v, None)
  }
}

#[test]
fn test_get() {
  get_test("color: red", PropertyId::Color, Some(("red", false)));
  get_test("color: red !important", PropertyId::Color, Some(("red", true)));
  get_test("color: green; color: red", PropertyId::Color, Some(("red", false)));
  get_test(
    r#"
    margin-top: 5px;
    margin-bottom: 5px;
    margin-left: 5px;
    margin-right: 5px;
    "#,
    PropertyId::Margin,
    Some(("5px", false)),
  );
  get_test(
    r#"
    margin-top: 5px;
    margin-bottom: 5px;
    margin-left: 6px;
    margin-right: 6px;
    "#,
    PropertyId::Margin,
    Some(("5px 6px", false)),
  );
  get_test(
    r#"
    margin-top: 5px;
    margin-bottom: 5px;
    margin-left: 6px;
    margin-right: 6px;
    "#,
    PropertyId::Margin,
    Some(("5px 6px", false)),
  );
  get_test(
    r#"
    margin-top: 5px;
    margin-bottom: 5px;
    "#,
    PropertyId::Margin,
    None,
  );
  get_test(
    r#"
    margin-top: 5px;
    margin-bottom: 5px;
    margin-left: 5px !important;
    margin-right: 5px;
    "#,
    PropertyId::Margin,
    None,
  );
  get_test(
    r#"
    margin-top: 5px !important;
    margin-bottom: 5px !important;
    margin-left: 5px !important;
    margin-right: 5px !important;
    "#,
    PropertyId::Margin,
    Some(("5px", true)),
  );
  get_test(
    "margin: 5px 6px 7px 8px",
    PropertyId::Margin,
    Some(("5px 6px 7px 8px", false)),
  );
  get_test("margin: 5px 6px 7px 8px", PropertyId::MarginTop, Some(("5px", false)));
  get_test(
    r#"
    border: 1px solid red;
    border-color: green;
    "#,
    PropertyId::Border,
    Some(("1px solid green", false)),
  );
  get_test(
    r#"
    border: 1px solid red;
    border-left-color: green;
    "#,
    PropertyId::Border,
    None,
  );
  get_test("background: red", PropertyId::Background, Some(("red", false)));
  get_test("background: red", PropertyId::BackgroundColor, Some(("red", false)));
  get_test(
    "background: red url(foo.png)",
    PropertyId::BackgroundColor,
    Some(("red", false)),
  );
  get_test(
    "background: url(foo.png), url(bar.png) red",
    PropertyId::BackgroundColor,
    Some(("red", false)),
  );
  get_test(
    "background: url(foo.png) green, url(bar.png) red",
    PropertyId::BackgroundColor,
    Some(("red", false)),
  );
  get_test(
    "background: linear-gradient(red, green)",
    PropertyId::BackgroundImage,
    Some(("linear-gradient(red, green)", false)),
  );
  get_test(
    "background: linear-gradient(red, green), linear-gradient(#fff, #000)",
    PropertyId::BackgroundImage,
    Some(("linear-gradient(red, green), linear-gradient(#fff, #000)", false)),
  );
  get_test(
    "background: linear-gradient(red, green) repeat-x, linear-gradient(#fff, #000) repeat-y",
    PropertyId::BackgroundImage,
    Some(("linear-gradient(red, green), linear-gradient(#fff, #000)", false)),
  );
  get_test(
    "background: linear-gradient(red, green) repeat-x, linear-gradient(#fff, #000) repeat-y",
    PropertyId::BackgroundRepeat,
    Some(("repeat-x, repeat-y", false)),
  );
  get_test(
    r#"
    background: linear-gradient(red, green);
    background-position-x: 20px;
    background-position-y: 10px;
    background-size: 50px 100px;
    background-repeat: repeat no-repeat;
    "#,
    PropertyId::Background,
    Some(("linear-gradient(red, green) 20px 10px / 50px 100px repeat-x", false)),
  );
  get_test(
    r#"
    background: linear-gradient(red, green);
    background-position-x: 20px;
    background-position-y: 10px !important;
    background-size: 50px 100px;
    background-repeat: repeat no-repeat;
    "#,
    PropertyId::Background,
    None,
  );
  get_test(
    r#"
    background: linear-gradient(red, green), linear-gradient(#fff, #000) gray;
    background-position-x: right 20px, 10px;
    background-position-y: top 20px, 15px;
    background-size: 50px 50px, auto;
    background-repeat: repeat no-repeat, no-repeat;
    "#,
    PropertyId::Background,
    Some(("linear-gradient(red, green) right 20px top 20px / 50px 50px repeat-x, gray linear-gradient(#fff, #000) 10px 15px no-repeat", false)),
  );
  get_test(
    r#"
    background: linear-gradient(red, green);
    background-position-x: right 20px, 10px;
    background-position-y: top 20px, 15px;
    background-size: 50px 50px, auto;
    background-repeat: repeat no-repeat, no-repeat;
    "#,
    PropertyId::Background,
    None,
  );
  get_test(
    r#"
    background: linear-gradient(red, green);
    background-position: 20px 10px;
    background-size: 50px 100px;
    background-repeat: repeat no-repeat;
    "#,
    PropertyId::Background,
    Some(("linear-gradient(red, green) 20px 10px / 50px 100px repeat-x", false)),
  );
  get_test(
    r#"
    background-position-x: 20px;
    background-position-y: 10px;
    "#,
    PropertyId::BackgroundPosition,
    Some(("20px 10px", false)),
  );
  get_test(
    r#"
    background: linear-gradient(red, green) 20px 10px;
    "#,
    PropertyId::BackgroundPosition,
    Some(("20px 10px", false)),
  );
  get_test(
    r#"
    background: linear-gradient(red, green) 20px 10px;
    "#,
    PropertyId::BackgroundPositionX,
    Some(("20px", false)),
  );
  get_test(
    r#"
    background: linear-gradient(red, green) 20px 10px;
    "#,
    PropertyId::BackgroundPositionY,
    Some(("10px", false)),
  );
  get_test(
    "mask-border: linear-gradient(red, green) 25",
    PropertyId::MaskBorderSource,
    Some(("linear-gradient(red, green)", false)),
  );
  get_test("grid-area: a / b / c / d", PropertyId::GridRowStart, Some(("a", false)));
  get_test("grid-area: a / b / c / d", PropertyId::GridRowEnd, Some(("c", false)));
  get_test("grid-area: a / b / c / d", PropertyId::GridRow, Some(("a / c", false)));
  get_test(
    "grid-area: a / b / c / d",
    PropertyId::GridColumn,
    Some(("b / d", false)),
  );
  get_test(
    r#"
    grid-template-rows: auto 1fr;
    grid-template-columns: auto 1fr auto;
    grid-template-areas: none;
    "#,
    PropertyId::GridTemplate,
    Some(("auto 1fr / auto 1fr auto", false)),
  );
  get_test(
    r#"
    grid-template-areas: ". a a ."
        ". b b .";
    grid-template-rows: auto 1fr;
    grid-template-columns: 10px 1fr 1fr 10px;
    "#,
    PropertyId::GridTemplate,
    Some((
      r#"
      ". a a ."
      ". b b ." 1fr
      / 10px 1fr 1fr 10px
      "#,
      false,
    )),
  );
  get_test(
    r#"
    grid-template-areas: "a a a"
                          "b b b";
    grid-template-columns: repeat(3, 1fr);
    grid-template-rows: auto 1fr;
    "#,
    PropertyId::GridTemplate,
    None,
  );
  get_test(
    r#"
    grid-template-areas: "a a a"
                         "b b b";
    grid-template-rows: [header-top] auto [header-bottom main-top] 1fr [main-bottom];
    grid-template-columns: auto 1fr auto;
    grid-auto-flow: row;
    grid-auto-rows: auto;
    grid-auto-columns: auto;
    "#,
    PropertyId::Grid,
    Some((
      r#"
      [header-top] "a a a" [header-bottom]
      [main-top] "b b b" 1fr [main-bottom]
      / auto 1fr auto
      "#,
      false,
    )),
  );
  get_test(
    r#"
    grid-template-areas: "a a a"
                         "b b b";
    grid-template-rows: [header-top] auto [header-bottom main-top] 1fr [main-bottom];
    grid-template-columns: auto 1fr auto;
    grid-auto-flow: column;
    grid-auto-rows: 1fr;
    grid-auto-columns: 1fr;
    "#,
    PropertyId::Grid,
    None,
  );
  get_test(
    r#"
    flex-direction: row;
    flex-wrap: wrap;
    "#,
    PropertyId::FlexFlow(VendorPrefix::None),
    Some(("row wrap", false)),
  );
  get_test(
    r#"
    -webkit-flex-direction: row;
    -webkit-flex-wrap: wrap;
    "#,
    PropertyId::FlexFlow(VendorPrefix::WebKit),
    Some(("row wrap", false)),
  );
  get_test(
    r#"
    flex-direction: row;
    flex-wrap: wrap;
    "#,
    PropertyId::FlexFlow(VendorPrefix::WebKit),
    None,
  );
  get_test(
    r#"
    -webkit-flex-direction: row;
    flex-wrap: wrap;
    "#,
    PropertyId::FlexFlow(VendorPrefix::WebKit),
    None,
  );
  get_test(
    r#"
    -webkit-flex-direction: row;
    flex-wrap: wrap;
    "#,
    PropertyId::FlexFlow(VendorPrefix::None),
    None,
  );
  get_test(
    r#"
    -webkit-flex-flow: row;
    "#,
    PropertyId::FlexDirection(VendorPrefix::WebKit),
    Some(("row", false)),
  );
  get_test(
    r#"
    -webkit-flex-flow: row;
    "#,
    PropertyId::FlexDirection(VendorPrefix::None),
    None,
  );
}

fn set_test(orig: &str, property: &str, value: &str, important: bool, expected: &str) {
  let mut decls = DeclarationBlock::parse_string(orig, ParserOptions::default()).unwrap();
  decls.set(
    Property::parse_string(property.into(), value, ParserOptions::default()).unwrap(),
    important,
  );
  assert_eq!(decls.to_css_string(PrinterOptions::default()).unwrap(), expected);
}

#[test]
fn test_set() {
  set_test("color: red", "color", "green", false, "color: green");
  set_test("color: red !important", "color", "green", false, "color: green");
  set_test("color: red", "color", "green", true, "color: green !important");
  set_test("margin: 5px", "margin", "10px", false, "margin: 10px");
  set_test("margin: 5px", "margin-top", "8px", false, "margin: 8px 5px 5px");
  set_test(
    "margin: 5px",
    "margin-inline-start",
    "8px",
    false,
    "margin: 5px; margin-inline-start: 8px",
  );
  set_test(
    "margin-inline-start: 5px; margin-top: 10px",
    "margin-inline-start",
    "8px",
    false,
    "margin-inline-start: 5px; margin-top: 10px; margin-inline-start: 8px",
  );
  set_test(
    "margin: 5px; margin-inline-start: 8px",
    "margin-left",
    "10px",
    false,
    "margin: 5px; margin-inline-start: 8px; margin-left: 10px",
  );
  set_test(
    "border: 1px solid red",
    "border-right",
    "1px solid green",
    false,
    "border: 1px solid red; border-right: 1px solid green",
  );
  set_test(
    "border: 1px solid red",
    "border-right-color",
    "green",
    false,
    "border: 1px solid red; border-right-color: green",
  );
  set_test(
    "animation: foo 2s",
    "animation-name",
    "foo, bar",
    false,
    "animation: 2s foo; animation-name: foo, bar",
  );
  set_test("animation: foo 2s", "animation-name", "bar", false, "animation: 2s bar");
  set_test(
    "background: linear-gradient(red, green)",
    "background-position-x",
    "20px",
    false,
    "background: linear-gradient(red, green) 20px 0",
  );
  set_test(
    "background: linear-gradient(red, green)",
    "background-position",
    "20px 10px",
    false,
    "background: linear-gradient(red, green) 20px 10px",
  );
  set_test(
    "flex-flow: row wrap",
    "flex-direction",
    "column",
    false,
    "flex-flow: column wrap",
  );
  set_test(
    "-webkit-flex-flow: row wrap",
    "-webkit-flex-direction",
    "column",
    false,
    "-webkit-flex-flow: column wrap",
  );
  set_test(
    "flex-flow: row wrap",
    "-webkit-flex-direction",
    "column",
    false,
    "flex-flow: wrap; -webkit-flex-direction: column",
  );
}

fn remove_test(orig: &str, property_id: PropertyId, expected: &str) {
  let mut decls = DeclarationBlock::parse_string(orig, ParserOptions::default()).unwrap();
  decls.remove(&property_id);
  assert_eq!(decls.to_css_string(PrinterOptions::default()).unwrap(), expected);
}

#[test]
fn test_remove() {
  remove_test("margin-top: 10px", PropertyId::MarginTop, "");
  remove_test(
    "margin-top: 10px; margin-left: 5px",
    PropertyId::MarginTop,
    "margin-left: 5px",
  );
  remove_test(
    "margin-top: 10px !important; margin-left: 5px",
    PropertyId::MarginTop,
    "margin-left: 5px",
  );
  remove_test(
    "margin: 10px",
    PropertyId::MarginTop,
    "margin-right: 10px; margin-bottom: 10px; margin-left: 10px",
  );
  remove_test("margin: 10px", PropertyId::Margin, "");
  remove_test(
    "margin-top: 10px; margin-right: 10px; margin-bottom: 10px; margin-left: 10px",
    PropertyId::Margin,
    "",
  );
  remove_test(
    "flex-flow: column wrap",
    PropertyId::FlexDirection(VendorPrefix::None),
    "flex-wrap: wrap",
  );
  remove_test(
    "flex-flow: column wrap",
    PropertyId::FlexDirection(VendorPrefix::WebKit),
    "flex-flow: column wrap",
  );
  remove_test(
    "-webkit-flex-flow: column wrap",
    PropertyId::FlexDirection(VendorPrefix::WebKit),
    "-webkit-flex-wrap: wrap",
  );
}
