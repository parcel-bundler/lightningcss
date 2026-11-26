//! CSS properties related to user interface.

use crate::compat;
use crate::context::PropertyHandlerContext;
use crate::declaration::{DeclarationBlock, DeclarationList};
use crate::error::{ParserError, PrinterError};
use crate::macros::{define_shorthand, enum_property, property_bitflags, shorthand_property};
use crate::printer::Printer;
use crate::properties::{Property, PropertyId};
use crate::targets::{should_compile, Browsers, Targets};
use crate::traits::{FallbackValues, IsCompatible, Parse, PropertyHandler, Shorthand, ToCss};
use crate::values::color::CssColor;
use crate::values::number::CSSNumber;
use crate::values::string::CowArcStr;
use crate::values::url::Url;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use bitflags::bitflags;
use cssparser::*;
use smallvec::SmallVec;

use super::custom::Token;
use super::{CustomProperty, CustomPropertyName, TokenList, TokenOrValue};

enum_property! {
  /// A value for the [resize](https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#resize) property.
  pub enum Resize {
    /// The element does not allow resizing.
    None,
    /// The element is resizable in both the x and y directions.
    Both,
    /// The element is resizable in the x direction.
    Horizontal,
    /// The element is resizable in the y direction.
    Vertical,
    /// The element is resizable in the block direction, according to the writing mode.
    Block,
    /// The element is resizable in the inline direction, according to the writing mode.
    Inline,
  }
}

/// A [cursor image](https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#cursor) value, used in the `cursor` property.
///
/// See [Cursor](Cursor).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct CursorImage<'i> {
  /// A url to the cursor image.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub url: Url<'i>,
  /// The location in the image where the mouse pointer appears.
  pub hotspot: Option<(CSSNumber, CSSNumber)>,
}

impl<'i> Parse<'i> for CursorImage<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let url = Url::parse(input)?;
    let hotspot = if let Ok(x) = input.try_parse(CSSNumber::parse) {
      let y = CSSNumber::parse(input)?;
      Some((x, y))
    } else {
      None
    };

    Ok(CursorImage { url, hotspot })
  }
}

impl<'i> ToCss for CursorImage<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.url.to_css(dest)?;

    if let Some((x, y)) = self.hotspot {
      dest.write_char(' ')?;
      x.to_css(dest)?;
      dest.write_char(' ')?;
      y.to_css(dest)?;
    }
    Ok(())
  }
}

enum_property! {
  /// A pre-defined [cursor](https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#cursor) value,
  /// used in the `cursor` property.
  ///
  /// See [Cursor](Cursor).
  #[allow(missing_docs)]
  pub enum CursorKeyword {
    Auto,
    Default,
    None,
    ContextMenu,
    Help,
    Pointer,
    Progress,
    Wait,
    Cell,
    Crosshair,
    Text,
    VerticalText,
    Alias,
    Copy,
    Move,
    NoDrop,
    NotAllowed,
    Grab,
    Grabbing,
    EResize,
    NResize,
    NeResize,
    NwResize,
    SResize,
    SeResize,
    SwResize,
    WResize,
    EwResize,
    NsResize,
    NeswResize,
    NwseResize,
    ColResize,
    RowResize,
    AllScroll,
    ZoomIn,
    ZoomOut,
  }
}

/// A value for the [cursor](https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#cursor) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct Cursor<'i> {
  /// A list of cursor images.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub images: SmallVec<[CursorImage<'i>; 1]>,
  /// A pre-defined cursor.
  pub keyword: CursorKeyword,
}

impl<'i> Parse<'i> for Cursor<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut images = SmallVec::new();
    loop {
      match input.try_parse(CursorImage::parse) {
        Ok(image) => images.push(image),
        Err(_) => break,
      }
      input.expect_comma()?;
    }

    Ok(Cursor {
      images,
      keyword: CursorKeyword::parse(input)?,
    })
  }
}

impl<'i> ToCss for Cursor<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    for image in &self.images {
      image.to_css(dest)?;
      dest.delim(',', false)?;
    }
    self.keyword.to_css(dest)
  }
}

/// A value for the [caret-color](https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#caret-color) property.
#[derive(Debug, Clone, PartialEq, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum ColorOrAuto {
  /// The `currentColor`, adjusted by the UA to ensure contrast against the background.
  Auto,
  /// A color.
  Color(CssColor),
}

impl Default for ColorOrAuto {
  fn default() -> ColorOrAuto {
    ColorOrAuto::Auto
  }
}

impl FallbackValues for ColorOrAuto {
  fn get_fallbacks(&mut self, targets: Targets) -> Vec<Self> {
    match self {
      ColorOrAuto::Color(color) => color
        .get_fallbacks(targets)
        .into_iter()
        .map(|color| ColorOrAuto::Color(color))
        .collect(),
      ColorOrAuto::Auto => Vec::new(),
    }
  }
}

impl IsCompatible for ColorOrAuto {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    match self {
      ColorOrAuto::Color(color) => color.is_compatible(browsers),
      ColorOrAuto::Auto => true,
    }
  }
}

enum_property! {
  /// A value for the [caret-shape](https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#caret-shape) property.
  pub enum CaretShape {
    /// The UA determines the caret shape.
    Auto,
    /// A thin bar caret.
    Bar,
    /// A rectangle caret.
    Block,
    /// An underscore caret.
    Underscore,
  }
}

impl Default for CaretShape {
  fn default() -> CaretShape {
    CaretShape::Auto
  }
}

enum_property! {
  /// A value for the [caret-animation](https://drafts.csswg.org/css-ui-4/#caret-animation) property.
  pub enum CaretAnimation {
    /// The caret blinks.
    Auto,
    /// The caret does not blink.
    Manual,
  }
}

impl Default for CaretAnimation {
  fn default() -> CaretAnimation {
    CaretAnimation::Auto
  }
}

shorthand_property! {
  /// A value for the [caret](https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#caret) shorthand property.
  pub struct Caret {
    /// The caret color.
    color: CaretColor(ColorOrAuto),
    /// The caret animation.
    animation: CaretAnimation(CaretAnimation),
    /// The caret shape.
    shape: CaretShape(CaretShape),
  }
}

impl FallbackValues for Caret {
  fn get_fallbacks(&mut self, targets: Targets) -> Vec<Self> {
    self
      .color
      .get_fallbacks(targets)
      .into_iter()
      .map(|color| Caret {
        color,
        animation: self.animation.clone(),
        shape: self.shape.clone(),
      })
      .collect()
  }
}

impl IsCompatible for Caret {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    self.color.is_compatible(browsers)
  }
}

property_bitflags! {
  #[derive(Default, Debug)]
  struct CaretProperty: u8 {
    const CaretColor = 1 << 0;
    const CaretAnimation = 1 << 1;
    const CaretShape = 1 << 2;
    const Caret = 1 << 3;
  }
}

#[derive(Default)]
pub(crate) struct CaretHandler {
  color: Option<ColorOrAuto>,
  animation: Option<CaretAnimation>,
  shape: Option<CaretShape>,
  /// A `caret` shorthand kept as written, because targets do not support it.
  shorthand: Option<Caret>,
  flushed_properties: CaretProperty,
  has_any: bool,
}

/// Whether the `caret` shorthand can stand in for its longhands. No browser supports it yet,
/// so without targets the longhands and the shorthand are kept as written.
fn supports_caret_shorthand(context: &PropertyHandlerContext) -> bool {
  context.targets.browsers.map_or(false, |browsers| {
    compat::Feature::CaretShorthand.is_compatible(browsers)
  })
}

fn is_compatible<T: IsCompatible>(val: &T, context: &PropertyHandlerContext) -> bool {
  context.targets.browsers.map_or(true, |browsers| val.is_compatible(browsers))
}

impl<'i> PropertyHandler<'i> for CaretHandler {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    macro_rules! property {
      ($key: ident, $val: expr, $compatible: expr) => {{
        // Keep a previous value as a fallback if targets don't support the new one, and keep a
        // shorthand that was not expanded before the longhands that follow it.
        if self.shorthand.is_some() || (self.$key.is_some() && !$compatible) {
          self.flush(dest, context);
        }
        self.$key = Some($val.clone());
        self.has_any = true;
      }};
    }

    match property {
      Property::CaretColor(val) => property!(color, val, is_compatible(val, context)),
      Property::CaretAnimation(val) => property!(animation, val, true),
      Property::CaretShape(val) => property!(shape, val, true),
      Property::Caret(val) => {
        if supports_caret_shorthand(context) {
          property!(color, &val.color, is_compatible(&val.color, context));
          property!(animation, &val.animation, true);
          property!(shape, &val.shape, true);
        } else {
          // The shorthand is not expanded, so it must stay after the longhands before it.
          if self.color.is_some()
            || self.animation.is_some()
            || self.shape.is_some()
            || (self.shorthand.is_some() && !is_compatible(val, context))
          {
            self.flush(dest, context);
          }
          self.shorthand = Some(val.clone());
          self.has_any = true;
        }
      }
      Property::Unparsed(val)
        if matches!(
          val.property_id,
          PropertyId::CaretColor | PropertyId::CaretAnimation | PropertyId::CaretShape | PropertyId::Caret
        ) =>
      {
        self.flush(dest, context);
        let mut unparsed = val.clone();
        context.add_unparsed_fallbacks(&mut unparsed);
        self
          .flushed_properties
          .insert(CaretProperty::try_from(&unparsed.property_id).unwrap());
        dest.push(Property::Unparsed(unparsed));
      }
      _ => return false,
    }

    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    self.flush(dest, context);
    self.flushed_properties = CaretProperty::empty();
  }
}

impl CaretHandler {
  fn flush<'i>(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    if !self.has_any {
      return;
    }

    self.has_any = false;

    let color = std::mem::take(&mut self.color);
    let animation = std::mem::take(&mut self.animation);
    let shape = std::mem::take(&mut self.shape);
    let shorthand = std::mem::take(&mut self.shorthand);

    macro_rules! push {
      ($prop: ident, $val: expr, $flushed: expr) => {{
        let mut val = $val;
        // An earlier declaration of the property already serves as the fallback.
        if !self.flushed_properties.intersects($flushed) {
          for fallback in val.get_fallbacks(context.targets) {
            dest.push(Property::$prop(fallback));
          }
        }
        dest.push(Property::$prop(val));
        self.flushed_properties.insert($flushed);
      }};
    }

    if let Some(caret) = shorthand {
      push!(Caret, caret, CaretProperty::Caret);
    }

    match (color, animation, shape) {
      (Some(color), Some(animation), Some(shape)) if supports_caret_shorthand(context) => {
        push!(
          Caret,
          Caret {
            color,
            animation,
            shape
          },
          CaretProperty::all()
        );
      }
      (color, animation, shape) => {
        if let Some(color) = color {
          push!(CaretColor, color, CaretProperty::CaretColor);
        }
        if let Some(animation) = animation {
          dest.push(Property::CaretAnimation(animation));
        }
        if let Some(shape) = shape {
          dest.push(Property::CaretShape(shape));
        }
      }
    }
  }
}

enum_property! {
  /// A value for the [user-select](https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#content-selection) property.
  pub enum UserSelect {
    /// The UA determines whether text is selectable.
    Auto,
    /// Text is selectable.
    Text,
    /// Text is not selectable.
    None,
    /// Text selection is contained to the element.
    Contain,
    /// Only the entire element is selectable.
    All,
  }
}

/// A value for the [appearance](https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#appearance-switching) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[allow(missing_docs)]
pub enum Appearance<'i> {
  None,
  Auto,
  Textfield,
  MenulistButton,
  Button,
  Checkbox,
  Listbox,
  Menulist,
  Meter,
  ProgressBar,
  PushButton,
  Radio,
  Searchfield,
  SliderHorizontal,
  SquareButton,
  Textarea,
  NonStandard(CowArcStr<'i>),
}

impl<'i> Appearance<'i> {
  fn from_str(name: &str) -> Option<Self> {
    Some(match_ignore_ascii_case! { &name,
      "none" => Appearance::None,
      "auto" => Appearance::Auto,
      "textfield" => Appearance::Textfield,
      "menulist-button" => Appearance::MenulistButton,
      "button" => Appearance::Button,
      "checkbox" => Appearance::Checkbox,
      "listbox" => Appearance::Listbox,
      "menulist" => Appearance::Menulist,
      "meter" => Appearance::Meter,
      "progress-bar" => Appearance::ProgressBar,
      "push-button" => Appearance::PushButton,
      "radio" => Appearance::Radio,
      "searchfield" => Appearance::Searchfield,
      "slider-horizontal" => Appearance::SliderHorizontal,
      "square-button" => Appearance::SquareButton,
      "textarea" => Appearance::Textarea,
      _ => return None
    })
  }

  fn to_str(&self) -> &str {
    match self {
      Appearance::None => "none",
      Appearance::Auto => "auto",
      Appearance::Textfield => "textfield",
      Appearance::MenulistButton => "menulist-button",
      Appearance::Button => "button",
      Appearance::Checkbox => "checkbox",
      Appearance::Listbox => "listbox",
      Appearance::Menulist => "menulist",
      Appearance::Meter => "meter",
      Appearance::ProgressBar => "progress-bar",
      Appearance::PushButton => "push-button",
      Appearance::Radio => "radio",
      Appearance::Searchfield => "searchfield",
      Appearance::SliderHorizontal => "slider-horizontal",
      Appearance::SquareButton => "square-button",
      Appearance::Textarea => "textarea",
      Appearance::NonStandard(s) => s.as_ref(),
    }
  }
}

impl<'i> Parse<'i> for Appearance<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let ident = input.expect_ident()?;
    Ok(Self::from_str(ident.as_ref()).unwrap_or_else(|| Appearance::NonStandard(ident.into())))
  }
}

impl<'i> ToCss for Appearance<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.write_str(self.to_str())
  }
}

#[cfg(feature = "serde")]
#[cfg_attr(docsrs, doc(cfg(feature = "serde")))]
impl<'i> serde::Serialize for Appearance<'i> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    serializer.serialize_str(self.to_str())
  }
}

#[cfg(feature = "serde")]
#[cfg_attr(docsrs, doc(cfg(feature = "serde")))]
impl<'i, 'de: 'i> serde::Deserialize<'de> for Appearance<'i> {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: serde::Deserializer<'de>,
  {
    let s = CowArcStr::deserialize(deserializer)?;
    Ok(Self::from_str(s.as_ref()).unwrap_or_else(|| Appearance::NonStandard(s)))
  }
}

#[cfg(feature = "jsonschema")]
#[cfg_attr(docsrs, doc(cfg(feature = "jsonschema")))]
impl<'a> schemars::JsonSchema for Appearance<'a> {
  fn is_referenceable() -> bool {
    true
  }

  fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
    str::json_schema(gen)
  }

  fn schema_name() -> String {
    "Appearance".into()
  }
}

bitflags! {
  /// A value for the [color-scheme](https://drafts.csswg.org/css-color-adjust/#color-scheme-prop) property.
  #[cfg_attr(feature = "visitor", derive(Visit))]
  #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(from = "SerializedColorScheme", into = "SerializedColorScheme"))]
  #[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
  #[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
  pub struct ColorScheme: u8 {
    /// Indicates that the element supports a light color scheme.
    const Light    = 0b01;
    /// Indicates that the element supports a dark color scheme.
    const Dark     = 0b10;
    /// Forbids the user agent from overriding the color scheme for the element.
    const Only     = 0b100;
  }
}

impl<'i> Parse<'i> for ColorScheme {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut res = ColorScheme::empty();
    let mut has_any = false;

    if input.try_parse(|input| input.expect_ident_matching("normal")).is_ok() {
      return Ok(res);
    }

    if input.try_parse(|input| input.expect_ident_matching("only")).is_ok() {
      res |= ColorScheme::Only;
      has_any = true;
    }

    loop {
      if input.try_parse(|input| input.expect_ident_matching("light")).is_ok() {
        res |= ColorScheme::Light;
        has_any = true;
        continue;
      }

      if input.try_parse(|input| input.expect_ident_matching("dark")).is_ok() {
        res |= ColorScheme::Dark;
        has_any = true;
        continue;
      }

      break;
    }

    // Only is allowed at the start or the end.
    if !res.contains(ColorScheme::Only) && input.try_parse(|input| input.expect_ident_matching("only")).is_ok() {
      res |= ColorScheme::Only;
      has_any = true;
    }

    if has_any {
      return Ok(res);
    }

    Err(input.new_custom_error(ParserError::InvalidValue))
  }
}

impl ToCss for ColorScheme {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if self.is_empty() {
      return dest.write_str("normal");
    }

    if self.contains(ColorScheme::Light) {
      dest.write_str("light")?;
      if self.contains(ColorScheme::Dark) {
        dest.write_char(' ')?;
      }
    }

    if self.contains(ColorScheme::Dark) {
      dest.write_str("dark")?;
    }

    if self.contains(ColorScheme::Only) {
      // Avoid parsing `color-scheme: only` as `color-scheme:  only`
      if !self.intersects(ColorScheme::Light | ColorScheme::Dark) {
        return dest.write_str("only");
      }
      dest.write_str(" only")?;
    }

    Ok(())
  }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
struct SerializedColorScheme {
  light: bool,
  dark: bool,
  only: bool,
}

impl From<ColorScheme> for SerializedColorScheme {
  fn from(color_scheme: ColorScheme) -> Self {
    Self {
      light: color_scheme.contains(ColorScheme::Light),
      dark: color_scheme.contains(ColorScheme::Dark),
      only: color_scheme.contains(ColorScheme::Only),
    }
  }
}

impl From<SerializedColorScheme> for ColorScheme {
  fn from(s: SerializedColorScheme) -> ColorScheme {
    let mut color_scheme = ColorScheme::empty();
    color_scheme.set(ColorScheme::Light, s.light);
    color_scheme.set(ColorScheme::Dark, s.dark);
    color_scheme.set(ColorScheme::Only, s.only);
    color_scheme
  }
}

#[cfg(feature = "jsonschema")]
#[cfg_attr(docsrs, doc(cfg(feature = "jsonschema")))]
impl<'a> schemars::JsonSchema for ColorScheme {
  fn is_referenceable() -> bool {
    true
  }

  fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
    SerializedColorScheme::json_schema(gen)
  }

  fn schema_name() -> String {
    "ColorScheme".into()
  }
}

#[derive(Default)]
pub(crate) struct ColorSchemeHandler;

impl<'i> PropertyHandler<'i> for ColorSchemeHandler {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    match property {
      Property::ColorScheme(color_scheme) => {
        if should_compile!(context.targets, LightDark) {
          if color_scheme.contains(ColorScheme::Light) {
            dest.push(define_var("--lightningcss-light", Token::Ident("initial".into())));
            dest.push(define_var("--lightningcss-dark", Token::WhiteSpace(" ".into())));

            if color_scheme.contains(ColorScheme::Dark) {
              context.add_dark_rule(define_var("--lightningcss-light", Token::WhiteSpace(" ".into())));
              context.add_dark_rule(define_var("--lightningcss-dark", Token::Ident("initial".into())));
            }
          } else if color_scheme.contains(ColorScheme::Dark) {
            dest.push(define_var("--lightningcss-light", Token::WhiteSpace(" ".into())));
            dest.push(define_var("--lightningcss-dark", Token::Ident("initial".into())));
          }
        }
        dest.push(property.clone());
        true
      }
      _ => false,
    }
  }

  fn finalize(&mut self, _: &mut DeclarationList<'i>, _: &mut PropertyHandlerContext<'i, '_>) {}
}

enum_property! {
  /// A value for the [print-color-adjust](https://drafts.csswg.org/css-color-adjust/#propdef-print-color-adjust) property.
  pub enum PrintColorAdjust {
    /// The user agent is allowed to make adjustments to the element as it deems appropriate.
    Economy,
    /// The user agent is not allowed to make adjustments to the element.
    Exact,
  }
}

#[inline]
fn define_var<'i>(name: &'static str, value: Token<'static>) -> Property<'i> {
  Property::Custom(CustomProperty {
    name: CustomPropertyName::Custom(name.into()),
    value: TokenList(vec![TokenOrValue::Token(value)]),
  })
}
