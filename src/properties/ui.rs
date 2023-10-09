//! CSS properties related to user interface.

use crate::declaration::DeclarationBlock;
use crate::error::{ParserError, PrinterError};
use crate::macros::{define_shorthand, enum_property, shorthand_property};
use crate::printer::Printer;
use crate::properties::{Property, PropertyId};
use crate::targets::{Browsers, Targets};
use crate::traits::{FallbackValues, IsCompatible, Parse, Shorthand, ToCss};
use crate::values::color::CssColor;
use crate::values::number::CSSNumber;
use crate::values::string::CowArcStr;
use crate::values::url::Url;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;
use smallvec::SmallVec;

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
#[cfg_attr(feature = "into_owned", derive(lightningcss_derive::IntoOwned))]
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
    "auto": Auto,
    "default": Default,
    "none": None,
    "context-menu": ContextMenu,
    "help": Help,
    "pointer": Pointer,
    "progress": Progress,
    "wait": Wait,
    "cell": Cell,
    "crosshair": Crosshair,
    "text": Text,
    "vertical-text": VerticalText,
    "alias": Alias,
    "copy": Copy,
    "move": Move,
    "no-drop": NoDrop,
    "not-allowed": NotAllowed,
    "grab": Grab,
    "grabbing": Grabbing,
    "e-resize": EResize,
    "n-resize": NResize,
    "ne-resize": NeResize,
    "nw-resize": NwResize,
    "s-resize": SResize,
    "se-resize": SeResize,
    "sw-resize": SwResize,
    "w-resize": WResize,
    "ew-resize": EwResize,
    "ns-resize": NsResize,
    "nesw-resize": NeswResize,
    "nwse-resize": NwseResize,
    "col-resize": ColResize,
    "row-resize": RowResize,
    "all-scroll": AllScroll,
    "zoom-in": ZoomIn,
    "zoom-out": ZoomOut,
  }
}

/// A value for the [cursor](https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#cursor) property.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(lightningcss_derive::IntoOwned))]
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
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
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

impl<'i> Parse<'i> for ColorOrAuto {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("auto")).is_ok() {
      return Ok(ColorOrAuto::Auto);
    }

    let color = CssColor::parse(input)?;
    Ok(ColorOrAuto::Color(color))
  }
}

impl ToCss for ColorOrAuto {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      ColorOrAuto::Auto => dest.write_str("auto"),
      ColorOrAuto::Color(color) => color.to_css(dest),
    }
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

shorthand_property! {
  /// A value for the [caret](https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#caret) shorthand property.
  pub struct Caret {
    /// The caret color.
    color: CaretColor(ColorOrAuto),
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
#[cfg_attr(feature = "into_owned", derive(lightningcss_derive::IntoOwned))]
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
