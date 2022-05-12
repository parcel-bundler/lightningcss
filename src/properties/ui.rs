//! CSS properties related to user interface.

use crate::declaration::DeclarationBlock;
use crate::error::{ParserError, PrinterError};
use crate::macros::{define_shorthand, enum_property, shorthand_property};
use crate::printer::Printer;
use crate::properties::{Property, PropertyId};
use crate::targets::Browsers;
use crate::traits::{FallbackValues, Parse, Shorthand, ToCss};
use crate::values::color::CssColor;
use crate::values::number::CSSNumber;
use crate::values::string::CowArcStr;
use crate::values::url::Url;
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
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
  fn get_fallbacks(&mut self, targets: Browsers) -> Vec<Self> {
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
  fn get_fallbacks(&mut self, targets: Browsers) -> Vec<Self> {
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
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "kebab-case")
)]
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
  NonStandard(#[cfg_attr(feature = "serde", serde(borrow))] CowArcStr<'i>),
}

impl<'i> Parse<'i> for Appearance<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "none" => Ok(Appearance::None),
      "auto" => Ok(Appearance::Auto),
      "textfield" => Ok(Appearance::Textfield),
      "menulist-button" => Ok(Appearance::MenulistButton),
      "button" => Ok(Appearance::Button),
      "checkbox" => Ok(Appearance::Checkbox),
      "listbox" => Ok(Appearance::Listbox),
      "menulist" => Ok(Appearance::Menulist),
      "meter" => Ok(Appearance::Meter),
      "progress-bar" => Ok(Appearance::ProgressBar),
      "push-button" => Ok(Appearance::PushButton),
      "radio" => Ok(Appearance::Radio),
      "searchfield" => Ok(Appearance::Searchfield),
      "slider-horizontal" => Ok(Appearance::SliderHorizontal),
      "square-button" => Ok(Appearance::SquareButton),
      "textarea" => Ok(Appearance::Textarea),
      _ => Ok(Appearance::NonStandard(ident.into()))
    }
  }
}

impl<'i> ToCss for Appearance<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      Appearance::None => dest.write_str("none"),
      Appearance::Auto => dest.write_str("auto"),
      Appearance::Textfield => dest.write_str("textfield"),
      Appearance::MenulistButton => dest.write_str("menulist-button"),
      Appearance::Button => dest.write_str("button"),
      Appearance::Checkbox => dest.write_str("checkbox"),
      Appearance::Listbox => dest.write_str("listbox"),
      Appearance::Menulist => dest.write_str("menulist"),
      Appearance::Meter => dest.write_str("meter"),
      Appearance::ProgressBar => dest.write_str("progress-bar"),
      Appearance::PushButton => dest.write_str("push-button"),
      Appearance::Radio => dest.write_str("radio"),
      Appearance::Searchfield => dest.write_str("searchfield"),
      Appearance::SliderHorizontal => dest.write_str("slider-horizontal"),
      Appearance::SquareButton => dest.write_str("square-button"),
      Appearance::Textarea => dest.write_str("textarea"),
      Appearance::NonStandard(s) => dest.write_str(&s),
    }
  }
}
