use crate::targets::Browsers;
use crate::values::string::CowArcStr;
use cssparser::*;
use crate::traits::{Parse, ToCss, FallbackValues};
use crate::values::color::CssColor;
use crate::macros::{enum_property, shorthand_property};
use crate::printer::Printer;
use smallvec::SmallVec;
use crate::values::url::Url;
use crate::error::{ParserError, PrinterError};

enum_property! {
  /// https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#resize
  pub enum Resize {
    None,
    Both,
    Horizontal,
    Vertical,
    Block,
    Inline,
  }
}

/// https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#cursor
#[derive(Debug, Clone, PartialEq)]
pub struct CursorImage<'i> {
  pub url: Url<'i>,
  pub hotspot: Option<(f32, f32)>
}

impl<'i> Parse<'i> for CursorImage<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let url = Url::parse(input)?;
    let hotspot = if let Ok(x) = input.try_parse(f32::parse) {
      let y = f32::parse(input)?;
      Some((x, y))
    } else {
      None
    };

    Ok(CursorImage {
      url,
      hotspot
    })
  }
}

impl<'i> ToCss for CursorImage<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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

/// https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#cursor
#[derive(Debug, Clone, PartialEq)]
pub struct Cursor<'i> {
  pub images: SmallVec<[CursorImage<'i>; 1]>,
  pub keyword: CursorKeyword
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
      keyword: CursorKeyword::parse(input)?
    })
  }
}

impl<'i> ToCss for Cursor<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    for image in &self.images {
      image.to_css(dest)?;
      dest.delim(',', false)?;
    }
    self.keyword.to_css(dest)
  }
}

/// https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#caret-color
#[derive(Debug, Clone, PartialEq)]
pub enum ColorOrAuto {
  Auto,
  Color(CssColor)
}

impl Default for ColorOrAuto {
  fn default() -> ColorOrAuto {
    ColorOrAuto::Auto
  }
}

impl<'i> Parse<'i> for ColorOrAuto {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|input| input.expect_ident_matching("auto")).is_ok() {
      return Ok(ColorOrAuto::Auto)
    }

    let color = CssColor::parse(input)?;
    Ok(ColorOrAuto::Color(color))
  }
}

impl ToCss for ColorOrAuto {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    match self {
      ColorOrAuto::Auto => dest.write_str("auto"),
      ColorOrAuto::Color(color) => color.to_css(dest)
    }
  }
}

impl FallbackValues for ColorOrAuto {
  fn get_fallbacks(&mut self, targets: Browsers) -> Vec<Self> {
    match self {
      ColorOrAuto::Color(color) => {
        color.get_fallbacks(targets)
          .into_iter()
          .map(|color| ColorOrAuto::Color(color))
          .collect()
      }
      ColorOrAuto::Auto => Vec::new()
    }
  }
}

enum_property! {
  /// https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#caret-shape
  pub enum CaretShape {
    Auto,
    Bar,
    Block,
    Underscore,
  }
}

impl Default for CaretShape {
  fn default() -> CaretShape {
    CaretShape::Auto
  }
}

// https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#caret
shorthand_property!(Caret {
  color: ColorOrAuto,
  shape: CaretShape,
});

impl FallbackValues for Caret {
  fn get_fallbacks(&mut self, targets: Browsers) -> Vec<Self> {
   self.color.get_fallbacks(targets)
    .into_iter()
    .map(|color| Caret {
      color,
      shape: self.shape.clone()
    })
    .collect()
  }
}

enum_property! {
  /// https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#content-selection
  pub enum UserSelect {
    Auto,
    Text,
    None,
    Contain,
    All,
  }
}

/// https://www.w3.org/TR/2021/WD-css-ui-4-20210316/#appearance-switching
#[derive(Debug, Clone, PartialEq)]
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
  NonStandard(CowArcStr<'i>)
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
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
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
      Appearance::NonStandard(s) => dest.write_str(&s)
    }
  }
}
