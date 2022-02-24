use cssparser::*;
use crate::values::color::ColorFallbackKind;
use crate::values::{
  length::LengthPercentageOrAuto,
  position::*,
  color::CssColor,
  image::Image
};
use crate::targets::Browsers;
use crate::prefixes::{Feature, is_webkit_gradient};
use crate::traits::{Parse, ToCss, PropertyHandler};
use crate::macros::*;
use crate::properties::{Property, PropertyId, VendorPrefix};
use crate::declaration::DeclarationList;
use itertools::izip;
use crate::printer::Printer;
use smallvec::SmallVec;
use crate::error::{ParserError, PrinterError};
use crate::logical::LogicalProperties;

/// https://www.w3.org/TR/css-backgrounds-3/#background-size
#[derive(Debug, Clone, PartialEq)]
pub enum BackgroundSize {
  Explicit {
    width: LengthPercentageOrAuto,
    height: LengthPercentageOrAuto
  },
  Cover,
  Contain
}

impl Default for BackgroundSize {
  fn default() -> BackgroundSize {
    BackgroundSize::Explicit {
      width: LengthPercentageOrAuto::Auto,
      height: LengthPercentageOrAuto::Auto
    }
  }
}

impl<'i> Parse<'i> for BackgroundSize {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(width) = input.try_parse(LengthPercentageOrAuto::parse) {
      let height = input.try_parse(LengthPercentageOrAuto::parse).unwrap_or(LengthPercentageOrAuto::Auto);
      return Ok(BackgroundSize::Explicit { width, height });
    }

    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    Ok(match_ignore_ascii_case! { ident,
      "cover" => BackgroundSize::Cover,
      "contain" => BackgroundSize::Contain,
      _ => return Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    })
  }
}

impl ToCss for BackgroundSize {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    use BackgroundSize::*;

    match &self {
      Cover => dest.write_str("cover"),
      Contain => dest.write_str("contain"),
      Explicit {width, height} => {
        width.to_css(dest)?;
        if *height != LengthPercentageOrAuto::Auto {
          dest.write_str(" ")?;
          height.to_css(dest)?;
        }
        Ok(())
      }
    }
  }
}

enum_property! {
  /// https://www.w3.org/TR/css-backgrounds-3/#typedef-repeat-style
  pub enum BackgroundRepeatKeyword {
    "repeat": Repeat,
    "space": Space,
    "round": Round,
    "no-repeat": NoRepeat,
  }
}

/// https://www.w3.org/TR/css-backgrounds-3/#background-repeat
#[derive(Debug, Clone, PartialEq)]
pub struct BackgroundRepeat {
  pub x: BackgroundRepeatKeyword,
  pub y: BackgroundRepeatKeyword
}

impl Default for BackgroundRepeat {
  fn default() -> BackgroundRepeat {
    BackgroundRepeat {
      x: BackgroundRepeatKeyword::Repeat,
      y: BackgroundRepeatKeyword::Repeat
    }
  }
}

impl<'i> Parse<'i> for BackgroundRepeat {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    use BackgroundRepeatKeyword::*;
    let state = input.state();
    let ident = input.expect_ident()?;

    match_ignore_ascii_case! { ident,
      "repeat-x" => return Ok(BackgroundRepeat { x: Repeat, y: NoRepeat }),
      "repeat-y" => return Ok(BackgroundRepeat { x: NoRepeat, y: Repeat }),
      _ => {}
    }

    input.reset(&state);

    let x = BackgroundRepeatKeyword::parse(input)?;
    let y = input.try_parse(BackgroundRepeatKeyword::parse).unwrap_or(x.clone());
    Ok(BackgroundRepeat { x, y })
  }
}

impl ToCss for BackgroundRepeat {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    use BackgroundRepeatKeyword::*;
    match (&self.x, &self.y) {
      (Repeat, NoRepeat) => dest.write_str("repeat-x"),
      (NoRepeat, Repeat) => dest.write_str("repeat-y"),
      (x, y) => {
        x.to_css(dest)?;
        if y != x {
          dest.write_str(" ")?;
          y.to_css(dest)?;
        }
        Ok(())
      }
    }
  }
}

enum_property! {
  /// https://www.w3.org/TR/css-backgrounds-3/#background-attachment
  pub enum BackgroundAttachment {
    Scroll,
    Fixed,
    Local,
  }
}

impl Default for BackgroundAttachment {
  fn default() -> BackgroundAttachment {
    BackgroundAttachment::Scroll
  }
}

enum_property! {
  /// https://www.w3.org/TR/css-backgrounds-3/#typedef-box
  pub enum BackgroundBox {
    "border-box": BorderBox,
    "padding-box": PaddingBox,
    "content-box": ContentBox,
  }
}

enum_property! {
  /// https://drafts.csswg.org/css-backgrounds-4/#background-clip
  pub enum BackgroundClip {
    "border-box": BorderBox,
    "padding-box": PaddingBox,
    "content-box": ContentBox,
    "border": Border,
    "text": Text,
  }
}

impl PartialEq<BackgroundBox> for BackgroundClip {
  fn eq(&self, other: &BackgroundBox) -> bool {
    match (self, other) {
      (BackgroundClip::BorderBox, BackgroundBox::BorderBox) |
      (BackgroundClip::PaddingBox, BackgroundBox::PaddingBox) |
      (BackgroundClip::ContentBox, BackgroundBox::ContentBox) => true,
      _ => false
    }
  }
}

impl Into<BackgroundClip> for BackgroundBox {
  fn into(self) -> BackgroundClip {
    match self {
      BackgroundBox::BorderBox => BackgroundClip::BorderBox,
      BackgroundBox::PaddingBox => BackgroundClip::PaddingBox,
      BackgroundBox::ContentBox => BackgroundClip::ContentBox
    }
  }
}

impl Default for BackgroundClip {
  fn default() -> BackgroundClip {
    BackgroundClip::BorderBox
  }
}

impl BackgroundClip {
  fn is_background_box(&self) -> bool {
    matches!(self, BackgroundClip::BorderBox | BackgroundClip::PaddingBox | BackgroundClip::ContentBox)
  }
}

/// https://www.w3.org/TR/css-backgrounds-3/#background
#[derive(Debug, Clone, PartialEq)]
pub struct Background<'i> {
  pub image: Image<'i>,
  pub color: CssColor,
  pub position: Position,
  pub repeat: BackgroundRepeat,
  pub size: BackgroundSize,
  pub attachment: BackgroundAttachment,
  pub origin: BackgroundBox,
  pub clip: BackgroundClip
}

impl<'i> Parse<'i> for Background<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut color: Option<CssColor> = None;
    let mut position: Option<Position> = None;
    let mut size: Option<BackgroundSize> = None;
    let mut image: Option<Image> = None;
    let mut repeat: Option<BackgroundRepeat> = None;
    let mut attachment: Option<BackgroundAttachment> = None;
    let mut origin: Option<BackgroundBox> = None;
    let mut clip: Option<BackgroundClip> = None;

    loop {
      // TODO: only allowed on the last background.
      if color.is_none() {
        if let Ok(value) = input.try_parse(CssColor::parse) {
          color = Some(value);
          continue;
        }
      }

      if position.is_none() {
        if let Ok(value) = input.try_parse(Position::parse) {
          position = Some(value);

          size = input.try_parse(|input| {
            input.expect_delim('/')?;
            BackgroundSize::parse(input)
          }).ok();

          continue;
        }
      }

      if image.is_none() {
        if let Ok(value) = input.try_parse(Image::parse) {
          image = Some(value);
          continue;
        }
      }

      if repeat.is_none() {
        if let Ok(value) = input.try_parse(BackgroundRepeat::parse) {
          repeat = Some(value);
          continue;
        }
      }

      if attachment.is_none() {
        if let Ok(value) = input.try_parse(BackgroundAttachment::parse) {
          attachment = Some(value);
          continue;
        }
      }

      if origin.is_none() {
        if let Ok(value) = input.try_parse(BackgroundBox::parse) {
          origin = Some(value);
          continue;
        }
      }

      if clip.is_none() {
        if let Ok(value) = input.try_parse(BackgroundClip::parse) {
          clip = Some(value);
          continue;
        }
      }

      break
    }

    if clip.is_none() {
      if let Some(origin) = origin {
        clip = Some(origin.into());
      }
    }

    Ok(Background {
      image: image.unwrap_or_default(),
      color: color.unwrap_or_default(),
      position: position.unwrap_or_default(),
      repeat: repeat.unwrap_or_default(),
      size: size.unwrap_or_default(),
      attachment: attachment.unwrap_or_default(),
      origin: origin.unwrap_or(BackgroundBox::PaddingBox),
      clip: clip.unwrap_or(BackgroundClip::BorderBox)
    })
  }
}

impl<'i> ToCss for Background<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError> where W: std::fmt::Write {
    let mut has_output = false;

    if self.color != CssColor::default() {
      self.color.to_css(dest)?;
      has_output = true;
    }

    if self.image != Image::default() {
      if has_output {
        dest.write_str(" ")?;
      }
      self.image.to_css(dest)?;
      has_output = true;
    }

    if !self.position.is_zero() || self.size != BackgroundSize::default() {
      if has_output {
        dest.write_str(" ")?;
      }
      self.position.to_css(dest)?;

      if self.size != BackgroundSize::default() {
        dest.delim('/', true)?;
        self.size.to_css(dest)?;
      }

      has_output = true;
    }

    if self.repeat != BackgroundRepeat::default() {
      if has_output {
        dest.write_str(" ")?;
      }

      self.repeat.to_css(dest)?;
      has_output = true;
    }

    if self.attachment != BackgroundAttachment::default() {
      if has_output {
        dest.write_str(" ")?;
      }

      self.attachment.to_css(dest)?;
      has_output = true;
    }

    let output_padding_box = self.origin != BackgroundBox::PaddingBox || (self.clip != BackgroundBox::BorderBox && self.clip.is_background_box());
    if output_padding_box {
      if has_output {
        dest.write_str(" ")?;
      }

      self.origin.to_css(dest)?;
      has_output = true;
    }

    if (output_padding_box && self.clip != self.origin) || self.clip != BackgroundBox::BorderBox {
      if has_output {
        dest.write_str(" ")?;
      }

      self.clip.to_css(dest)?;
      has_output = true;
    }

    // If nothing was output, then this is the initial value, e.g. background: transparent
    if !has_output {
      if dest.minify {
        // `0 0` is the shortest valid background value
        self.position.to_css(dest)?;
      } else {
        dest.write_str("none")?;
      }
    }

    Ok(())
  }
}

#[derive(Default)]
pub(crate) struct BackgroundHandler<'i> {
  targets: Option<Browsers>,
  color: Option<CssColor>,
  images: Option<SmallVec<[Image<'i>; 1]>>,
  has_prefix: bool,
  x_positions: Option<SmallVec<[HorizontalPosition; 1]>>,
  y_positions: Option<SmallVec<[VerticalPosition; 1]>>,
  repeats: Option<SmallVec<[BackgroundRepeat; 1]>>,
  sizes: Option<SmallVec<[BackgroundSize; 1]>>,
  attachments: Option<SmallVec<[BackgroundAttachment; 1]>>,
  origins: Option<SmallVec<[BackgroundBox; 1]>>,
  clips: Option<SmallVec<[BackgroundClip; 1]>>,
  decls: Vec<Property<'i>>,
  has_any: bool
}

impl<'i> BackgroundHandler<'i> {
  pub fn new(targets: Option<Browsers>) -> Self {
    BackgroundHandler {
      targets,
      ..BackgroundHandler::default()
    }
  }
}

impl<'i> PropertyHandler<'i> for BackgroundHandler<'i> {
  fn handle_property(&mut self, property: &Property<'i>, dest: &mut DeclarationList<'i>, _: &mut LogicalProperties) -> bool {
    macro_rules! background_image {
      ($val: ident) => {
        // Store prefixed properties. Clear if we hit an unprefixed property and we have
        // targets. In this case, the necessary prefixes will be generated.
        self.has_prefix = $val.iter().any(|x| x.has_vendor_prefix());
        if self.has_prefix {
          self.decls.push(property.clone())
        } else if self.targets.is_some() {
          self.decls.clear();
        }
      };
    }

    match &property {
      Property::BackgroundColor(val) => self.color = Some(val.clone()),
      Property::BackgroundImage(val) => {
        background_image!(val);
        self.images = Some(val.clone())
      },
      Property::BackgroundPosition(val) => {
        self.x_positions = Some(val.iter().map(|p| p.x.clone()).collect());
        self.y_positions = Some(val.iter().map(|p| p.y.clone()).collect());
      },
      Property::BackgroundPositionX(val) => self.x_positions = Some(val.clone()),
      Property::BackgroundPositionY(val) => self.y_positions = Some(val.clone()),
      Property::BackgroundRepeat(val) => self.repeats = Some(val.clone()),
      Property::BackgroundSize(val) => self.sizes = Some(val.clone()),
      Property::BackgroundAttachment(val) => self.attachments = Some(val.clone()),
      Property::BackgroundOrigin(val) => self.origins = Some(val.clone()),
      Property::BackgroundClip(val, vendor_prefix) => {
        if *vendor_prefix == VendorPrefix::None {
          self.clips = Some(val.clone());
        } else {
          self.flush(dest);
          dest.push(property.clone())
        }
      },
      Property::Background(val) => {
        let images: SmallVec<[Image; 1]> = val.iter().map(|b| b.image.clone()).collect();
        background_image!(images);
        self.color = Some(val.last().unwrap().color.clone());
        self.images = Some(images);
        self.x_positions = Some(val.iter().map(|b| b.position.x.clone()).collect());
        self.y_positions = Some(val.iter().map(|b| b.position.y.clone()).collect());
        self.repeats = Some(val.iter().map(|b| b.repeat.clone()).collect());
        self.sizes = Some(val.iter().map(|b| b.size.clone()).collect());
        self.attachments = Some(val.iter().map(|b| b.attachment.clone()).collect());
        self.origins = Some(val.iter().map(|b| b.origin.clone()).collect());
        self.clips = Some(val.iter().map(|b| b.clip.clone()).collect());
      }
      Property::Unparsed(val) if is_background_property(&val.property_id) => {
        self.flush(dest);
        dest.push(property.clone())
      }
      _ => return false
    }

    self.has_any = true;
    true
  }

  fn finalize(&mut self, dest: &mut DeclarationList<'i>, _: &mut LogicalProperties) {
    // If the last declaration is prefixed, pop the last value
    // so it isn't duplicated when we flush.
    if self.has_prefix {
      self.decls.pop();
    }

    dest.extend(self.decls.drain(..));
    self.flush(dest);
  }
}

impl<'i> BackgroundHandler<'i> {
  fn flush(&mut self, dest: &mut DeclarationList<'i>) {    
    if !self.has_any {
      return
    }

    self.has_any = false;

    let color = std::mem::take(&mut self.color);
    let mut images = std::mem::take(&mut self.images);
    let mut x_positions = std::mem::take(&mut self.x_positions);
    let mut y_positions = std::mem::take(&mut self.y_positions);
    let mut repeats = std::mem::take(&mut self.repeats);
    let mut sizes = std::mem::take(&mut self.sizes);
    let mut attachments = std::mem::take(&mut self.attachments);
    let mut origins = std::mem::take(&mut self.origins);
    let mut clips = std::mem::take(&mut self.clips);

    if let (Some(color), Some(images), Some(x_positions), Some(y_positions), Some(repeats), Some(sizes), Some(attachments), Some(origins), Some(clips)) = (&color, &mut images, &mut x_positions, &mut y_positions, &mut repeats, &mut sizes, &mut attachments, &mut origins, &mut clips) {
      // Only use shorthand syntax if the number of layers matches on all properties.
      let len = images.len();
      if x_positions.len() == len && y_positions.len() == len && repeats.len() == len && sizes.len() == len && attachments.len() == len && origins.len() == len && clips.len() == len {
        let (prefixes, clip_prefixes) = if let Some(targets) = self.targets {
          let mut prefixes = VendorPrefix::empty();
          for image in images.iter() {
            prefixes |= image.get_necessary_prefixes(targets);
          }
          let clip_prefixes = if clips.iter().any(|clip| *clip == BackgroundClip::Text) {
            Feature::BackgroundClip.prefixes_for(targets)
          } else {
            VendorPrefix::None
          };
          (prefixes, clip_prefixes)
        } else {
          (VendorPrefix::None, VendorPrefix::None)
        };

        let clip_property = if clip_prefixes != VendorPrefix::None {
          Some(Property::BackgroundClip(clips.clone(), clip_prefixes))
        } else {
          None
        };

        let backgrounds: SmallVec<[Background<'i>; 1]> = izip!(images.drain(..), x_positions.drain(..), y_positions.drain(..), repeats.drain(..), sizes.drain(..), attachments.drain(..), origins.drain(..), clips.drain(..)).enumerate().map(|(i, (image, x_position, y_position, repeat, size, attachment, origin, clip))| {
          Background {
            color: if i == len - 1 {
              color.clone()
            } else {
              CssColor::default()
            },
            image,
            position: Position {
              x: x_position,
              y: y_position
            },
            repeat,
            size,
            attachment,
            origin,
            clip: if clip_prefixes == VendorPrefix::None {
              clip
            } else {
              BackgroundClip::default()
            }
          }
        }).collect();

        if let Some(targets) = self.targets {
          // Determine which color fallbacks are needed in case of
          // new color types like lab.
          let mut fallbacks = ColorFallbackKind::empty();
          for background in &backgrounds {
            fallbacks |= background.image.get_necessary_fallbacks(targets)
          }

          // Get RGB fallbacks if needed.
          let rgb_backgrounds = if fallbacks.contains(ColorFallbackKind::RGB) {
            Some(backgrounds
              .iter()
              .map(|bg| Background { image: bg.image.get_fallback(ColorFallbackKind::RGB), ..bg.clone() })
              .collect())
          } else {
            None
          };

          // Prefixed properties only support RGB.
          let prefix_backgrounds = rgb_backgrounds.as_ref().unwrap_or(&backgrounds);

          // Legacy -webkit-gradient()
          if prefixes.contains(VendorPrefix::WebKit) && is_webkit_gradient(targets) && prefix_backgrounds.iter().any(|bg| matches!(bg.image, Image::Gradient(_))) {
            let backgrounds: SmallVec<[Background<'i>; 1]> = prefix_backgrounds
              .iter()
              .map(|bg| -> Result<Background<'i>, ()> { Ok(Background { image: bg.image.get_legacy_webkit()?, ..bg.clone() })})
              .flatten()
              .collect();
            if !backgrounds.is_empty() {
              dest.push(Property::Background(backgrounds))
            }
          }

          // Standard syntax, with prefixes.
          macro_rules! prefix {
            ($prefix: ident) => {
              if prefixes.contains(VendorPrefix::$prefix) {
                let backgrounds = prefix_backgrounds
                  .iter()
                  .map(|bg| Background { image: bg.image.get_prefixed(VendorPrefix::$prefix), ..bg.clone() })
                  .collect();
                dest.push(Property::Background(backgrounds))
              }
            };
          }

          prefix!(WebKit);
          prefix!(Moz);
          prefix!(O);

          if prefixes.contains(VendorPrefix::None) {
            if let Some(rgb_backgrounds) = rgb_backgrounds {
              dest.push(Property::Background(rgb_backgrounds));
            }

            if fallbacks.contains(ColorFallbackKind::P3) {
              let p3_backgrounds = backgrounds
                .iter()
                .map(|bg| Background { image: bg.image.get_fallback(ColorFallbackKind::P3), ..bg.clone() })
                .collect();
  
              dest.push(Property::Background(p3_backgrounds))
            }

            // Convert to lab if needed (e.g. if oklab is not supported but lab is).
            let backgrounds = if fallbacks.contains(ColorFallbackKind::LAB) {
              backgrounds
                .iter()
                .map(|bg| Background { image: bg.image.get_fallback(ColorFallbackKind::LAB), ..bg.clone() })
                .collect()
            } else {
              backgrounds
            };

            dest.push(Property::Background(backgrounds))
          }
  
        } else {
          dest.push(Property::Background(backgrounds));
        }

        if let Some(clip) = clip_property {
          dest.push(clip)
        }

        self.reset();
        return
      }
    }

    if let Some(mut color) = color {
      if let Some(targets) = self.targets {
        for fallback in color.get_fallbacks(targets) {
          dest.push(Property::BackgroundColor(fallback))
        }
      }

      dest.push(Property::BackgroundColor(color))
    }

    if let Some(images) = images {
      if let Some(targets) = self.targets {
        // Determine what vendor prefixes and color fallbacks are needed.
        let mut prefixes = VendorPrefix::empty();
        let mut fallbacks = ColorFallbackKind::empty();
        for image in images.iter() {
          prefixes |= image.get_necessary_prefixes(targets);
          fallbacks |= image.get_necessary_fallbacks(targets);
        }

        // Get RGB fallbacks if needed.
        let rgb = if fallbacks.contains(ColorFallbackKind::RGB) {
          Some(images
            .iter()
            .map(|image| image.get_fallback(ColorFallbackKind::RGB))
            .collect())
        } else {
          None
        };

        // Prefixed properties only support RGB.
        let prefix_images = rgb.as_ref().unwrap_or(&images);
      
        // Legacy -webkit-gradient()
        if prefixes.contains(VendorPrefix::WebKit) && is_webkit_gradient(targets) {
          let images: SmallVec<[Image<'i>; 1]> = prefix_images.iter().map(|image| image.get_legacy_webkit()).flatten().collect();
          if !images.is_empty() {
            dest.push(Property::BackgroundImage(images))
          }
        }

        // Standard syntax, with prefixes.
        macro_rules! prefix {
          ($prefix: ident) => {
            if prefixes.contains(VendorPrefix::$prefix) {
              let images = prefix_images.iter().map(|image| image.get_prefixed(VendorPrefix::$prefix)).collect();
              dest.push(Property::BackgroundImage(images))
            }
          };
        }

        prefix!(WebKit);
        prefix!(Moz);
        prefix!(O);
        if prefixes.contains(VendorPrefix::None) {
          if let Some(rgb) = rgb {
            dest.push(Property::BackgroundImage(rgb));
          }

          if fallbacks.contains(ColorFallbackKind::P3) {
            let p3_images = images
              .iter()
              .map(|image| image.get_fallback(ColorFallbackKind::P3))
              .collect();

            dest.push(Property::BackgroundImage(p3_images))
          }

          // Convert to lab if needed (e.g. if oklab is not supported but lab is).
          let images = if fallbacks.contains(ColorFallbackKind::LAB) {
            images
              .iter()
              .map(|image| image.get_fallback(ColorFallbackKind::LAB))
              .collect()
          } else {
            images
          };

          dest.push(Property::BackgroundImage(images))
        }
      } else {
        dest.push(Property::BackgroundImage(images))
      }
    }

    match (&mut x_positions, &mut y_positions) {
      (Some(x_positions), Some(y_positions)) if x_positions.len() == y_positions.len() => {
        let positions = izip!(x_positions.drain(..), y_positions.drain(..)).map(|(x, y)| Position {x, y}).collect();
        dest.push(Property::BackgroundPosition(positions))
      }
      _ => {
        if let Some(x_positions) = x_positions {
          dest.push(Property::BackgroundPositionX(x_positions))
        }
  
        if let Some(y_positions) = y_positions {
          dest.push(Property::BackgroundPositionY(y_positions))
        }
      }
    }

    if let Some(repeats) = repeats {
      dest.push(Property::BackgroundRepeat(repeats))
    }

    if let Some(sizes) = sizes {
      dest.push(Property::BackgroundSize(sizes))
    }

    if let Some(attachments) = attachments {
      dest.push(Property::BackgroundAttachment(attachments))
    }

    if let Some(origins) = origins {
      dest.push(Property::BackgroundOrigin(origins))
    }

    if let Some(clips) = clips {
      let prefixes = if let Some(targets) = self.targets {
        if clips.iter().any(|clip| *clip == BackgroundClip::Text) {
          Feature::BackgroundClip.prefixes_for(targets)
        } else {
          VendorPrefix::None
        }
      } else {
        VendorPrefix::None
      };
      dest.push(Property::BackgroundClip(clips, prefixes))
    }

    self.reset();
  }

  fn reset(&mut self) {
    self.color = None;
    self.images = None;
    self.x_positions = None;
    self.y_positions = None;
    self.repeats = None;
    self.sizes = None;
    self.attachments = None;
    self.origins = None;
    self.clips = None
  }
}

#[inline]
fn is_background_property(property_id: &PropertyId) -> bool {
  match property_id {
    PropertyId::BackgroundColor |
    PropertyId::BackgroundImage |
    PropertyId::BackgroundPosition |
    PropertyId::BackgroundPositionX |
    PropertyId::BackgroundPositionY |
    PropertyId::BackgroundRepeat |
    PropertyId::BackgroundSize |
    PropertyId::BackgroundAttachment |
    PropertyId::BackgroundOrigin |
    PropertyId::BackgroundClip(_) |
    PropertyId::Background => true,
    _ => false
  }
}
