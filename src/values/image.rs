//! CSS image values.

use super::color::ColorFallbackKind;
use super::gradient::*;
use super::resolution::Resolution;
use crate::compat;
use crate::dependencies::{Dependency, UrlDependency};
use crate::error::{ParserError, PrinterError};
use crate::prefixes::{is_webkit_gradient, Feature};
use crate::printer::Printer;
use crate::targets::{Browsers, Targets};
use crate::traits::{FallbackValues, IsCompatible, Parse, ToCss};
use crate::values::string::CowArcStr;
use crate::values::url::Url;
use crate::vendor_prefix::VendorPrefix;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;
use smallvec::SmallVec;

/// A CSS [`<image>`](https://www.w3.org/TR/css-images-3/#image-values) value.
#[derive(Debug, Clone, PartialEq, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "visitor", visit(visit_image, IMAGES))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum Image<'i> {
  /// The `none` keyword.
  None,
  /// A `url()`.
  #[cfg_attr(feature = "serde", serde(borrow))]
  Url(Url<'i>),
  /// A gradient.
  Gradient(Box<Gradient>),
  /// An `image-set()`.
  ImageSet(ImageSet<'i>),
}

impl<'i> Default for Image<'i> {
  fn default() -> Image<'i> {
    Image::None
  }
}

impl<'i> Image<'i> {
  /// Returns whether the image includes any vendor prefixed values.
  pub fn has_vendor_prefix(&self) -> bool {
    let prefix = self.get_vendor_prefix();
    !prefix.is_empty() && prefix != VendorPrefix::None
  }

  /// Returns the vendor prefix used in the image value.
  pub fn get_vendor_prefix(&self) -> VendorPrefix {
    match self {
      Image::Gradient(a) => a.get_vendor_prefix(),
      Image::ImageSet(a) => a.get_vendor_prefix(),
      _ => VendorPrefix::empty(),
    }
  }

  /// Returns the vendor prefixes that are needed for the given browser targets.
  pub fn get_necessary_prefixes(&self, targets: Targets) -> VendorPrefix {
    match self {
      Image::Gradient(grad) => grad.get_necessary_prefixes(targets),
      Image::ImageSet(image_set) => image_set.get_necessary_prefixes(targets),
      _ => VendorPrefix::None,
    }
  }

  /// Returns a vendor prefixed version of the image for the given vendor prefixes.
  pub fn get_prefixed(&self, prefix: VendorPrefix) -> Image<'i> {
    match self {
      Image::Gradient(grad) => Image::Gradient(Box::new(grad.get_prefixed(prefix))),
      Image::ImageSet(image_set) => Image::ImageSet(image_set.get_prefixed(prefix)),
      _ => self.clone(),
    }
  }

  /// Returns a legacy `-webkit-gradient()` value for the image.
  ///
  /// May return an error in case the gradient cannot be converted.
  pub fn get_legacy_webkit(&self) -> Result<Image<'i>, ()> {
    match self {
      Image::Gradient(grad) => Ok(Image::Gradient(Box::new(grad.get_legacy_webkit()?))),
      _ => Ok(self.clone()),
    }
  }

  /// Returns the color fallbacks that are needed for the given browser targets.
  pub fn get_necessary_fallbacks(&self, targets: Targets) -> ColorFallbackKind {
    match self {
      Image::Gradient(grad) => grad.get_necessary_fallbacks(targets),
      _ => ColorFallbackKind::empty(),
    }
  }

  /// Returns a fallback version of the image for the given color fallback type.
  pub fn get_fallback(&self, kind: ColorFallbackKind) -> Image<'i> {
    match self {
      Image::Gradient(grad) => Image::Gradient(Box::new(grad.get_fallback(kind))),
      _ => self.clone(),
    }
  }
}

impl<'i> IsCompatible for Image<'i> {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    match self {
      Image::Gradient(g) => match &**g {
        Gradient::Linear(g) => {
          compat::Feature::LinearGradient.is_compatible(browsers) && g.is_compatible(browsers)
        }
        Gradient::RepeatingLinear(g) => {
          compat::Feature::RepeatingLinearGradient.is_compatible(browsers) && g.is_compatible(browsers)
        }
        Gradient::Radial(g) => {
          compat::Feature::RadialGradient.is_compatible(browsers) && g.is_compatible(browsers)
        }
        Gradient::RepeatingRadial(g) => {
          compat::Feature::RepeatingRadialGradient.is_compatible(browsers) && g.is_compatible(browsers)
        }
        Gradient::Conic(g) => compat::Feature::ConicGradient.is_compatible(browsers) && g.is_compatible(browsers),
        Gradient::RepeatingConic(g) => {
          compat::Feature::RepeatingConicGradient.is_compatible(browsers) && g.is_compatible(browsers)
        }
        Gradient::WebKitGradient(..) => is_webkit_gradient(browsers),
      },
      Image::ImageSet(i) => i.is_compatible(browsers),
      Image::Url(..) | Image::None => true,
    }
  }
}

pub(crate) trait ImageFallback<'i>: Sized {
  fn get_image(&self) -> &Image<'i>;
  fn with_image(&self, image: Image<'i>) -> Self;

  #[inline]
  fn get_necessary_fallbacks(&self, targets: Targets) -> ColorFallbackKind {
    self.get_image().get_necessary_fallbacks(targets)
  }

  #[inline]
  fn get_fallback(&self, kind: ColorFallbackKind) -> Self {
    self.with_image(self.get_image().get_fallback(kind))
  }
}

impl<'i> ImageFallback<'i> for Image<'i> {
  #[inline]
  fn get_image(&self) -> &Image<'i> {
    self
  }

  #[inline]
  fn with_image(&self, image: Image<'i>) -> Self {
    image
  }
}

impl<'i> FallbackValues for Image<'i> {
  fn get_fallbacks(&mut self, targets: Targets) -> Vec<Self> {
    // Determine which prefixes and color fallbacks are needed.
    let prefixes = self.get_necessary_prefixes(targets);
    let fallbacks = self.get_necessary_fallbacks(targets);
    let mut res = Vec::new();

    // Get RGB fallbacks if needed.
    let rgb = if fallbacks.contains(ColorFallbackKind::RGB) {
      Some(self.get_fallback(ColorFallbackKind::RGB))
    } else {
      None
    };

    // Prefixed properties only support RGB.
    let prefix_image = rgb.as_ref().unwrap_or(self);

    // Legacy -webkit-gradient()
    if prefixes.contains(VendorPrefix::WebKit)
      && targets.browsers.map(is_webkit_gradient).unwrap_or(false)
      && matches!(prefix_image, Image::Gradient(_))
    {
      if let Ok(legacy) = prefix_image.get_legacy_webkit() {
        res.push(legacy);
      }
    }

    // Standard syntax, with prefixes.
    if prefixes.contains(VendorPrefix::WebKit) {
      res.push(prefix_image.get_prefixed(VendorPrefix::WebKit))
    }

    if prefixes.contains(VendorPrefix::Moz) {
      res.push(prefix_image.get_prefixed(VendorPrefix::Moz))
    }

    if prefixes.contains(VendorPrefix::O) {
      res.push(prefix_image.get_prefixed(VendorPrefix::O))
    }

    if prefixes.contains(VendorPrefix::None) {
      // Unprefixed, rgb fallback.
      if let Some(rgb) = rgb {
        res.push(rgb);
      }

      // P3 fallback.
      if fallbacks.contains(ColorFallbackKind::P3) {
        res.push(self.get_fallback(ColorFallbackKind::P3));
      }

      // Convert original to lab if needed (e.g. if oklab is not supported but lab is).
      if fallbacks.contains(ColorFallbackKind::LAB) {
        *self = self.get_fallback(ColorFallbackKind::LAB);
      }
    } else if let Some(last) = res.pop() {
      // Prefixed property with no unprefixed version.
      // Replace self with the last prefixed version so that it doesn't
      // get duplicated when the caller pushes the original value.
      *self = last;
    }

    res
  }
}

impl<'i, T: ImageFallback<'i>> FallbackValues for SmallVec<[T; 1]> {
  fn get_fallbacks(&mut self, targets: Targets) -> Vec<Self> {
    // Determine what vendor prefixes and color fallbacks are needed.
    let mut prefixes = VendorPrefix::empty();
    let mut fallbacks = ColorFallbackKind::empty();
    let mut res = Vec::new();
    for item in self.iter() {
      prefixes |= item.get_image().get_necessary_prefixes(targets);
      fallbacks |= item.get_necessary_fallbacks(targets);
    }

    // Get RGB fallbacks if needed.
    let rgb: Option<SmallVec<[T; 1]>> = if fallbacks.contains(ColorFallbackKind::RGB) {
      Some(self.iter().map(|item| item.get_fallback(ColorFallbackKind::RGB)).collect())
    } else {
      None
    };

    // Prefixed properties only support RGB.
    let prefix_images = rgb.as_ref().unwrap_or(&self);

    // Legacy -webkit-gradient()
    if prefixes.contains(VendorPrefix::WebKit) && targets.browsers.map(is_webkit_gradient).unwrap_or(false) {
      let images: SmallVec<[T; 1]> = prefix_images
        .iter()
        .map(|item| item.get_image().get_legacy_webkit().map(|image| item.with_image(image)))
        .flatten()
        .collect();
      if !images.is_empty() {
        res.push(images)
      }
    }

    // Standard syntax, with prefixes.
    macro_rules! prefix {
      ($prefix: ident) => {
        if prefixes.contains(VendorPrefix::$prefix) {
          let images = prefix_images
            .iter()
            .map(|item| {
              let image = item.get_image().get_prefixed(VendorPrefix::$prefix);
              item.with_image(image)
            })
            .collect();
          res.push(images)
        }
      };
    }

    prefix!(WebKit);
    prefix!(Moz);
    prefix!(O);
    if prefixes.contains(VendorPrefix::None) {
      if let Some(rgb) = rgb {
        res.push(rgb);
      }

      if fallbacks.contains(ColorFallbackKind::P3) {
        let p3_images = self.iter().map(|item| item.get_fallback(ColorFallbackKind::P3)).collect();

        res.push(p3_images)
      }

      // Convert to lab if needed (e.g. if oklab is not supported but lab is).
      if fallbacks.contains(ColorFallbackKind::LAB) {
        for item in self.iter_mut() {
          *item = item.get_fallback(ColorFallbackKind::LAB);
        }
      }
    } else if let Some(last) = res.pop() {
      // Prefixed property with no unprefixed version.
      // Replace self with the last prefixed version so that it doesn't
      // get duplicated when the caller pushes the original value.
      *self = last;
    }

    res
  }
}

/// A CSS [`image-set()`](https://drafts.csswg.org/css-images-4/#image-set-notation) value.
///
/// `image-set()` allows the user agent to choose between multiple versions of an image to
/// display the most appropriate resolution or file type that it supports.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct ImageSet<'i> {
  /// The image options to choose from.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub options: Vec<ImageSetOption<'i>>,
  /// The vendor prefix for the `image-set()` function.
  pub vendor_prefix: VendorPrefix,
}

impl<'i> ImageSet<'i> {
  /// Returns the vendor prefix for the `image-set()`.
  pub fn get_vendor_prefix(&self) -> VendorPrefix {
    self.vendor_prefix
  }

  /// Returns the vendor prefixes needed for the given browser targets.
  pub fn get_necessary_prefixes(&self, targets: Targets) -> VendorPrefix {
    targets.prefixes(self.vendor_prefix, Feature::ImageSet)
  }

  /// Returns the `image-set()` value with the given vendor prefix.
  pub fn get_prefixed(&self, prefix: VendorPrefix) -> ImageSet<'i> {
    ImageSet {
      options: self.options.clone(),
      vendor_prefix: prefix,
    }
  }
}

impl<'i> Parse<'i> for ImageSet<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let f = input.expect_function()?;
    let vendor_prefix = match_ignore_ascii_case! { &*f,
      "image-set" => VendorPrefix::None,
      "-webkit-image-set" => VendorPrefix::WebKit,
      _ => return Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(f.clone())
      ))
    };

    let options = input.parse_nested_block(|input| input.parse_comma_separated(ImageSetOption::parse))?;
    Ok(ImageSet { options, vendor_prefix })
  }
}

impl<'i> ToCss for ImageSet<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.vendor_prefix.to_css(dest)?;
    dest.write_str("image-set(")?;
    let mut first = true;
    for option in &self.options {
      if first {
        first = false;
      } else {
        dest.delim(',', false)?;
      }
      option.to_css(dest, self.vendor_prefix != VendorPrefix::None)?;
    }
    dest.write_char(')')
  }
}

impl<'i> IsCompatible for ImageSet<'i> {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    compat::Feature::ImageSet.is_compatible(browsers)
      && self.options.iter().all(|opt| opt.image.is_compatible(browsers))
  }
}

/// An image option within the `image-set()` function. See [ImageSet](ImageSet).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct ImageSetOption<'i> {
  /// The image for this option.
  #[cfg_attr(feature = "visitor", skip_type)]
  pub image: Image<'i>,
  /// The resolution of the image.
  pub resolution: Resolution,
  /// The mime type of the image.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub file_type: Option<CowArcStr<'i>>,
}

impl<'i> Parse<'i> for ImageSetOption<'i> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let loc = input.current_source_location();
    let image = if let Ok(url) = input.try_parse(|input| input.expect_url_or_string()) {
      Image::Url(Url {
        url: url.into(),
        loc: loc.into(),
      })
    } else {
      Image::parse(input)?
    };

    let (resolution, file_type) = if let Ok(res) = input.try_parse(Resolution::parse) {
      let file_type = input.try_parse(parse_file_type).ok();
      (res, file_type)
    } else {
      let file_type = input.try_parse(parse_file_type).ok();
      let resolution = input.try_parse(Resolution::parse).unwrap_or(Resolution::Dppx(1.0));
      (resolution, file_type)
    };

    Ok(ImageSetOption {
      image,
      resolution,
      file_type: file_type.map(|x| x.into()),
    })
  }
}

impl<'i> ImageSetOption<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>, is_prefixed: bool) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match &self.image {
      // Prefixed syntax didn't allow strings, only url()
      Image::Url(url) if !is_prefixed => {
        // Add dependency if needed. Normally this is handled by the Url type.
        let dep = if dest.dependencies.is_some() {
          Some(UrlDependency::new(url, dest.filename()))
        } else {
          None
        };
        if let Some(dep) = dep {
          serialize_string(&dep.placeholder, dest)?;
          if let Some(dependencies) = &mut dest.dependencies {
            dependencies.push(Dependency::Url(dep))
          }
        } else {
          serialize_string(&url.url, dest)?;
        }
      }
      _ => self.image.to_css(dest)?,
    }

    // TODO: Throwing an error when `self.resolution = Resolution::Dppx(0.0)`
    // TODO: -webkit-image-set() does not support `<image()> | <image-set()> |
    // <cross-fade()> | <element()> | <gradient>` and `type(<string>)`.
    dest.write_char(' ')?;

    // Safari only supports the x resolution unit in image-set().
    // In other places, x was added as an alias later.
    // Temporarily ignore the targets while printing here.
    let targets = std::mem::take(&mut dest.targets.current);
    self.resolution.to_css(dest)?;
    dest.targets.current = targets;

    if let Some(file_type) = &self.file_type {
      dest.write_str(" type(")?;
      serialize_string(&file_type, dest)?;
      dest.write_char(')')?;
    }

    Ok(())
  }
}

fn parse_file_type<'i, 't>(input: &mut Parser<'i, 't>) -> Result<CowRcStr<'i>, ParseError<'i, ParserError<'i>>> {
  input.expect_function_matching("type")?;
  input.parse_nested_block(|input| Ok(input.expect_string_cloned()?))
}
