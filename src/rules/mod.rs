pub mod keyframes;
pub mod font_face;
pub mod page;
pub mod supports;
pub mod counter_style;
pub mod namespace;
pub mod import;
pub mod media;
pub mod style;

use media::MediaRule;
use import::ImportRule;
use style::StyleRule;
use keyframes::KeyframesRule;
use font_face::FontFaceRule;
use page::PageRule;
use supports::SupportsRule;
use counter_style::CounterStyleRule;
use namespace::NamespaceRule;
use crate::traits::ToCss;
use crate::printer::Printer;

#[derive(Debug, PartialEq)]
pub enum CssRule {
  Media(MediaRule),
  Import(ImportRule),
  Style(StyleRule),
  Keyframes(KeyframesRule),
  FontFace(FontFaceRule),
  Page(PageRule),
  Supports(SupportsRule),
  CounterStyle(CounterStyleRule),
  Namespace(NamespaceRule)
}

impl ToCss for CssRule {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> std::fmt::Result where W: std::fmt::Write {
    match self {
      CssRule::Media(media) => media.to_css(dest),
      CssRule::Import(import) => import.to_css(dest),
      CssRule::Style(style) => style.to_css(dest),
      CssRule::Keyframes(keyframes) => keyframes.to_css(dest),
      CssRule::FontFace(font_face) => font_face.to_css(dest),
      CssRule::Page(font_face) => font_face.to_css(dest),
      CssRule::Supports(supports) => supports.to_css(dest),
      CssRule::CounterStyle(counter_style) => counter_style.to_css(dest),
      CssRule::Namespace(namespace) => namespace.to_css(dest)
    }
  }
}
