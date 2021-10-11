use cssparser::*;
use super::traits::Parse;

#[derive(Debug, Clone, PartialEq)]
pub struct CssColor(Color);

impl CssColor {
  pub fn current_color() -> CssColor {
    CssColor(Color::CurrentColor)
  }
}

impl Parse for CssColor {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ()>> {
    Color::parse(input)
      .map(CssColor)
      .map_err(|_| input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))
  }
}

impl ToCss for CssColor {
  fn to_css<W>(&self, dest: &mut W) -> std::fmt::Result where W: std::fmt::Write {
    match self.0 {
      Color::CurrentColor => dest.write_str("currentColor"),
      Color::RGBA(color) => {
        if color.alpha == 255 {
          let hex: u32 = ((color.red as u32) << 16) | ((color.green as u32) << 8) | (color.blue as u32);
          let compact = compact_hex(hex);
          if hex == expand_hex(compact) {
            write!(dest, "#{:03x}", compact);
          } else {
            write!(dest, "#{:06x}", hex);
          }
        } else {
          let hex: u32 = ((color.red as u32) << 24) | ((color.green as u32) << 16) | ((color.blue as u32) << 8) | (color.alpha as u32);
          let compact = compact_hex(hex);
          if hex == expand_hex(compact) {
            write!(dest, "#{:04x}", compact);
          } else {
            write!(dest, "#{:08x}", hex);
          }
        }
        Ok(())
      }
    }
  }
}

// From esbuild: https://github.com/evanw/esbuild/blob/18e13bdfdca5cd3c7a2fae1a8bd739f8f891572c/internal/css_parser/css_decls_color.go#L218
// 0xAABBCCDD => 0xABCD
fn compact_hex(v: u32) -> u32 {
	return ((v & 0x0FF00000) >> 12) | ((v & 0x00000FF0) >> 4)
}

// 0xABCD => 0xAABBCCDD
fn expand_hex(v: u32) -> u32 {
	return ((v & 0xF000) << 16) | ((v & 0xFF00) << 12) | ((v & 0x0FF0) << 8) | ((v & 0x00FF) << 4) | (v & 0x000F)
}
