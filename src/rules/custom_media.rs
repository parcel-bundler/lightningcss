//! The `@custom-media` rule.

use super::Location;
use crate::error::PrinterError;
use crate::media_query::MediaList;
use crate::printer::Printer;
use crate::traits::ToCss;
use crate::values::ident::DashedIdent;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;

/// A [@custom-media](https://drafts.csswg.org/mediaqueries-5/#custom-mq) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct CustomMediaRule<'i> {
  /// The name of the declared media query.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub name: DashedIdent<'i>,
  /// The media query to declare.
  pub query: MediaList<'i>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i> ToCss for CustomMediaRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    dest.write_str("@custom-media ")?;
    self.name.to_css(dest)?;
    dest.write_char(' ')?;
    self.query.to_css(dest)?;
    dest.write_char(';')
  }
}
