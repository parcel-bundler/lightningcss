//! The `@nest` rule.

use smallvec::SmallVec;

use super::style::StyleRule;
use super::Location;
use super::MinifyContext;
use crate::context::DeclarationContext;
use crate::declaration::DeclarationBlock;
use crate::error::{MinifyError, PrinterError};
use crate::parser::DefaultAtRule;
use crate::printer::Printer;
use crate::targets::should_compile;
use crate::traits::ToCss;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;

/// A [@nest](https://www.w3.org/TR/css-nesting-1/#at-nest) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct NestingRule<'i, R = DefaultAtRule> {
  /// The style rule that defines the selector and declarations for the `@nest` rule.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub style: StyleRule<'i, R>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i, T: Clone> NestingRule<'i, T> {
  pub(crate) fn minify(
    &mut self,
    context: &mut MinifyContext<'_, 'i>,
    parent_is_unused: bool,
  ) -> Result<bool, MinifyError> {
    self.style.minify(context, parent_is_unused)
  }
}

impl<'a, 'i, T: ToCss> ToCss for NestingRule<'i, T> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);
    if dest.context().is_none() {
      dest.write_str("@nest ")?;
    }
    self.style.to_css(dest)
  }
}

/// A [nested declarations](https://drafts.csswg.org/css-nesting/#nested-declarations-rule) rule.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct NestedDeclarationsRule<'i> {
  /// The style rule that defines the selector and declarations for the `@nest` rule.
  #[cfg_attr(feature = "serde", serde(borrow))]
  pub declarations: DeclarationBlock<'i>,
  /// The location of the rule in the source file.
  #[cfg_attr(feature = "visitor", skip_visit)]
  pub loc: Location,
}

impl<'i> NestedDeclarationsRule<'i> {
  pub(crate) fn minify(&mut self, context: &mut MinifyContext<'_, 'i>, parent_is_unused: bool) -> bool {
    if parent_is_unused {
      return true;
    }

    context.handler_context.context = DeclarationContext::StyleRule;
    self
      .declarations
      .minify(context.handler, context.important_handler, &mut context.handler_context);
    context.handler_context.context = DeclarationContext::None;
    return false;
  }
}

impl<'i> ToCss for NestedDeclarationsRule<'i> {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    #[cfg(feature = "sourcemap")]
    dest.add_mapping(self.loc);

    if should_compile!(dest.targets.current, Nesting) {
      if let Some(context) = dest.context() {
        let has_printable_declarations = self.declarations.has_printable_declarations();
        if has_printable_declarations {
          dest.with_parent_context(|dest| context.selectors.to_css(dest))?;
          dest.whitespace()?;
          dest.write_char('{')?;
          dest.indent();
          dest.newline()?;
        }

        self
          .declarations
          .to_css_declarations(dest, false, &context.selectors, self.loc.source_index)?;

        if has_printable_declarations {
          dest.dedent();
          dest.newline()?;
          dest.write_char('}')?;
        }
        return Ok(());
      }
    }

    self
      .declarations
      .to_css_declarations(dest, false, &parcel_selectors::SelectorList(SmallVec::new()), 0)
  }
}
