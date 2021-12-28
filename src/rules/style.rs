use cssparser::SourceLocation;
use parcel_selectors::SelectorList;
use crate::selector::{Selectors, is_compatible};
use crate::traits::ToCss;
use crate::printer::Printer;
use crate::declaration::{DeclarationBlock, DeclarationHandler};
use crate::vendor_prefix::VendorPrefix;
use crate::targets::Browsers;
use crate::rules::{CssRuleList, ToCssWithContext, StyleContext};
use crate::compat::Feature;

#[derive(Debug, PartialEq)]
pub struct StyleRule {
  pub selectors: SelectorList<Selectors>,
  pub vendor_prefix: VendorPrefix,
  pub declarations: DeclarationBlock,
  pub rules: CssRuleList,
  pub loc: SourceLocation
}

impl StyleRule {
  pub(crate) fn minify(&mut self, handler: &mut DeclarationHandler, important_handler: &mut DeclarationHandler) {
    self.declarations.minify(handler, important_handler);
  }

  pub fn is_compatible(&self, targets: Option<Browsers>) -> bool {
    is_compatible(&self.selectors, targets)
  }
}

impl ToCssWithContext for StyleRule {
  fn to_css_with_context<W>(&self, dest: &mut Printer<W>, context: Option<&StyleContext>) -> std::fmt::Result where W: std::fmt::Write {
    if self.vendor_prefix.is_empty() {
      self.to_css_base(dest, context)
    } else {
      let mut first_rule = true;
      macro_rules! prefix {
        ($prefix: ident) => {
          if self.vendor_prefix.contains(VendorPrefix::$prefix) {
            #[allow(unused_assignments)]
            if first_rule {
              first_rule = false;
            } else {
              if !dest.minify {
                dest.write_char('\n')?; // no indent
              }
              dest.newline()?;
            }
            dest.vendor_prefix = VendorPrefix::$prefix;
            self.to_css_base(dest, context)?;
          }
        };
      }

      prefix!(WebKit);
      prefix!(Moz);
      prefix!(Ms);
      prefix!(O);
      prefix!(None);

      dest.vendor_prefix = VendorPrefix::empty();
      Ok(())
    }
  }
}

impl StyleRule {
  fn to_css_base<W>(&self, dest: &mut Printer<W>, context: Option<&StyleContext>) -> std::fmt::Result where W: std::fmt::Write {
    // If supported, or there are no targets, preserve nesting. Otherwise, write nested rules after parent.
    let supports_nesting = self.rules.0.is_empty() || dest.targets.is_none() || Feature::CssNesting.is_compatible(dest.targets.unwrap());
    let len = self.declarations.declarations.len();
    let has_declarations = supports_nesting || len > 0 || self.rules.0.is_empty();

    if has_declarations {
      dest.add_mapping(self.loc);
      self.selectors.to_css_with_context(dest, context)?;
      dest.whitespace()?;
      dest.write_char('{')?;
      dest.indent();

      for (i, decl) in self.declarations.declarations.iter().enumerate() {
        // The CSS modules `composes` property is handled specially, and omitted during printing.
        // We need to add the classes it references to the list for the selectors in this rule.
        if let crate::properties::Property::Composes(composes) = &decl.property {
          if let Some(css_module) = &mut dest.css_module {
            css_module.handle_composes(&self.selectors, &composes)
              .map_err(|_| std::fmt::Error)?; // TODO: error
            continue;
          }
        }

        dest.newline()?;
        decl.to_css(dest)?;
        if i != len - 1 || !dest.minify {
          dest.write_char(';')?;
        }
      }
    }

    macro_rules! newline {
      () => {
        if !dest.minify && (supports_nesting || len > 0) && !self.rules.0.is_empty() {
          if len > 0 {
            dest.write_char('\n')?;
          }
          dest.newline()?;
        }
      };
    }

    macro_rules! end {
      () => {
        if has_declarations {
          dest.dedent();
          dest.newline()?;
          dest.write_char('}')?;
        }
      };
    }

    // Write nested rules after the parent.
    if supports_nesting {
      newline!();
      self.rules.to_css(dest)?;
      end!();
    } else {
      end!();
      newline!();
      self.rules.to_css_with_context(dest, Some(&StyleContext {
        rule: self,
        parent: context
      }))?;
    }

    Ok(())
  }
}
