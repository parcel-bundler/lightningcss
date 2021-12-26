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
    if !self.rules.0.is_empty() && (dest.targets.is_none() || Feature::CssNesting.is_compatible(dest.targets.unwrap())) {
      dest.add_mapping(self.loc);
      self.selectors.to_css_with_context(dest, context)?;
      dest.whitespace()?;
      dest.write_char('{')?;
      dest.indent();
      let len = self.declarations.declarations.len();
      for (i, decl) in self.declarations.declarations.iter().enumerate() {
        dest.newline()?;
        decl.to_css(dest)?;
        if i != len - 1 || !dest.minify {
          dest.write_char(';')?;
        }
      }

      if self.rules.0.len() > 0 && !dest.minify {
        dest.write_char('\n')?;
      }

      for rule in &self.rules.0 {
        dest.newline()?;
        rule.to_css(dest)?;
      }

      dest.dedent();
      dest.newline()?;
      dest.write_char('}')?;
    } else {
      let has_declarations = self.declarations.declarations.len() > 0 || self.rules.0.is_empty();

      // If there are any declarations in the rule, or no child rules, write the parent.
      if has_declarations {
        dest.add_mapping(self.loc);
        self.selectors.to_css_with_context(dest, context)?;
        self.declarations.to_css(dest)?;
      }

      // Write nested rules after the parent.
      let mut newline = has_declarations;
      for rule in &self.rules.0 {
        if newline {
          dest.newline()?;
        }
        rule.to_css_with_context(dest, Some(&StyleContext {
          rule: self,
          parent: context
        }))?;
        newline = true;
      }
    }
    
    Ok(())
  }
}
