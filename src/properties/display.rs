//! CSS properties related to display.

use super::custom::UnparsedProperty;
use super::{Property, PropertyId};
use crate::context::PropertyHandlerContext;
use crate::declaration::DeclarationList;
use crate::error::{ParserError, PrinterError};
use crate::macros::enum_property;
use crate::prefixes::{is_flex_2009, Feature};
use crate::printer::Printer;
use crate::traits::{Parse, PropertyHandler, ToCss};
use crate::vendor_prefix::VendorPrefix;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

enum_property! {
  /// A [`<display-outside>`](https://drafts.csswg.org/css-display-3/#typedef-display-outside) value.
  #[allow(missing_docs)]
  pub enum DisplayOutside {
    Block,
    Inline,
    RunIn,
  }
}

/// A [`<display-inside>`](https://drafts.csswg.org/css-display-3/#typedef-display-inside) value.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "vendorPrefix", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[allow(missing_docs)]
pub enum DisplayInside {
  Flow,
  FlowRoot,
  Table,
  Flex(VendorPrefix),
  Box(VendorPrefix),
  Grid,
  Ruby,
}

impl<'i> Parse<'i> for DisplayInside {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "flow" => Ok(DisplayInside::Flow),
      "flow-root" => Ok(DisplayInside::FlowRoot),
      "table" => Ok(DisplayInside::Table),
      "flex" => Ok(DisplayInside::Flex(VendorPrefix::None)),
      "-webkit-flex" => Ok(DisplayInside::Flex(VendorPrefix::WebKit)),
      "-ms-flexbox" => Ok(DisplayInside::Flex(VendorPrefix::Ms)),
      "-webkit-box" => Ok(DisplayInside::Box(VendorPrefix::WebKit)),
      "-moz-box" => Ok(DisplayInside::Box(VendorPrefix::Moz)),
      "grid" => Ok(DisplayInside::Grid),
      "ruby" => Ok(DisplayInside::Ruby),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for DisplayInside {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      DisplayInside::Flow => dest.write_str("flow"),
      DisplayInside::FlowRoot => dest.write_str("flow-root"),
      DisplayInside::Table => dest.write_str("table"),
      DisplayInside::Flex(prefix) => {
        prefix.to_css(dest)?;
        if *prefix == VendorPrefix::Ms {
          dest.write_str("flexbox")
        } else {
          dest.write_str("flex")
        }
      }
      DisplayInside::Box(prefix) => {
        prefix.to_css(dest)?;
        dest.write_str("box")
      }
      DisplayInside::Grid => dest.write_str("grid"),
      DisplayInside::Ruby => dest.write_str("ruby"),
    }
  }
}

impl DisplayInside {
  fn is_equivalent(&self, other: &DisplayInside) -> bool {
    match (self, other) {
      (DisplayInside::Flex(_), DisplayInside::Flex(_)) => true,
      (DisplayInside::Box(_), DisplayInside::Box(_)) => true,
      (DisplayInside::Flex(_), DisplayInside::Box(_)) => true,
      (DisplayInside::Box(_), DisplayInside::Flex(_)) => true,
      _ => self == other,
    }
  }
}

/// A pair of inside and outside display values, as used in the `display` property.
///
/// See [Display](Display).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct DisplayPair {
  /// The outside display value.
  pub outside: DisplayOutside,
  /// The inside display value.
  pub inside: DisplayInside,
  /// Whether this is a list item.
  pub is_list_item: bool,
}

impl<'i> Parse<'i> for DisplayPair {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let mut list_item = false;
    let mut outside = None;
    let mut inside = None;

    loop {
      if input.try_parse(|input| input.expect_ident_matching("list-item")).is_ok() {
        list_item = true;
        continue;
      }

      if outside.is_none() {
        if let Ok(o) = input.try_parse(DisplayOutside::parse) {
          outside = Some(o);
          continue;
        }
      }

      if inside.is_none() {
        if let Ok(i) = input.try_parse(DisplayInside::parse) {
          inside = Some(i);
          continue;
        }
      }

      break;
    }

    if list_item || inside.is_some() || outside.is_some() {
      let inside = inside.unwrap_or(DisplayInside::Flow);
      let outside = outside.unwrap_or(match inside {
        // "If <display-outside> is omitted, the element’s outside display type
        // defaults to block — except for ruby, which defaults to inline."
        // https://drafts.csswg.org/css-display/#inside-model
        DisplayInside::Ruby => DisplayOutside::Inline,
        _ => DisplayOutside::Block,
      });

      if list_item && !matches!(inside, DisplayInside::Flow | DisplayInside::FlowRoot) {
        return Err(input.new_custom_error(ParserError::InvalidDeclaration));
      }

      return Ok(DisplayPair {
        outside,
        inside,
        is_list_item: list_item,
      });
    }

    let location = input.current_source_location();
    let ident = input.expect_ident()?;
    match_ignore_ascii_case! { &*ident,
      "inline-block" => Ok(DisplayPair {
        outside: DisplayOutside::Inline,
        inside: DisplayInside::FlowRoot,
        is_list_item: false
      }),
      "inline-table" => Ok(DisplayPair {
        outside: DisplayOutside::Inline,
        inside: DisplayInside::Table,
        is_list_item: false
      }),
      "inline-flex" => Ok(DisplayPair {
        outside: DisplayOutside::Inline,
        inside: DisplayInside::Flex(VendorPrefix::None),
        is_list_item: false
      }),
      "-webkit-inline-flex" => Ok(DisplayPair {
        outside: DisplayOutside::Inline,
        inside: DisplayInside::Flex(VendorPrefix::WebKit),
        is_list_item: false
      }),
      "-ms-inline-flexbox" => Ok(DisplayPair {
        outside: DisplayOutside::Inline,
        inside: DisplayInside::Flex(VendorPrefix::Ms),
        is_list_item: false
      }),
      "-webkit-inline-box" => Ok(DisplayPair {
        outside: DisplayOutside::Inline,
        inside: DisplayInside::Box(VendorPrefix::WebKit),
        is_list_item: false
      }),
      "-moz-inline-box" => Ok(DisplayPair {
        outside: DisplayOutside::Inline,
        inside: DisplayInside::Box(VendorPrefix::Moz),
        is_list_item: false
      }),
      "inline-grid" => Ok(DisplayPair {
        outside: DisplayOutside::Inline,
        inside: DisplayInside::Grid,
        is_list_item: false
      }),
      _ => Err(location.new_unexpected_token_error(
        cssparser::Token::Ident(ident.clone())
      ))
    }
  }
}

impl ToCss for DisplayPair {
  fn to_css<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      DisplayPair {
        outside: DisplayOutside::Inline,
        inside: DisplayInside::FlowRoot,
        is_list_item: false,
      } => dest.write_str("inline-block"),
      DisplayPair {
        outside: DisplayOutside::Inline,
        inside: DisplayInside::Table,
        is_list_item: false,
      } => dest.write_str("inline-table"),
      DisplayPair {
        outside: DisplayOutside::Inline,
        inside: DisplayInside::Flex(prefix),
        is_list_item: false,
      } => {
        prefix.to_css(dest)?;
        if *prefix == VendorPrefix::Ms {
          dest.write_str("inline-flexbox")
        } else {
          dest.write_str("inline-flex")
        }
      }
      DisplayPair {
        outside: DisplayOutside::Inline,
        inside: DisplayInside::Box(prefix),
        is_list_item: false,
      } => {
        prefix.to_css(dest)?;
        dest.write_str("inline-box")
      }
      DisplayPair {
        outside: DisplayOutside::Inline,
        inside: DisplayInside::Grid,
        is_list_item: false,
      } => dest.write_str("inline-grid"),
      DisplayPair {
        outside,
        inside,
        is_list_item,
      } => {
        let default_outside = match inside {
          DisplayInside::Ruby => DisplayOutside::Inline,
          _ => DisplayOutside::Block,
        };

        let mut needs_space = false;
        if *outside != default_outside || (*inside == DisplayInside::Flow && !*is_list_item) {
          outside.to_css(dest)?;
          needs_space = true;
        }

        if *inside != DisplayInside::Flow {
          if needs_space {
            dest.write_char(' ')?;
          }
          inside.to_css(dest)?;
          needs_space = true;
        }

        if *is_list_item {
          if needs_space {
            dest.write_char(' ')?;
          }
          dest.write_str("list-item")?;
        }

        Ok(())
      }
    }
  }
}

enum_property! {
  /// A `display` keyword.
  ///
  /// See [Display](Display).
  #[allow(missing_docs)]
  pub enum DisplayKeyword {
    None,
    Contents,
    TableRowGroup,
    TableHeaderGroup,
    TableFooterGroup,
    TableRow,
    TableCell,
    TableColumnGroup,
    TableColumn,
    TableCaption,
    RubyBase,
    RubyText,
    RubyBaseContainer,
    RubyTextContainer,
  }
}

/// A value for the [display](https://drafts.csswg.org/css-display-3/#the-display-properties) property.
#[derive(Debug, Clone, PartialEq, Parse, ToCss)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum Display {
  /// A display keyword.
  #[cfg_attr(
    feature = "serde",
    serde(with = "crate::serialization::ValueWrapper::<DisplayKeyword>")
  )]
  Keyword(DisplayKeyword),
  /// The inside and outside display values.
  Pair(DisplayPair),
}

enum_property! {
  /// A value for the [visibility](https://drafts.csswg.org/css-display-3/#visibility) property.
  pub enum Visibility {
    /// The element is visible.
    Visible,
    /// The element is hidden.
    Hidden,
    /// The element is collapsed.
    Collapse,
  }
}

#[derive(Default)]
pub(crate) struct DisplayHandler<'i> {
  decls: Vec<Property<'i>>,
  display: Option<Display>,
}

impl<'i> PropertyHandler<'i> for DisplayHandler<'i> {
  fn handle_property(
    &mut self,
    property: &Property<'i>,
    dest: &mut DeclarationList<'i>,
    context: &mut PropertyHandlerContext<'i, '_>,
  ) -> bool {
    if let Property::Display(display) = property {
      match (&self.display, display) {
        (Some(Display::Pair(cur)), Display::Pair(new)) => {
          // If the new value is different but equivalent (e.g. different vendor prefix),
          // we need to preserve multiple values.
          if cur.outside == new.outside
            && cur.is_list_item == new.is_list_item
            && cur.inside != new.inside
            && cur.inside.is_equivalent(&new.inside)
          {
            // If we have targets, and there is no vendor prefix, clear the existing
            // declarations. The prefixes will be filled in later. Otherwise, if there
            // are no targets, or there is a vendor prefix, add a new declaration.
            if context.targets.browsers.is_some() && new.inside == DisplayInside::Flex(VendorPrefix::None) {
              self.decls.clear();
            } else if context.targets.browsers.is_none() || cur.inside != DisplayInside::Flex(VendorPrefix::None) {
              self.decls.push(Property::Display(self.display.clone().unwrap()));
            }
          }
        }
        _ => {}
      }

      self.display = Some(display.clone());
      return true;
    }

    if matches!(
      property,
      Property::Unparsed(UnparsedProperty {
        property_id: PropertyId::Display,
        ..
      })
    ) {
      self.finalize(dest, context);
      dest.push(property.clone());
      return true;
    }

    false
  }

  fn finalize(&mut self, dest: &mut DeclarationList<'i>, context: &mut PropertyHandlerContext<'i, '_>) {
    if self.display.is_none() {
      return;
    }

    dest.extend(self.decls.drain(..));

    if let Some(display) = std::mem::take(&mut self.display) {
      // If we have an unprefixed `flex` value, then add the necessary prefixed values.
      if let Display::Pair(DisplayPair {
        inside: DisplayInside::Flex(VendorPrefix::None),
        outside,
        ..
      }) = display
      {
        let prefixes = context.targets.prefixes(VendorPrefix::None, Feature::DisplayFlex);

        if let Some(targets) = context.targets.browsers {
          // Handle legacy -webkit-box/-moz-box values if needed.
          if is_flex_2009(targets) {
            if prefixes.contains(VendorPrefix::WebKit) {
              dest.push(Property::Display(Display::Pair(DisplayPair {
                inside: DisplayInside::Box(VendorPrefix::WebKit),
                outside: outside.clone(),
                is_list_item: false,
              })));
            }

            if prefixes.contains(VendorPrefix::Moz) {
              dest.push(Property::Display(Display::Pair(DisplayPair {
                inside: DisplayInside::Box(VendorPrefix::Moz),
                outside: outside.clone(),
                is_list_item: false,
              })));
            }
          }
        }

        if prefixes.contains(VendorPrefix::WebKit) {
          dest.push(Property::Display(Display::Pair(DisplayPair {
            inside: DisplayInside::Flex(VendorPrefix::WebKit),
            outside: outside.clone(),
            is_list_item: false,
          })));
        }

        if prefixes.contains(VendorPrefix::Ms) {
          dest.push(Property::Display(Display::Pair(DisplayPair {
            inside: DisplayInside::Flex(VendorPrefix::Ms),
            outside: outside.clone(),
            is_list_item: false,
          })));
        }
      }

      dest.push(Property::Display(display))
    }
  }
}
