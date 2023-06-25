//! Common [CSS values](https://www.w3.org/TR/css3-values/) used across many properties.
//!
//! Each value provides parsing and serialization support using the [Parse](super::traits::Parse)
//! and [ToCss](super::traits::ToCss) traits. In addition, many values support ways of manipulating
//! them, including converting between representations and units, generating fallbacks for legacy
//! browsers, minifying them, etc.
//!
//! # Example
//!
//! This example shows how you could parse a CSS color value, convert it to RGB, and re-serialize it.
//! Similar patterns for parsing and serializing are possible across all value types.
//!
//! ```
//! use lightningcss::{
//!   traits::{Parse, ToCss},
//!   values::color::CssColor,
//!   printer::PrinterOptions
//! };
//!
//! let color = CssColor::parse_string("lch(50% 75 0)").unwrap();
//! let rgb = color.to_rgb().unwrap();
//! assert_eq!(rgb.to_css_string(PrinterOptions::default()).unwrap(), "#e1157b");
//! ```
//!
//! If you have a [cssparser::Parser](cssparser::Parser) already, you can also use the `parse` and `to_css`
//! methods instead, rather than parsing from a string.

#![deny(missing_docs)]

pub mod alpha;
pub mod angle;
pub mod calc;
pub mod color;
pub mod easing;
pub mod gradient;
pub mod ident;
pub mod image;
pub mod length;
pub mod number;
pub mod percentage;
pub mod position;
pub mod ratio;
pub mod rect;
pub mod resolution;
pub mod shape;
pub mod size;
pub mod string;
pub mod syntax;
pub mod time;
pub mod url;
