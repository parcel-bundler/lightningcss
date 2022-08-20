#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct CssError CssError;

/**
 * A CSS style sheet, representing a `.css` file or inline `<style>` element.
 *
 * Style sheets can be parsed from a string, constructed from scratch,
 * or created using a [Bundler](super::bundler::Bundler). Then, they can be
 * minified and transformed for a set of target browsers, and serialied to a string.
 *
 * # Example
 *
 * ```
 * use parcel_css::stylesheet::{
 *   StyleSheet, ParserOptions, MinifyOptions, PrinterOptions
 * };
 *
 * // Parse a style sheet from a string.
 * let mut stylesheet = StyleSheet::parse(
 *   r#"
 *   .foo {
 *     color: red;
 *   }
 *
 *   .bar {
 *     color: red;
 *   }
 *   "#,
 *   ParserOptions::default()
 * ).unwrap();
 *
 * // Minify the stylesheet.
 * stylesheet.minify(MinifyOptions::default()).unwrap();
 *
 * // Serialize it to a string.
 * let res = stylesheet.to_css(PrinterOptions::default()).unwrap();
 * assert_eq!(res.code, ".foo, .bar {\n  color: red;\n}\n");
 * ```
 */
typedef struct StyleSheet StyleSheet;

typedef struct RawString {
  char *text;
  uintptr_t len;
} RawString;

struct StyleSheet *stylesheet_parse(const char *source, uintptr_t len, struct CssError **error);

bool stylesheet_transform(struct StyleSheet *stylesheet, struct CssError **error);

struct RawString stylesheet_to_css(struct StyleSheet *stylesheet, struct CssError **error);

void stylesheet_free(struct StyleSheet *stylesheet);

void raw_string_free(struct RawString s);

struct RawString error_message(struct CssError *error);

void error_free(struct CssError *error);
