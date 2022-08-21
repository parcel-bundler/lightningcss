#include <stdio.h>
#include <string.h>
#include "parcel_css.h"

int print_error(CssError *error);

int main()
{
  char *source =
      ".foo {"
      "  color: lch(50.998% 135.363 338);"
      "}"
      ".bar {"
      "  color: yellow;"
      "}"
      ".baz:hover {"
      "  color: red;"
      "}";

  ParseOptions parse_opts = {
      .filename = "test.css",
      .css_modules = true,
      .css_modules_pattern = "yo_[name]_[local]"};

  CssError *error = NULL;
  StyleSheet *stylesheet = stylesheet_parse(source, strlen(source), parse_opts, &error);
  if (!stylesheet)
    return print_error(error);

  char *unused_symbols[1] = {"bar"};
  TransformOptions transform_opts = {
      .unused_symbols = unused_symbols,
      .unused_symbols_len = 1};

  if (!browserslist_to_targets("last 2 versions, not IE <= 11", &transform_opts.targets, &error))
    return print_error(error);

  if (!stylesheet_transform(stylesheet, transform_opts, &error))
    return print_error(error);

  ToCssOptions to_css_opts = {
      .minify = true,
      .source_map = true,
      .pseudo_classes = {
          .hover = "is-hovered"}};

  ToCssResult result = stylesheet_to_css(stylesheet, to_css_opts, &error);
  if (error)
    return print_error(error);

  stylesheet_free(stylesheet);
  fwrite(result.code.text, sizeof(char), result.code.len, stdout);
  printf("\n");
  fwrite(result.map.text, sizeof(char), result.map.len, stdout);
  printf("\n");
  to_css_result_free(result);
}

int print_error(CssError *error)
{
  printf("error: %s\n", error_message(error));
  error_free(error);
  return 1;
}
