#include <stdio.h>
#include <string.h>
#include "parcel_css.h"

int main()
{
  char *source =
      ".foo {"
      "  color: yellow;"
      "}";
  StyleSheet *stylesheet = stylesheet_parse(source, strlen(source));
  RawString result = stylesheet_to_css(stylesheet);
  stylesheet_free(stylesheet);
  fwrite(result.text, sizeof(char), result.len, stdout);
  printf("\n");
  raw_string_free(result);
}
